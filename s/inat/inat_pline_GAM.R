suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(lubridate)
  library(MASS)
  library(ggplot2)
  library(scales)
  library(grid)
})

#--------------------------------------------------
# 1. PREP DATA
#--------------------------------------------------
rate_linear_per_year <- function(fit, stable_thresh = 5) {
  cf <- coef(fit)
  vn <- names(cf)
  
  time_term <- c("year_cont_num", "year_std", "year_observed")
  time_term <- time_term[time_term %in% vn][1]
  
  if (is.na(time_term)) {
    stop("Could not find a linear year term in the model.")
  }
  
  beta <- cf[[time_term]]
  V    <- vcov(fit)[time_term, time_term]
  se   <- sqrt(V)
  
  change_per_year <- exp(beta) - 1
  lo              <- exp(beta - 1.96 * se) - 1
  hi              <- exp(beta + 1.96 * se) - 1
  
  pct <- 100 * change_per_year
  ci  <- 100 * c(lo, hi)
  
  direction <- dplyr::case_when(
    pct >  stable_thresh ~ "growing",
    pct < -stable_thresh ~ "declining",
    TRUE                 ~ "stable"
  )
  
  symbol <- dplyr::case_when(
    direction == "growing"   ~ "\u2191",   # ↑
    direction == "declining" ~ "\u2193",   # ↓
    TRUE                     ~ "\u2248"    # ≈
  )
  
  symbol_color <- dplyr::case_when(
    direction == "growing"   ~ "#2E8B57",
    direction == "declining" ~ "#C0392B",
    TRUE                     ~ "black"
  )
  
  list(
    term         = time_term,
    pct_per_year = pct,
    ci95         = ci,
    direction    = direction,
    symbol       = symbol,
    symbol_color = symbol_color
  )
}

make_trend_annotation <- function(fit, x_range, y_ceiling, stable_thresh = 5, x_pos = 0.90) {
  roc <- rate_linear_per_year(fit, stable_thresh = stable_thresh)
  
  label_txt <- sprintf("%.1f%%/yr", roc$pct_per_year)
  
  tibble::tibble(
    x      = min(x_range) + x_pos * diff(range(x_range)),
    y_sym  = y_ceiling * 0.95,
    y_lab  = y_ceiling * 0.79,
    symbol = roc$symbol,
    color  = roc$symbol_color,
    label  = label_txt,
    dir    = roc$direction
  )
}

prep_model_data <- function(df, effort_col = "total_observations") {
  
  df <- df %>%
    mutate(
      shark_observations = replace_na(shark_observations, 0),
      effort_offset      = pmax(.data[[effort_col]], 1),
      total_users = total_users,
      year_observed = as.numeric(year_observed),
      month_num     = as.integer(month_factor),
      
      # harmonic seasonality
      month_sin = sin(2*pi*month_num/12),
      month_cos = cos(2*pi*month_num/12),
      
      # continuous time (standardized)
      year_std = year_observed - mean(year_observed, na.rm = TRUE),
      
      year_factor = factor(year_observed)
    )
  
  return(df)
}

get_valid_year_sequence <- function(
    combined_date,
    max_zero_gap = 2,
    max_year = 2025
) {
  
  library(dplyr)
  library(tidyr)
  
  #-----------------------------------
  # 1. Build yearly presence table
  #-----------------------------------
  
  year_df <- combined_date %>%
    filter(!is.na(year_observed)) %>%
    filter(year_observed <= max_year) %>%
    group_by(year_observed) %>%
    summarise(n = sum(shark_observations), .groups = "drop") %>%
    complete(
      year_observed = seq(min(year_observed), max(year_observed)),
      fill = list(n = 0)
    ) %>%
    arrange(year_observed) %>%
    mutate(
      present = as.integer(n > 0)
    )
  
  #-----------------------------------
  # 2. Identify runs (0 vs 1)
  #-----------------------------------
  
  year_df <- year_df %>%
    mutate(
      run_id = cumsum(c(1, diff(present) != 0))
    )
  
  runs <- year_df %>%
    group_by(run_id) %>%
    summarise(
      start_year = min(year_observed),
      end_year   = max(year_observed),
      present    = first(present),
      length     = n(),
      .groups = "drop"
    )
  
  #-----------------------------------
  # 3. Apply gap rule
  #-----------------------------------
  
  runs <- runs %>%
    mutate(
      keep = case_when(
        present == 1 ~ TRUE,
        
        present == 0 &
          length <= max_zero_gap &
          lag(present, default = 0) == 1 &
          lead(present, default = 0) == 1 ~ TRUE,
        
        TRUE ~ FALSE
      )
    )
  
  #-----------------------------------
  # 4. Map back to years
  #-----------------------------------
  
  year_df <- year_df %>%
    left_join(runs %>% dplyr::select(run_id, keep), by = "run_id") %>%
    mutate(keep = replace_na(keep, FALSE))
  
  valid_years <- year_df %>%
    filter(keep) %>%
    pull(year_observed)
  
  #-----------------------------------
  # 5. Extract longest continuous sequence
  #-----------------------------------
  
  runs_final <- split(valid_years, cumsum(c(1, diff(valid_years) != 1)))
  
  yrs <- runs_final[[which.max(lengths(runs_final))]]
  
  return(yrs)
}
#--------------------------------------------------
# 2. FIT MODELS
#--------------------------------------------------

fit_gam_models <- function(df) {
  
  m_curve <- mgcv::gam(
    shark_observations ~ year_std + month_sin + month_cos +
      offset(log(effort_offset)),
    family = mgcv::nb(),
    method = "REML",
    data = df
  )
  
  m_points <- mgcv::gam(
    shark_observations ~ year_factor + month_sin + month_cos +
      offset(log(effort_offset)),
    family = mgcv::nb(),
    method = "REML",
    data = df
  )
  
  list(curve = m_curve, points = m_points)
}

#--------------------------------------------------
# 3. PREDICTIONS
#--------------------------------------------------

predict_glm_trends <- function(df, models,
                               effort_fixed = 1000,
                               average_months = TRUE,
                               ci_level = 0.95) {
  
  z <- qnorm(0.5 + ci_level/2)
  
  years <- sort(unique(df$year_observed))
  
  # helper: seasonal grid
  make_month_grid <- function() {
    tibble(
      month_num = 1:12,
      month_sin = sin(2*pi*month_num/12),
      month_cos = cos(2*pi*month_num/12)
    )
  }
  
  #-----------------------------------
  # MODEL 1: continuous trend
  #-----------------------------------
  
  nd_curve <- expand_grid(
    year_observed = years,
    make_month_grid()
  ) %>%
    mutate(
      year_std = year_observed - mean(df$year_observed),
      effort_offset = effort_fixed
    )
  
  pr_curve <- predict(models$curve, nd_curve,
                      type = "link", se.fit = TRUE)
  
  nd_curve <- nd_curve %>%
    mutate(
      fit    = pr_curve$fit,
      se.fit = pr_curve$se.fit
    ) %>%
    group_by(year_observed) %>%
    summarise(
      fit    = mean(fit),
      se.fit = sqrt(mean(se.fit^2)),
      .groups = "drop"
    )
  
  #-----------------------------------
  # MODEL 2: yearly estimates
  #-----------------------------------
  
  nd_points <- expand_grid(
    year_observed = years,
    make_month_grid()
  ) %>%
    mutate(
      year_factor = factor(year_observed, levels = levels(df$year_factor)),
      effort_offset = effort_fixed
    )
  
  pr_points <- predict(models$points, nd_points,
                       type = "link", se.fit = TRUE)
  
  nd_points <- nd_points %>%
    mutate(
      fit    = pr_points$fit,
      se.fit = pr_points$se.fit
    ) %>%
    group_by(year_observed) %>%
    summarise(
      fit    = mean(fit),
      se.fit = sqrt(mean(se.fit^2)),
      .groups = "drop"
    )
  
  #-----------------------------------
  # 🔥 CRITICAL: SPUE CALCULATION
  #-----------------------------------
  
  link_to_spue <- function(fit, se) {
    
    lo <- fit - z * se
    hi <- fit + z * se
    
    mu    <- exp(fit)
    mu_lo <- exp(lo)
    mu_hi <- exp(hi)
    
    # EXACTLY matches your methods
    spue    <- mu    / (effort_fixed / 1000)
    spue_lo <- mu_lo / (effort_fixed / 1000)
    spue_hi <- mu_hi / (effort_fixed / 1000)
    
    tibble(
      spue_hat = spue,
      lo_spue  = spue_lo,
      hi_spue  = spue_hi
    )
  }
  
  pred_curve <- bind_cols(
    nd_curve["year_observed"],
    link_to_spue(nd_curve$fit, nd_curve$se.fit)
  )
  
  pred_points <- bind_cols(
    nd_points["year_observed"],
    link_to_spue(nd_points$fit, nd_points$se.fit)
  )
  
  list(curve = pred_curve, points = pred_points)
}

#--------------------------------------------------
# 4. PLOTTING
#--------------------------------------------------

plot_trends <- function(pred_curve, pred_points, clip_ci_q = 0.975, roc_pos = 0.9, model) {
  
  # keep only finite upper CI values
  hi_all <- c(pred_curve$hi_spue, pred_points$hi_spue)
  hi_all <- hi_all[is.finite(hi_all)]
  
  y_ceiling <- unname(quantile(hi_all, probs = clip_ci_q, na.rm = TRUE))
  
  if (!is.finite(y_ceiling) || y_ceiling <= 0) {
    y_ceiling <- max(c(pred_curve$spue_hat, pred_points$spue_hat), na.rm = TRUE) * 1.1
  }
  
  pred_curve_plot <- pred_curve %>%
    mutate(
      spue_plot = pmin(spue_hat, y_ceiling),
      lo_plot   = pmax(lo_spue, 0),
      hi_plot   = pmin(hi_spue, y_ceiling),
      off_top   = hi_spue > y_ceiling + 1e-9
    )
  
  pred_points_plot <- pred_points %>%
    mutate(
      spue_plot = pmin(spue_hat, y_ceiling),
      lo_plot   = pmax(lo_spue, 0),
      hi_plot   = pmin(hi_spue, y_ceiling),
      off_top   = hi_spue > y_ceiling + 1e-9
    )
  
  y_breaks <- scales::breaks_extended(n = 5)(c(0, y_ceiling))
  y_breaks <- y_breaks[y_breaks >= 0 & y_breaks <= y_ceiling]
  
  trend_anno <- make_trend_annotation(
    fit = model$curve,
    x_range = pred_curve$year_observed,
    y_ceiling = y_ceiling,
    stable_thresh = 5,
    x_pos = roc_pos
  )
  
  min_year <- min(pred_curve_plot$year_observed, na.rm = TRUE)
  max_year <- max(pred_curve_plot$year_observed, na.rm = TRUE)
  
  step <- ceiling((max_year - min_year) / 4)  # max ~5 ticks
  
  ggplot(pred_curve_plot, aes(x = year_observed, y = spue_plot)) +
    
    geom_ribbon(
      aes(ymin = lo_plot, ymax = hi_plot),
      fill = "#74AC00", alpha = 0.2
    ) +
    
    geom_line(color = "#74AC00", linewidth = 1.2) +
    
    geom_point(
      data = pred_points_plot,
      aes(y = spue_plot),
      size = 2
    ) +
    
    geom_errorbar(
      data = pred_points_plot,
      aes(ymin = lo_plot, ymax = hi_plot),
      width = 0.2
    ) +
    
    geom_segment(
      data = dplyr::filter(pred_points_plot, off_top),
      aes(
        x = year_observed, xend = year_observed,
        y = y_ceiling * 0.97, yend = y_ceiling
      ),
      linewidth = 0.5,
      arrow = grid::arrow(length = grid::unit(5, "pt"), type = "closed")
    ) +
    
    geom_text(
      data = trend_anno,
      aes(x = x, y = y_sym, label = symbol),
      inherit.aes = FALSE,
      color = trend_anno$color,
      size = 15,
      fontface = "bold"
    ) +
    geom_label(
      data = trend_anno,
      aes(x = x, y = y_lab, label = label),
      inherit.aes = FALSE,
      size = 3.5,
      label.size = 0.2,
      fill = "white"
    ) +
    
    scale_y_continuous(
      limits = c(0, y_ceiling),
      breaks = y_breaks,
      labels = label_number(big.mark = ","),
      oob = scales::squish
    ) +
    scale_x_continuous(
      breaks = seq(min_year, max_year, by = step)
    ) +
    labs(
      x = "Year",
      y = "Sightings per 1,000 posts",
      title = ""
    ) +
    
    theme_classic(base_size = 14)
}

#--------------------------------------------------
# 5. WRAPPER FUNCTION
#--------------------------------------------------

run_model_pipeline <- function(combined_date, clip_ci_q = 0.975, sym_pos = 0.9) {
  
  df <- prep_model_data(combined_date)
  
  models <- fit_gam_models(df)
  
  preds <- predict_glm_trends(df, models, ci_level = 0.95)
  
  p <- plot_trends(preds$curve, preds$points, 
                   clip_ci_q = clip_ci_q, model = models,
                   roc_pos = sym_pos)
  
  list(
    data   = df,
    models = models,
    preds  = preds,
    plot   = p
  )
}

################################################################################
# yrs <- get_valid_year_sequence(combined_date) # allows max 2 consecutive zero-obs years

combined_date <- combined_date %>%
  group_by(year_observed) %>%
  filter(sum(shark_observations) >= 1) %>%
  ungroup()

years <- as.numeric(format(combined_date$month, "%Y"))

valid <- combined_date$shark_observations > 0 & combined_date$year_observed < 2026

min_year <- min(years[valid], na.rm = TRUE)
max_year <- max(years[valid], na.rm = TRUE)

yrs <- seq(2018, max_year)
yrs

m1 <- run_model_pipeline(clip_ci_q = 0.975, sym_pos = 0.9,
  combined_date %>%
    filter(!is.na(year_observed)) %>%
    filter(year_observed %in% yrs)
)

m1$plot
summary(m1$models$curve); summary(m1$models$points)

df <- m1$preds$curve %>% arrange(year_observed)

delta_spue <- df$spue_hat[nrow(df)] - df$spue_hat[1]
delta_spue

print(m1$preds$curve, n = Inf)