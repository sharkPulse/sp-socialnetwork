suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(lubridate)
  library(mgcv)
  library(ggplot2)
  library(grid)      # arrow()
  library(scales)
})

# --- helpers -------------------------------------------------------------
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

is_nb_family <- function(fit) {
  isTRUE(grepl("Negative Binomial", fit$family$family))
}

get_theta_any <- function(fit) {
  tryCatch({
    if (!is.null(fit$family$getTheta)) {
      th <- suppressWarnings(fit$family$getTheta(fit))
      if (is.finite(th)) return(th)
    }
    if (!is.null(fit$family$theta) && is.finite(fit$family$theta)) return(fit$family$theta)
    famtxt <- fit$family$family
    if (grepl("Negative Binomial\\(", famtxt)) {
      num <- suppressWarnings(as.numeric(sub(".*\\((.*)\\).*", "\\1", famtxt)))
      if (is.finite(num)) return(num)
    }
    NA_real_
  }, error = function(e) NA_real_)
}

add_time_cols <- function(df) {
  # ensures year_cont and numeric version exist
  if (!("year_cont" %in% names(df))) {
    stopifnot(all(c("year_observed","month") %in% names(df)))
    df$year_cont <- as.Date(sprintf("%d-%02d-15", df$year_observed, lubridate::month(df$month)))
  }
  df$year_cont_num <- lubridate::decimal_date(df$year_cont)
  df
}

# Link-scale -> SPUE per `effort_fixed` units; NB aware
link_to_spue <- function(fit_obj, link_fit, se_link, effort_fixed, ci = 0.95) {
  z  <- qnorm(0.5 + ci/2)
  lo <- link_fit - z * se_link
  hi <- link_fit + z * se_link
  mu <- exp(link_fit); mu_lo <- exp(lo); mu_hi <- exp(hi)
  lam    <- mu    / effort_fixed
  lam_lo <- mu_lo / effort_fixed
  lam_hi <- mu_hi / effort_fixed
  
  if (is_nb_family(fit_obj)) {
    th <- get_theta_any(fit_obj)
    if (is.finite(th)) {
      p1    <- 1 - (1 + lam    / th)^(-th)
      p1_lo <- 1 - (1 + lam_lo / th)^(-th)
      p1_hi <- 1 - (1 + lam_hi / th)^(-th)
    } else {
      p1    <- 1 - exp(-lam); p1_lo <- 1 - exp(-lam_lo); p1_hi <- 1 - exp(-lam_hi)
    }
  } else {
    p1    <- 1 - exp(-lam); p1_lo <- 1 - exp(-lam_lo); p1_hi <- 1 - exp(-lam_hi)
  }
  
  tibble::tibble(
    spue_hat = effort_fixed * p1,
    lo_spue  = effort_fixed * p1_lo,
    hi_spue  = effort_fixed * p1_hi
  )
}

extract_edf_time <- function(fit) {
  st <- tryCatch(summary(fit)$s.table, error = function(e) NULL)
  if (is.null(st)) return(NA_real_)
  rn <- rownames(st)
  hit <- grep("^s\\(", rn)
  if (length(hit) == 0) return(NA_real_)
  as.numeric(st[hit[1], "edf"])
}

.make_year_breaks <- function(years, n_target = 6) {
  yrs <- sort(unique(as.integer(years)))
  rng <- range(yrs, na.rm = TRUE)
  if (!all(is.finite(rng))) return(yrs)
  span <- diff(rng)
  if (span <= 0) return(rng[1])
  step <- max(1L, round(span / max(1L, n_target - 1L)))
  br <- seq(from = rng[1], to = rng[2], by = step)
  if (length(br) == 0L) br <- rng[1]
  if (tail(br, 1) != rng[2]) br <- c(br, rng[2])
  unique(as.integer(br))
}

# --- core ---------------------------------------------------------------

#' Fit shark trend model on combined_date
#'
#' @param dat combined_date tibble with columns:
#'   month, year_observed, shark_observations, total_observations, total_users
#' @param effort_offset_col "total_observations" or "total_users"
#' @param ensure_offset_ge_obs logical; if TRUE, offset >= shark_observations
#' @param start_when_first_obs logical; if TRUE, start at first year with shark_observations > 0
#' @param years_keep integer vector of years to keep (optional). Applied after start rule.
#' @param drop_years integer vector of years to drop (e.g., outliers)
#' @param min_year,max_year optional numeric bounds for modeling window
#' @param weight_strategy "none","sqrt_effort","user_ratio","hybrid"
#' @param season_type "harmonic","factor","factor_spline","none"
#' @param trend_type "spline" or "linear"
#' @param trend_scale "calendar" (decimal year) or "std" (centered)
#' @param k_year spline basis size if trend_type = "spline"
#' @param average_months logical; TRUE averages months; FALSE uses fixed_month
#' @param fixed_month month name or number if average_months = FALSE
#' @param effort_fixed denominator used to express SPUE (e.g., per 1000 posts/users)
#' @param ci_level CI level for ribbons/intervals
#' @param clip_ci_q quantile to clip y-axis
#' @param x_ticks_n target number of x-axis ticks
#'
#' @return list(data, fit_curve, fit_points, pred_ts, pred_pts, plot, metrics, y_ceiling)
fit_shark_trend2 <- function(
    dat,
    effort_offset_col   = c("total_observations","total_users"),
    ensure_offset_ge_obs= TRUE,
    start_when_first_obs= TRUE,
    years_keep          = NULL,
    drop_years          = NULL,
    min_year            = NULL,
    max_year            = NULL,
    weight_strategy     = c("none","sqrt_effort","user_ratio","hybrid"),
    season_type         = c("harmonic","factor","factor_spline","none"),
    trend_type          = c("spline","linear"),
    trend_scale         = c("calendar","std"),
    k_year              = 10,
    average_months      = TRUE,
    fixed_month         = "Jun",
    effort_fixed        = 1000,
    ci_level            = 0.95,
    clip_ci_q           = 0.95,
    x_ticks_n           = 6,
    roc_pos = 0.90
) {
  effort_offset_col <- match.arg(effort_offset_col)
  weight_strategy   <- match.arg(weight_strategy)
  season_type       <- match.arg(season_type)
  trend_type        <- match.arg(trend_type)
  trend_scale       <- match.arg(trend_scale)
  
  req <- c("month","year_observed","shark_observations","total_observations","total_users")
  if (any(!req %in% names(dat))) stop("`dat` is missing required columns: ", paste(setdiff(req, names(dat)), collapse=", "))
  
  # base df
  df <- dat %>%
    mutate(
      shark_observations = as.numeric(replace_na(shark_observations, 0)),
      eff_raw            = pmax(.data[[effort_offset_col]], 1L)
    )
  
  # optionally enforce offset >= observed (and >= 1), then log-offset safe
  if (isTRUE(ensure_offset_ge_obs)) {
    df <- df %>% mutate(effort_offset = pmax(eff_raw, shark_observations, 1))
  } else {
    df <- df %>% mutate(effort_offset = eff_raw)
  }
  
  # windowing rules ------------------------------------------------------
  if (!is.null(min_year)) df <- df %>% filter(year_observed >= min_year)
  if (!is.null(max_year)) df <- df %>% filter(year_observed <= max_year)
  
  if (isTRUE(start_when_first_obs)) {
    first_obs_year <- suppressWarnings(min(df$year_observed[df$shark_observations > 0], na.rm = TRUE))
    if (is.finite(first_obs_year)) df <- df %>% filter(year_observed >= first_obs_year)
  }
  
  if (!is.null(years_keep)) {
    df <- df %>% filter(year_observed %in% years_keep)
  }
  if (!is.null(drop_years)) {
    df <- df %>% filter(!(year_observed %in% drop_years))
  }
  
  # must have data
  if (!nrow(df)) stop("No rows left after filtering—check your year filters.")
  
  # season features
  df <- df %>%
    mutate(
      month_factor = factor(lubridate::month(.data$month), levels = 1:12, labels = month.abb),
      month_num    = as.integer(month_factor),
      month_sin    = sin(2*pi*month_num/12),
      month_cos    = cos(2*pi*month_num/12)
    )
  
  # time numeric
  df <- add_time_cols(df)
  
  # year_std computed AFTER all windowing (only if needed)
  if (trend_scale == "std") {
    yc <- mean(df$year_cont_num, na.rm = TRUE)
    df$year_std <- df$year_cont_num - yc
    trend_var   <- "year_std"
  } else {
    trend_var   <- "year_cont_num"
  }
  
  # weights --------------------------------------------------------------
  if (weight_strategy == "sqrt_effort") {
    w <- sqrt(df$effort_offset)
    w <- pmin(w, stats::quantile(w, 0.95, na.rm = TRUE))
    df$w <- w / max(w, na.rm = TRUE)
  } else if (weight_strategy == "user_ratio") {
    r <- df$total_users / pmax(df$total_observations, 1L)
    r <- pmin(pmax(r, 0), 1); r[r == 0] <- 0.05
    df$w <- r / max(r, na.rm = TRUE)
  } else if (weight_strategy == "hybrid") {
    r <- df$total_users / pmax(df$total_observations, 1L)
    r <- pmin(pmax(r, 0), 1); r[r == 0] <- 0.05
    se <- sqrt(df$effort_offset); se <- se / max(se, na.rm = TRUE)
    w <- r * se
    w <- pmin(w, stats::quantile(w, 0.95, na.rm = TRUE))
    df$w <- w / max(w, na.rm = TRUE)
  } else {
    df$w <- 1
  }
  
  # family
  fam_nb <- mgcv::nb()
  
  # seasonal term
  season_term <- switch(
    season_type,
    harmonic      = "month_sin + month_cos",
    factor        = "month_factor",
    factor_spline = "s(month_factor, bs = 're')",
    none          = NULL
  )
  
  # trend term
  trend_term <- if (trend_type == "spline") paste0("s(", trend_var, ", k=", k_year, ")")
  else trend_var
  
  # formulas (response is shark_observations)
  form_curve  <- as.formula(paste(
    "shark_observations ~", trend_term,
    if (!is.null(season_term)) paste("+", season_term) else "",
    "+ offset(log(effort_offset))"
  ))

  df$year_factor <- factor(df$year_observed)
  
  form_points <- as.formula(paste(
    "shark_observations ~ s(year_factor, bs='re')",
    if (!is.null(season_term)) paste("+", season_term) else "",
    "+ offset(log(effort_offset))"
  ))
  
  # form_points <- as.formula(paste(
  #   "shark_observations ~ year_factor",
  #   if (!is.null(season_term)) paste("+", season_term) else "",
  #   "+ offset(log(effort_offset))"
  # ))
  
  # fits
  
  fit_curve  <- mgcv::gam(form_curve,  family = fam_nb, data = df, method = "REML", weights = df$w)
  fit_points <- mgcv::gam(form_points, family = fam_nb, data = df, method = "REML", weights = df$w)

  # fit_curve <- MASS::glm.nb(
  #   form_curve,
  #   data = df,
  #   weights = df$w
  # )
  # 
  # fit_points <- MASS::glm.nb(
  #   form_points,
  #   data = df,
  #   weights = df$w
  # )

  
  years_pred <- sort(unique(df$year_observed))
  
  fm_to_num <- function(fm) { fm <- if (is.character(fm)) match(fm, month.abb) else as.integer(fm); ifelse(is.na(fm), 6L, fm) }
  make_month_grid <- function() {
    tibble::tibble(
      month_num    = 1:12,
      month_sin    = sin(2*pi*month_num/12),
      month_cos    = cos(2*pi*month_num/12),
      month_factor = factor(month_num, levels = 1:12, labels = month.abb)
    )
  }
  
  # predictions for curve ------------------------------------------------
  if (average_months) {
    nd_curve <- tidyr::expand_grid(year_observed = years_pred) %>%
      dplyr::left_join(make_month_grid() %>% mutate(key = 1), by = character()) %>%
      dplyr::mutate(
        effort_offset = effort_fixed,
        year_cont     = as.Date(sprintf("%d-%02d-15", year_observed, month_num)),
        year_cont_num = lubridate::decimal_date(year_cont)
      )
    if (trend_scale == "std") {
      yc <- mean(df$year_cont_num, na.rm = TRUE)
      nd_curve$year_std <- nd_curve$year_cont_num - yc
    }
    pr <- as.data.frame(predict(fit_curve, nd_curve, type = "link", se.fit = TRUE, unconditional = TRUE))
    tmp <- dplyr::bind_cols(nd_curve, pr) %>%
      dplyr::group_by(year_observed) %>%
      dplyr::summarise(
        fit    = mean(fit, na.rm = TRUE),
        se.fit = sqrt(mean(se.fit^2, na.rm = TRUE)),
        .groups = "drop"
      )
    pred_ts <- dplyr::bind_cols(
      tibble::tibble(year_observed = tmp$year_observed),
      link_to_spue(fit_curve, tmp$fit, tmp$se.fit, effort_fixed, ci_level)
    )
  } else {
    fm <- fm_to_num(fixed_month)
    nd_curve <- tibble::tibble(
      year_observed = years_pred,
      month_num     = fm,
      month_sin     = sin(2*pi*fm/12),
      month_cos     = cos(2*pi*fm/12),
      month_factor  = factor(fm, levels = 1:12, labels = month.abb),
      effort_offset = effort_fixed,
      year_cont     = as.Date(sprintf("%d-%02d-15", years_pred, fm)),
      year_cont_num = lubridate::decimal_date(year_cont)
    )
    if (trend_scale == "std") {
      yc <- mean(df$year_cont_num, na.rm = TRUE)
      nd_curve$year_std <- nd_curve$year_cont_num - yc
    }
    pr <- as.data.frame(predict(fit_curve, nd_curve, type = "link", se.fit = TRUE, unconditional = TRUE))
    pred_ts <- dplyr::bind_cols(
      nd_curve["year_observed"],
      link_to_spue(fit_curve, pr$fit, pr$se.fit, effort_fixed, ci_level)
    )
  }
  
  # predictions for points ----------------------------------------------
  if (average_months) {
    nd_points <- tidyr::expand_grid(year_observed = years_pred) %>%
      dplyr::left_join(make_month_grid() %>% mutate(key = 1), by = character()) %>%
      dplyr::mutate(
        effort_offset = effort_fixed,
        year_cont     = as.Date(sprintf("%d-%02d-15", year_observed, month_num)),
        year_cont_num = lubridate::decimal_date(year_cont),
        year_factor   = factor(year_observed, levels = levels(df$year_factor))
      )
    if (trend_scale == "std") {
      yc <- mean(df$year_cont_num, na.rm = TRUE)
      nd_points$year_std <- nd_points$year_cont_num - yc
    }
    prp <- as.data.frame(predict(fit_points, nd_points, type = "link", se.fit = TRUE, unconditional = TRUE))
    tmp <- dplyr::bind_cols(nd_points, prp) %>%
      dplyr::group_by(year_observed) %>%
      dplyr::summarise(
        fit    = mean(fit, na.rm = TRUE),
        se.fit = sqrt(mean(se.fit^2, na.rm = TRUE)),
        .groups = "drop"
      )
    pred_pts <- dplyr::bind_cols(
      tibble::tibble(year_observed = tmp$year_observed),
      link_to_spue(fit_points, tmp$fit, tmp$se.fit, effort_fixed, ci_level)
    )
  } else {
    fm <- fm_to_num(fixed_month)
    nd_points <- tibble::tibble(
      year_observed = years_pred,
      month_num     = fm,
      month_sin     = sin(2*pi*fm/12),
      month_cos     = cos(2*pi*fm/12),
      month_factor  = factor(fm, levels = 1:12, labels = month.abb),
      effort_offset = effort_fixed,
      year_cont     = as.Date(sprintf("%d-%02d-15", years_pred, fm)),
      year_cont_num = lubridate::decimal_date(year_cont),
      year_factor   = factor(year_observed, levels = levels(df$year_factor))
    )
    if (trend_scale == "std") {
      yc <- mean(df$year_cont_num, na.rm = TRUE)
      nd_points$year_std <- nd_points$year_cont_num - yc
    }
    prp <- as.data.frame(predict(fit_points, nd_points, type = "link", se.fit = TRUE, unconditional = TRUE))
    pred_pts <- dplyr::bind_cols(
      nd_points["year_observed"],
      link_to_spue(fit_points, prp$fit, prp$se.fit, effort_fixed, ci_level)
    )
  }
  
  # plotting -------------------------------------------------------------
  y_ceiling <- stats::quantile(c(pred_ts$hi_spue, pred_pts$hi_spue),
                               probs = clip_ci_q, na.rm = TRUE)
  if (!is.finite(y_ceiling) || y_ceiling <= 0) {
    y_ceiling <- max(c(pred_ts$spue_hat, pred_pts$spue_hat), na.rm = TRUE) * 1.1
  }
  
  pred_ts_plot <- pred_ts %>%
    mutate(lo_plot = pmax(lo_spue, 0), hi_plot = pmin(hi_spue, y_ceiling), off_top = hi_spue > y_ceiling + 1e-9)
  pred_pts_plot <- pred_pts %>%
    mutate(lo_plot = pmax(lo_spue, 0), hi_plot = pmin(hi_spue, y_ceiling), off_top = hi_spue > y_ceiling + 1e-9)
  
  year_breaks <- .make_year_breaks(pred_ts$year_observed, n_target = x_ticks_n)
  if (length(year_breaks) < 2L) {
    rng <- range(pred_ts$year_observed, na.rm = TRUE)
    year_breaks <- unique(as.integer(scales::breaks_pretty(n = max(2, x_ticks_n))(rng)))
  }
  y_unit <- if (effort_offset_col == "total_users") "Users" else "Posts"
  
  trend_anno <- make_trend_annotation(
    fit = fit_curve,
    x_range = pred_ts_plot$year_observed,
    y_ceiling = y_ceiling,
    stable_thresh = 5,
    x_pos = roc_pos
  )
  
  p <- ggplot(pred_ts_plot, aes(x = year_observed, y = spue_hat)) +
    geom_ribbon(aes(ymin = lo_plot, ymax = hi_plot), fill = "#74AC00", alpha = 0.15) +
    geom_line(linewidth = 1, color = "#74AC00") +
    geom_point(data = pred_pts_plot, aes(y = spue_hat), size = 2) +
    geom_errorbar(data = pred_pts_plot, aes(ymin = lo_plot, ymax = hi_plot), width = 0.2) +
    geom_segment(
      data = dplyr::filter(pred_pts_plot, off_top),
      aes(x = year_observed, xend = year_observed, y = y_ceiling * 0.97, yend = y_ceiling),
      linewidth = 0.5, arrow = grid::arrow(length = grid::unit(5, "pt"), type = "closed")
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
    labs(x = "Year", y = sprintf("Sightings per %s %s", format(effort_fixed, big.mark=","), y_unit)) +
    scale_x_continuous(
      breaks = scales::breaks_extended(n = x_ticks_n),
      labels = function(x) as.integer(x),
      guide  = guide_axis(check.overlap = TRUE)
    ) +
    coord_cartesian(ylim = c(0, y_ceiling)) +
    theme_classic(base_size = 14) +
    theme(
      panel.grid = element_blank(),
      axis.line  = element_line(color = "black"),
      axis.ticks = element_line(color = "black")
    )
  
  # metrics --------------------------------------------------------------
  metrics <- tibble::tibble(
    first_obs_year     = suppressWarnings(min(df$year_observed[df$shark_observations > 0], na.rm = TRUE)),
    effort_offset_col  = effort_offset_col,
    weight_strategy    = weight_strategy,
    season_type        = season_type,
    trend_type         = trend_type,
    trend_scale        = trend_scale,
    AIC_curve          = AIC(fit_curve),
    AIC_points         = AIC(fit_points),
    EDF_time           = extract_edf_time(fit_curve),
    theta_curve        = get_theta_any(fit_curve),
    theta_points       = get_theta_any(fit_points)
  )
  
  list(
    data       = df,
    fit_curve  = fit_curve,
    fit_points = fit_points,
    pred_ts    = pred_ts,
    pred_pts   = pred_pts,
    plot       = p,
    metrics    = metrics,
    y_ceiling  = y_ceiling
  )
}

########################################################################
# combined_date = combined_date %>%
#   group_by(year_observed) %>%
#   filter(sum(shark_observations) > 0) %>%
#   ungroup()

min_year <- min(as.numeric(format(combined_date$month[combined_date$shark_observations > 0], "%Y")),
                na.rm = TRUE)
min_year
max_year <- max(as.numeric(format(combined_date$month[combined_date$shark_observations > 0 & combined_date$year_observed < 2026], "%Y")),
                na.rm = TRUE)
max_year
yrs <- seq(min_year, max_year)
yrs
m1 <- fit_shark_trend2(
  combined_date,
  effort_offset_col   = "total_observations",   # or "total_users"
  start_when_first_obs= FALSE,
  # drop_years = c(2025),
  years_keep          = yrs,
  weight_strategy     = "none",
  season_type         = "harmonic",
  trend_type          = "linear",
  trend_scale         = "std",             # or "std"
  k_year              = 12,
  average_months      = TRUE,
  effort_fixed        = 1000,
  clip_ci_q           = 0.975,
  x_ticks_n           = 4
)
print(m1$metrics)
print(m1$plot)
summary(m1$fit_curve); summary(m1$fit_points)
