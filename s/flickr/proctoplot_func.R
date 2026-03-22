# =========================================================
# Case-study automation: multi-species map + inset trends
# =========================================================
suppressPackageStartupMessages({
  library(dplyr)
  library(lubridate)
  library(ggplot2)
  library(readr)
  library(MASS)
  library(scales)
  library(sf)
  library(rnaturalearth)
  library(rnaturalearthdata)
  library(stringr)
  library(cowplot)
  library(patchwork)
  library(rsvg)
  library(magick)
})

# ---------- robust matcher over multiple name fields ----------
.match_rows <- function(sf_obj, target) {
  target_l <- str_to_lower(target)
  name_fields <- intersect(
    c("admin","name","name_long","formal_en","abbrev","sovereignt","geounit","name_en","iso_a2","iso_a3"),
    names(sf_obj)
  )
  exact_idx <- Reduce(`|`, lapply(name_fields, function(col)
    str_to_lower(sf_obj[[col]]) %in% target_l
  ))
  if (any(exact_idx, na.rm = TRUE)) return(sf_obj[exact_idx, ])
  
  alias <- c(
    "bahamas"       = "The Bahamas",
    "the bahamas"   = "The Bahamas",
    "ivory coast"   = "Côte d’Ivoire",
    "cote d’ivoire" = "Côte d’Ivoire",
    "cabo verde"    = "Cape Verde"
  )
  if (target_l %in% names(alias)) {
    alias_val <- alias[[target_l]]
    exact_idx2 <- Reduce(`|`, lapply(name_fields, function(col)
      str_to_lower(sf_obj[[col]]) %in% str_to_lower(alias_val)
    ))
    if (any(exact_idx2, na.rm = TRUE)) return(sf_obj[exact_idx2, ])
  }
  
  partial_idx <- Reduce(`|`, lapply(name_fields, function(col)
    str_detect(str_to_lower(sf_obj[[col]]), fixed(target_l))
  ))
  sf_obj[partial_idx, ]
}

# calculate joint f1 score of species
get_joint_acc <- function(sp, metrics, sharktx) {
  lineage <- subset(sharktx, species_name == sp,
                    select = c(order_name, family_name, genus_name, species_name))
  
  scores <- c(
    subset(metrics, class == lineage$order_name)$f1score,
    subset(metrics, class == lineage$family_name)$f1score,
    subset(metrics, class == lineage$genus_name)$f1score,
    subset(metrics, class == lineage$species_name)$f1score
  )
  
  prod(scores, na.rm = TRUE)
}

# ---------- Region resolver (country or US state) ----------
# Requires: sf, rnaturalearth, rnaturalearthdata, stringr
# Optional: your .match_rows() helper; if missing, a local matcher is used.

.normalize_bounds <- function(bx) {
  stopifnot(length(bx) == 4)
  # Expect: c(lon_min, lon_max, lat_min, lat_max)
  # If the user passed lat then lon (common mistake), detect & swap.
  # Heuristic: valid longitudes ~ [-180,180], valid latitudes ~ [-90,90]
  lon_like <- abs(bx[c(1,2)]) <= 180
  lat_like <- abs(bx[c(3,4)]) <= 90
  if (!(all(lon_like) && all(lat_like))) {
    # try swapped pattern: c(lat_min, lon_min, lat_max, lon_max)
    bx2 <- c(bx[2], bx[4], bx[1], bx[3])  # (lon_min, lon_max, lat_min, lat_max)
    lon_like2 <- abs(bx2[c(1,2)]) <= 180
    lat_like2 <- abs(bx2[c(3,4)]) <= 90
    if (all(lon_like2) && all(lat_like2)) bx <- bx2
  }
  # enforce min<max on each pair
  if (bx[1] > bx[2]) bx[c(1,2)] <- bx[c(2,1)]
  if (bx[3] > bx[4]) bx[c(3,4)] <- bx[c(4,3)]
  bx
}


resolve_region_sf <- function(
    region_name,
    region_level = c("country", "state", "admin1", "province", "region"),
    country = NULL,         # e.g., "United States of America", "Ecuador"
    buffer_km = 0,          # add shoreline/EEZ-ish padding
    crs_out = 4326
) {
  stopifnot(!is.null(region_name))
  # normalize level synonyms
  rl <- match.arg(region_level)
  if (rl %in% c("state","province","region")) rl <- "admin1"
  
  # local robust matcher (used if your .match_rows() isn't defined)
  .match_rows_multi <- function(sf_obj, target) {
    if (nrow(sf_obj) == 0) return(sf_obj)
    cols <- intersect(
      c("name","name_en","name_long","gn_name","abbrev","postal","woe_name",
        "region","subregion","type_en","type"),
      names(sf_obj)
    )
    if (!length(cols)) return(sf_obj[0,])
    tt <- stringr::str_to_lower(target)
    keep <- Reduce(`|`, lapply(cols, function(cl) {
      stringr::str_detect(stringr::str_to_lower(sf_obj[[cl]]), tt)
    }))
    sf_obj[keep %in% TRUE, ]
  }
  .smart_match <- function(sf_obj, target) {
    if (exists(".match_rows", mode = "function")) .match_rows(sf_obj, target)
    else .match_rows_multi(sf_obj, target)
  }
  
  if (rl == "country") {
    lay <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
    reg <- .smart_match(lay, region_name)
  } else {
    # admin-1 worldwide, optionally constrained by a country
    if (is.null(country)) {
      lay <- rnaturalearth::ne_states(returnclass = "sf")
    } else {
      lay <- rnaturalearth::ne_states(country = country, returnclass = "sf")
    }
    reg <- .smart_match(lay, region_name)
    
    # helpful nudges for common tricky names
    if (nrow(reg) == 0) {
      nm <- region_name
      if (grepl("hawai", nm, ignore.case = TRUE)) {
        if (is.null(country)) lay <- rnaturalearth::ne_states(country = "United States of America", returnclass = "sf")
        reg <- .match_rows_multi(lay, "Hawai")
      }
      if (nrow(reg) == 0 && grepl("galap|galáp", nm, ignore.case = TRUE)) {
        if (is.null(country)) {
          ecu <- rnaturalearth::ne_states(country = "Ecuador", returnclass = "sf")
          reg <- .match_rows_multi(ecu, "galap")
        } else {
          reg <- .match_rows_multi(lay, "galap")
        }
      }
    }
  }
  
  if (nrow(reg) == 0) {
    stop("Region not found. Try alternate spellings (e.g., 'Hawaii', 'Hawaiʻi', 'Galapagos', 'Galápagos') or set `country=`.")
  }
  
  reg <- sf::st_make_valid(reg)
  if (buffer_km != 0) {
    reg <- sf::st_transform(reg, 3857) |>
      sf::st_buffer(buffer_km * 1000) |>
      sf::st_transform(crs_out)
  } else {
    reg <- sf::st_transform(reg, crs_out)
  }
  reg
}


# ---------- small helper: italic title ----------
italic_title <- function(x) ggplot2::ggtitle(label = bquote(italic(.(x))))

# ---------- MAIN FUNCTION ----------
# species_vec: character vector of scientific names
# region_name + region_level: "The Bahamas"/"country", or "Hawaii"/"state" (US)
# bounds: c(lon_min, lon_max, lat_min, lat_max) to override auto-bounds
# alldata_path: path to your alldat_meta CSV
# effort_path: path to iNat effort CSV
# bin: grid cell size (deg) for SPUE map (0.5 matches your pipeline)
# inset_xy/wh: where to place the inset (npc coordinates 0..1)
# return: patchwork plot combining all species with one legend and master title
flickr_case_study_plots <- function(
    species_vec,
    region_name,
    region_level = c("country","state","admin1"),
    country = NULL,                                 # ← NEW: constrain admin1 search (e.g., "Ecuador")
    buffer_km = 0,                                  # ← NEW: shoreline/nearshore padding for region_sf
    bounds = NULL,
    species_cols = 3,                 # << how many panels per row
    alldata_path = "./data/raw/alldat_meta_20250812.csv",
    effort_path = NA,
    bin = 0.5,
    inset_xy = c(0.60, 0.63),
    inset_wh = c(0.36, 0.34),
    legend_position = "bottom",
    pad = 0.15,            # only used if bounds is NULL
    center = NULL, width_deg = NULL, height_deg = NULL,  # optional alternate bounding mode
    ill_dir = "./figures/shark_ill",              
    ill_xy  = c(0.73, 0.73),            # <- NEW: lower-left corner (npc: 0..1)
    ill_wh  = c(0.24, 0.24)             # <- NEW: width/height (npc)
    ) {
  
  region_level <- match.arg(region_level)
  
  # --- load data ---
  sharktx <- sharkDetectoR::get_taxonomy()
  metrics <- sharkDetectoR::get_metrics()
  # alldata <- readr::read_csv(alldata_path, show_col_types = FALSE)
  # effort <- readr::read_csv(effort_path, show_col_types = FALSE)
  alldata = read.csv(alldata_path)
  effort = subset(alldata, source_type == "Flickr" & !is_wild)
  
  # --- region geometry + bounds (explicit > center/size > auto padded) ---
  region_sf <- resolve_region_sf(
    region_name   = region_name,
    region_level  = region_level,
    country       = country,
    buffer_km     = buffer_km,
    crs_out       = 4326
  )
  
  if (!is.null(bounds)) {
    b <- .normalize_bounds(bounds)
    lon_min <- b[1]; lon_max <- b[2]
    lat_min <- b[3]; lat_max <- b[4]
  } else if (!is.null(center) && !is.null(width_deg) && !is.null(height_deg)) {
    lon_min <- center[1] - width_deg/2; lon_max <- center[1] + width_deg/2
    lat_min <- center[2] - height_deg/2; lat_max <- center[2] + height_deg/2
  } else {
    bb <- sf::st_bbox(sf::st_union(region_sf))
    pad_x <- (bb["xmax"] - bb["xmin"]) * pad
    pad_y <- (bb["ymax"] - bb["ymin"]) * pad
    lon_min <- as.numeric(bb["xmin"] - pad_x); lon_max <- as.numeric(bb["xmax"] + pad_x)
    lat_min <- as.numeric(bb["ymin"] - pad_y); lat_max <- as.numeric(bb["ymax"] + pad_y)
  }
  
  # --- per-species loop: compute map grid + plot, trend plot, and collect vmax ---
  land <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
  bbox_poly <- st_polygon(list(rbind(
    c(lon_min, lat_min),
    c(lon_max, lat_min),
    c(lon_max, lat_max),
    c(lon_min, lat_max),
    c(lon_min, lat_min)
  ))) |> st_sfc(crs = 4326)
  
  lat_bounds = c(lat_min, lat_max)
  lon_bounds = c(lon_min, lon_max)
  
  per_species <- list()
  all_spue_vals <- c()
  
  for (sp in species_vec) {
    
    res_single <- prep_shark_obs(
      data        = alldata,
      effort      = effort,
      lat_bounds  = lat_bounds,
      lon_bounds  = lon_bounds,
      validated_col = "is_validated",
      species_col = "sd_species",
      species_mode= "single",
      species     = sp,   
      source_filter = "Flickr",
      bin_deg     = 0.5
    )
    combined_date = res_single$combined_date
    combined_spat = res_single$combined_spat %>% filter(shark_observations > 0)
    
    # ---------- modeling ----------
    p_line <- NULL
    map_df_grid <- NULL
    
    eff_tousers = c("Sphyrna lewini", "Prionace glauca", "Galeocerdo cuvier",
                    "Carcharhinus galapagensis")
    
    # Extract the year for rows with >0 observations
    min_year <- min(as.numeric(format(combined_date$month[combined_date$shark_observations > 0], "%Y")), 
                    na.rm = TRUE)
    # Create the yearly sequence
    yrs <- seq(min_year, 2024)
    m1 <- fit_shark_trend2(
      combined_date,
      effort_offset_col   = if (sp %in% eff_tousers) "total_users" else "total_observations",   # or "total_users"
      start_when_first_obs= FALSE,
      # drop_years = c(2025),
      years_keep          = yrs,
      weight_strategy     = "none",
      season_type         = "harmonic",
      trend_type          = "linear", 
      trend_scale         = "calendar",             # or "std"
      k_year              = 12,
      average_months      = TRUE,
      effort_fixed        = 1000,
      clip_ci_q           = 0.975,
      x_ticks_n           = 4
    )

      p_line <- m1$plot
      
      if (!is.null(p_line)) {

        # map grid (raw SPUE) for color scale & tiles
        map_df_grid <- combined_spat %>%
          group_by(lat_bin, lon_bin) %>%
          summarise(
            counts = sum(shark_observations, na.rm = TRUE),
            effort = sum(pmax(total_observations, 1L),    na.rm = TRUE),
            .groups = "drop"
          ) %>%
          mutate(
            spue_cell = (counts / pmax(effort, 1)),
            x = lon_bin + bin/2,
            y = lat_bin + bin/2
          )
        
        all_spue_vals <- c(all_spue_vals, map_df_grid$spue_cell)
      }
  
    
    
    # count observations
    n_obs <- sum(combined_date$shark_observations, na.rm = TRUE)
    
    # calculate joint f1 score of species
    acc_spec <- get_joint_acc(sp, metrics, sharktx)
    
    per_species[[sp]] <- list(
      p_line = p_line,
      map_df_grid = map_df_grid,
      n_obs = n_obs,
      acc_spec = acc_spec
    )
  }
  
  # --- global color scale (shared legend) ---
  if (length(all_spue_vals) == 0) {
    stop("No SPUE values could be computed for the requested species/region/bounds.")
  }
  vmax <- quantile(all_spue_vals, 0.99, na.rm = TRUE)
  
  # ---- build a shared legend seed (no visible panel), OUTSIDE the loop ----
  legend_seed <- ggplot(
    data.frame(x = 1, y = 1, z = c(0, vmax, vmax, 0)),
    aes(x, y, fill = z)
  ) +
    geom_raster(alpha = 0, show.legend = TRUE) +   # invisible layer still carries 'fill'
    scale_fill_viridis_c(
      limits = c(0, vmax),
      option = "C",
      name   = "SPUE",
      guide  = guide_colorbar(barwidth = unit(6, "cm"),
                              barheight = unit(0.6, "cm"))
    ) +
    theme_void() +
    theme(
      legend.position   = "bottom",
      legend.title      = element_text(size = 13),
      legend.text       = element_text(size = 11),
      legend.margin     = margin(0, 0, 0, 0),   # << tighten legend itself
      plot.margin       = margin(0, 0, 0, 0),   # << remove outer white frame
      legend.key.height = unit(0.7, "cm"),
      legend.key.width  = unit(0.95, "cm")
    )

  # --- build each species panel (map + inset), then combine ---
  land <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
  
  # label point for region (nice to have)
  reg_label_pts <- st_point_on_surface(st_geometry(st_union(region_sf))) |> st_as_sf()
  reg_label_pts$lab <- region_name
  
  panels <- list()
  for (i in seq_along(names(per_species))) {
    sp  <- names(per_species)[i]
    res <- per_species[[sp]]
    
    # empty species fallback
    if (is.null(res$p_line) || is.null(res$map_df_grid) || nrow(res$map_df_grid) == 0) {
      panels[[i]] <- ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = paste("No data for", sp), size = 5) +
        theme_void() +
        ggtitle(bquote(italic(.(sp)))) +
        theme(plot.title = element_text(size = 7))
      next
    }
    
    # --- MAP (no inset) ---
    p_map <- ggplot() +
      geom_tile(
        data = res$map_df_grid,                     # <- use the same object you summarized
        aes(x = x, y = y, fill = spue_cell),
        width = bin, height = bin, alpha = 0.95
      ) +
      geom_sf(data = land, fill = "grey50", color = "#1f2228", linewidth = 0.25) +
      geom_sf(data = bbox_poly, fill = NA, color = "white", linewidth = 0.8) +
      coord_sf(xlim = c(lon_min, lon_max), ylim = c(lat_min, lat_max), expand = FALSE) +
      scale_fill_viridis_c(
        limits = c(0, vmax),
        oob    = scales::squish,        # <- cap out-of-range into the legend range
        na.value = "transparent",
        option = "C", name = "SPUE"
      ) +
      scale_x_continuous(
        breaks = seq(floor(lon_min), ceiling(lon_max), by = 2),
        labels = function(b) paste0(abs(b), "°", ifelse(b < 0, "W", "E"))
      ) +
      labs(x = NULL, y = NULL) +
      ggtitle(bquote(italic(.(sp)))) +
      theme_minimal(base_size = 9) +
      guides(fill = "none", color = "none", linetype = "none", shape = "none") +
      theme(
        panel.background = element_rect(fill = "white", color = NA),
        plot.background  = element_rect(fill = "white", color = NA),
        panel.grid = element_blank(),
        axis.text  = element_text(size = 9),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 0.6),
        plot.title   = element_text(size = 11.5, face = "plain", hjust = 0),
        plot.margin  = margin(1, 1, 1, 1)
      )
    
    # --- LINE panel (side column) ---
    p_line <- res$p_line +                                
      theme(
        plot.title  = element_blank(),
        legend.position = "none"
      )
    
    title_italic_n <- function(sp, n, acc) {
      acc_pct <- percent(acc, accuracy = 1)   # e.g. 0.991 -> "99%"
      
      bquote(atop(
        italic(.(sp)) ~ "(n =" ~ .(n) ~ ")",
        "Accuracy =" ~ .(acc_pct)
      ))
    }
    
    n_obs_sp   <- if (!is.null(res$n_obs)) res$n_obs else 0L
    acc_spec_sp <- if (!is.null(res$acc_spec)) res$acc_spec else 0L
    
    title_expr <- title_italic_n(sp, n_obs_sp, acc_spec_sp)
    
    p_map <- p_map + labs(title = title_expr) + 
      theme(plot.title = element_text(hjust = 0))   # left align
    
    # --- NEW: try to overlay the species SVG on the MAP panel ---
    # Build filename like "Genus_species.svg", with lowercase fallback
    ill_base <- str_replace(sp, " ", "_")
    ill_file <- file.path(ill_dir, paste0(ill_base, ".svg"))
    if (!file.exists(ill_file)) {
      ill_file_lower <- file.path(ill_dir, paste0(tolower(ill_base), ".svg"))
      if (file.exists(ill_file_lower)) ill_file <- ill_file_lower
    }
    
    if (file.exists(ill_file)) {
      # Render SVG to PNG (keeps transparency) and overlay
      tmp_png <- tempfile(fileext = ".png")
      ok <- FALSE
      try({
        rsvg::rsvg_png(ill_file, tmp_png, width = 1200, height = 1200)
        ok <- file.exists(tmp_png)
      }, silent = TRUE)
      
      if (ok) {
        # cowplot draw_image uses lower-left origin in npc; keep within bounds
        p_map <- cowplot::ggdraw() +
          cowplot::draw_plot(p_map) +
          cowplot::draw_image(
            tmp_png,
            x = ill_xy[1], y = ill_xy[2],      # lower-left corner
            width  = ill_wh[1], height = ill_wh[2],
            hjust = 0, vjust = 0,             # anchor at lower-left
            interpolate = TRUE
          )
      }
      
    }
    
    # --- side-by-side panel for this species (no inset) ---
    panels[[i]] <- p_map + p_line +
      plot_layout(widths = c(5, 4)) &
      theme(plot.margin = margin(4, 8, 4, 4))  # tighter margins inside each sub-plot
  }
  
  combined_wrap <- wrap_plots(panels, ncol = species_cols) +
    plot_annotation(title = paste0(region_name, " - Flickr")) &
    theme(
      plot.margin = margin(3, 6, 3, 3),
      axis.ticks.length = unit(0.15, "cm"),
      plot.title = element_text(size = 16, face = "bold")
    )
  
  final_plot <- (
    (wrap_plots(panels, ncol = species_cols) / legend_seed) +
      plot_layout(heights = c(1, 0), guides = "collect") +
      plot_annotation(
        title = paste0(region_name, " — Flickr"),
        # style the annotation title here if you like:
        theme = theme(plot.title = element_text(size = 16, face = "bold", margin = margin(b = 6)))
      )
  ) & theme(legend.position = "bottom")
  
  
  return(final_plot)

}

#################################################################################
source("./modules/flickr/flickr_data.R")
source("./modules/flickr/flickr_pline_final.R")

hawaii_species_vec <- c(
  "Galeocerdo cuvier",
  "Sphyrna lewini",
  "Rhincodon typus",
  "Triaenodon obesus",
  # "Prionace glauca",
  "Carcharias taurus",
  "Carcharhinus amblyrhynchos",
  "Carcharhinus galapagensis"
  # "Carcharhinus melanopterus",
  # "Carcharodon carcharias"
)

# Example 1 — Hawaii, auto-bounds with padding knob
p_hawaii <- flickr_case_study_plots(
  species_vec   = hawaii_species_vec,
  region_name   = "Hawaii",
  region_level  = "state",
  country       = "United States of America",  
  alldata_path = "./data/raw/alldat_meta_20250826.csv",
  buffer_km     = 25,                          # include nearshore waters
  pad           = 0.2,                 # widen view a bit
  bin           = 0.5,
  bounds = c(-160, -154, 18.5, 23.5), # Hawaii
  ill_xy  = c(0.525, 0.5),            # species illustrations <- lower-left corner (npc: 0..1)
  ill_wh  = c(0.45, 0.45)             # <- width/height (npc)
)
p_hawaii

ggsave("./figures/Hawaii_flickr_plots.pdf", plot = p_hawaii, width = 19, height = 11)
ggsave("./figures/Hawaii_flickr_plots.png", plot = p_hawaii, width = 19, height = 11)

################################################################################


# Ginglymostoma cirratum 
# Negaprion brevirostris
# Sphyrna mokarran
# Carcharhinus leucas
# Galeocerdo cuvier
# Carcharhinus limbatus
# Carcharhinus falciformis
bahamas_species_vec <- c(
  "Galeocerdo cuvier",
  "Ginglymostoma cirratum",
  # "Carcharodon carcharias",
  "Triaenodon obesus",
  "Rhincodon typus",
  "Carcharhinus perezii",
  # "Carcharhinus amblyrhynchos",
  "Carcharhinus leucas",
  "Carcharhinus melanopterus"
)

# Example 2 — Bahamas, auto-bounds with padding knob
p_bahamas <- flickr_case_study_plots(
  species_vec   = bahamas_species_vec,
  region_name   = "Bahamas",
  region_level  = "country",
  alldata_path = "./data/raw/alldat_meta_20250826.csv",
  buffer_km     = 25,                          # include nearshore waters
  pad           = 0.2,                 # widen view a bit
  bin           = 0.5,
  bounds = c(-82.0365, -67.7489, 17.7121, 29.3775), # Bahamas
  ill_xy  = c(0.525, 0.5),            # species illustrations <- lower-left corner (npc: 0..1)
  ill_wh  = c(0.45, 0.45)             # <- width/height (npc)
)
p_bahamas

ggsave("./figures/Bahamas_flickr_plots.pdf", plot = p_bahamas, width = 19, height = 11)
ggsave("./figures/Bahamas_flickr_plots.png", plot = p_bahamas, width = 19, height = 11)

################################################################################


