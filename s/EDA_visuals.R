# all data - alldat
# alldata = read.csv("./data/raw/alldat_meta_20250826.csv")
# source("./s/SNgetEffort.R")
# alleff = SNgetEffort("spr", "spr_pass")

###########################################################
# =========================================================
# Global map of shark posts (points only) — fast + toggle
# =========================================================
suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
  library(sf)
  library(ggplot2)
  library(rnaturalearth)
  library(rnaturalearthdata)
  library(scales)
})

plot_world_points <- function(
    alldata,
    source_col      = "source_type",   # "source" or "source_type"
    flag_col        = "is_wild",       # "is_wild" or "is_validated"
    show_all_points = TRUE,            # <- toggle: TRUE = plot ALL points; FALSE = sample
    sample_points   = 2e5,             # used only when show_all_points = FALSE
    point_size      = 0.20,
    point_alpha     = 0.25
) {
  stopifnot(source_col %in% names(alldata), flag_col %in% names(alldata))
  
  # palette + desired plotting order
  pal <- c(Instagram = "#962fbf", Flickr = "#FF0084", iNaturalist = "#74AC00", GBIF = "#175CA1")
  platform_levels <- c("GBIF","iNaturalist","Flickr","Instagram")
  
  # helper to parse booleans in your flags
  as_boolish <- function(x) {
    if (is.logical(x)) return(x)
    tolower(as.character(x)) %in% c("1","true","t","yes","y")
  }
  
  # ---- Clean & filter (points only) ----
  df <- alldata |>
    dplyr::mutate(
      .flag      = as_boolish(.data[[flag_col]]),
      lon        = suppressWarnings(as.numeric(.data$longitude)),
      lat        = suppressWarnings(as.numeric(.data$latitude)),
      source_raw = dplyr::coalesce(.data[[source_col]], .data$source, .data$source_type),
      source_std = dplyr::recode(stringr::str_to_lower(source_raw),
                                 "instagram"="Instagram","ig"="Instagram",
                                 "flickr"="Flickr",
                                 "inaturalist"="iNaturalist","inat"="iNaturalist",
                                 "gbif"="GBIF", .default="Other")
    ) |>
    dplyr::filter(.flag, !is.na(lon), !is.na(lat),
                  dplyr::between(lat, -60, 90), dplyr::between(lon, -180, 180)) |>
    dplyr::filter(source_std %in% platform_levels) |>
    dplyr::mutate(source_std = factor(source_std, levels = platform_levels))
  
  if (nrow(df) == 0L) stop("No rows to plot after filtering. Check 'source_col' and 'flag_col'.")
  
  # ---- To sf + (optional) sampling ----
  crs_wgs84 <- 4326
  crs_robin <- "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
  
  pts <- sf::st_as_sf(df, coords = c("lon","lat"), crs = crs_wgs84, remove = FALSE) |>
    sf::st_transform(crs_robin)
  
  n_pts <- nrow(pts)
  if (!show_all_points && n_pts > sample_points) {
    set.seed(42)
    pts <- pts[sample.int(n_pts, sample_points), ]
  }
  n_plotted <- nrow(pts)
  
  # ---- Base map (no Antarctica + darker land) ----
  world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") |>
    dplyr::filter(continent != "Antarctica") |>
    sf::st_transform(crs_robin)
  
  # ---- Draw in requested order (GBIF -> iNat -> Flickr -> IG) ----
  pts_by_src <- lapply(platform_levels, function(s) pts[pts$source_std == s, ])
  
  p_points <- ggplot2::ggplot() +
    # legend layer (mapped to color so legend shows correct names+colors)
    ggplot2::geom_sf(data = pts,
                     ggplot2::aes(color = source_std),
                     alpha = 0, size = 0, show.legend = TRUE) +
    # ordered draw with fixed colors (doesn't affect legend)
    ggplot2::geom_sf(data = pts_by_src[[1]], color = pal["GBIF"],        alpha = point_alpha, size = point_size, show.legend = FALSE) +
    ggplot2::geom_sf(data = pts_by_src[[2]], color = pal["iNaturalist"], alpha = point_alpha, size = point_size, show.legend = FALSE) +
    ggplot2::geom_sf(data = pts_by_src[[4]], color = pal["Instagram"],   alpha = point_alpha, size = point_size, show.legend = FALSE) +
    ggplot2::geom_sf(data = pts_by_src[[3]], color = pal["Flickr"],      alpha = point_alpha, size = point_size, show.legend = FALSE) +
    ggplot2::geom_sf(data = world, fill = "grey70", color = "grey40", linewidth = 0.2) +
    ggplot2::coord_sf(crs = crs_robin) +
    ggplot2::scale_color_manual(values = pal, breaks = platform_levels, drop = FALSE, name = "Source") +
    ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(alpha = 1, size = 2))) +
    ggplot2::labs(
      title = sprintf("Shark Observations by Platform", flag_col),
      subtitle = if (show_all_points)
        sprintf("%s observations", scales::comma(n_plotted))
      else
        sprintf("Showing %s of %s points (random sample)", scales::comma(n_pts)),
      caption = ""
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      axis.title = ggplot2::element_blank(),
      plot.title = element_text(size = 18),
      plot.subtitle = element_text(size = 16),
      legend.title = element_text(size = 15),
      legend.text = element_text(size = 14.5),
      panel.grid.major = ggplot2::element_line(color = "grey88", linewidth = 0.4),
      legend.position = "right",
      plot.title.position = "plot"
    )
  
  # return just what you need for points
  list(
    points = p_points,
    points_sf = pts,
    n_plotted = n_plotted,
    n_total = n_pts
  )
}

alldata = read.csv("./data/processed/alldat_combined_20250827.csv")
# ---- Usage ----
# All points (may be heavy if you have millions):
res <- plot_world_points(alldata, source_col = "source_type", 
                         flag_col = "is_wild", 
                         show_all_points = TRUE,
                         point_size = 0.45,
                         point_alpha = 0.45)
print(res$points)

# save as PNG
ggplot2::ggsave(
  filename = "./figures/obs_validated_summary.png",
  plot     = res$points,
  width    = 10, height = 5, dpi = 300
)

# or save as PDF (vector format, great for publication)
ggplot2::ggsave(
  filename = "./figures/obs_validated_summary.pdf",
  plot     = res$points,
  width    = 14, height = 7
)
