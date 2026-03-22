suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(lubridate)
})

#' Prepare shark observations & effort summaries by space (grid) and time (month)
#'
#' @param data    data.frame of shark observations (e.g., iNaturalist subset of your master)
#' @param effort  data.frame of platform observation effort (same platform as `data`)
#' @param lat_bounds numeric length-2 c(lat_min, lat_max)
#' @param lon_bounds numeric length-2 c(lon_min, lon_max)
#' @param species_mode "single" or "multi"; ignored if `species` is a vector of length > 1
#' @param species character scalar (exact species) or character vector; if NULL, keep all
#' @param source_filter character (e.g., "iNaturalist"); set NULL to skip
#' @param bin_deg numeric grid size in degrees (default 0.5)
#' @param require_wild logical, keep only wild observations if column `is_wild` exists
#' @param date_col name of the observation date column in `data` (default "date")
#' @param species_col name of the species column in `data` (default "species_name")
#' @param validated_col name of the validation flag in `data` (default "is_validated")
#' @param effort_time_col datetime col in `effort` with ISO date prefix (default "time_observed_at")
#' @param effort_user_col user id col in `effort` (default "user_id")
#' @param effort_lat_col  latitude column in `effort` (default "latitude")
#' @param effort_lon_col  longitude column in `effort` (default "longitude")
#' @param drop_inland logical; if TRUE and `effort$inland` exists, drop inland rows
#'
#' @return list(combined_spat = tibble, combined_date = tibble, params = list(...))
#'
prep_shark_obs <- function(
    data,
    effort,
    lat_bounds,
    lon_bounds,
    species_mode      = c("single","multi"),
    species           = NULL,
    source_filter     = "iNaturalist",
    bin_deg           = 0.5,
    require_wild      = TRUE,
    date_col          = "date",
    species_col       = "species_name",
    validated_col     = "is_validated",
    effort_time_col   = "time_observed_at",
    effort_user_col   = "user_id",
    effort_lat_col    = "latitude",
    effort_lon_col    = "longitude",
    drop_inland       = TRUE
) {
  species_mode <- match.arg(species_mode)
  
  # --- basic input checks ------------------------------------------------------
  needed_obs <- c("latitude","longitude", date_col)
  missing_obs <- setdiff(needed_obs, names(data))
  if (length(missing_obs)) stop("Missing columns in `data`: ", paste(missing_obs, collapse=", "))
  
  needed_eff <- c(effort_lat_col, effort_lon_col, effort_time_col)
  missing_eff <- setdiff(needed_eff, names(effort))
  if (length(missing_eff)) stop("Missing columns in `effort`: ", paste(missing_eff, collapse=", "))
  
  # --- species filter logic ----------------------------------------------------
  wants_species <- !is.null(species) && length(species) > 0
  if (wants_species && !(species_col %in% names(data))) {
    stop("`species_col` ('", species_col, "') not found in `data`.")
  }
  
  # --- source filter (optional) -----------------------------------------------
  dat <- data
  if (!is.null(source_filter) && ("source_type" %in% names(dat))) {
    dat <- dat %>% filter(.data$source_type == !!source_filter)
  }
  
  # --- keep wild if requested and available -----------------------------------
  if (require_wild && ("is_wild" %in% names(dat))) {
    dat <- dat %>% filter(.data$is_wild %in% c(TRUE, 1))
  }
  
  # --- species mode ------------------------------------------------------------
  if (wants_species) {
    # exact match on a vector of species
    dat <- dat %>% filter(.data[[species_col]] %in% species)
  } else if (species_mode == "single") {
    # single species path expects `species` scalar
    if (!is.null(species) && length(species) == 1L) {
      dat <- dat %>% filter(.data[[species_col]] == !!species)
    }
  } # if multi, keep all species
  
  # --- parse observation dates & bin to grid ----------------------------------
  # allow character POSIX/Date in `date_col`
  dat <- dat %>%
    mutate(
      date_observed = as.Date(.data[[date_col]]),
      lat_bin = floor(.data$latitude  / bin_deg) * bin_deg,
      lon_bin = floor(.data$longitude / bin_deg) * bin_deg,
      month   = floor_date(date_observed, "month"),
      year    = year(date_observed)
    ) %>%
    filter(
      latitude  >= lat_bounds[1], latitude  <= lat_bounds[2],
      longitude >= lon_bounds[1], longitude <= lon_bounds[2]
    )
  
  # --- validated flag ----------------------------------------------------------
  has_validated <- validated_col %in% names(dat)
  if (!has_validated) dat[[validated_col]] <- NA
  
  # --- effort prep -------------------------------------------------------------
  eff <- effort %>%
    # basic NA guards
    filter(
      !is.na(.data[[effort_lat_col]]),
      !is.na(.data[[effort_lon_col]]),
      !is.na(.data[[effort_time_col]])
    )
  
  # drop inland if that column exists and requested
  if (drop_inland && ("inland" %in% names(eff))) {
    eff <- eff %>% filter(!.data$inland)
  }
  
  eff <- eff %>%
    mutate(
      date_observed = as.Date(substr(.data[[effort_time_col]], 1, 10)),
      lat_bin = floor(.data[[effort_lat_col]]  / bin_deg) * bin_deg,
      lon_bin = floor(.data[[effort_lon_col]] / bin_deg) * bin_deg,
      month   = floor_date(date_observed, "month")
    ) %>%
    filter(
      .data[[effort_lat_col]]  >= lat_bounds[1], .data[[effort_lat_col]]  <= lat_bounds[2],
      .data[[effort_lon_col]]  >= lon_bounds[1], .data[[effort_lon_col]]  <= lon_bounds[2]
    )
  
  # --- spatial (grid + month) summaries ---------------------------------------
  grid_summary <- dat %>%
    group_by(lat_bin, lon_bin, month) %>%
    summarise(
      shark_observations = n(),
      validated_obs      = sum(.data[[validated_col]], na.rm = TRUE),
      auto_obs           = shark_observations - validated_obs,
      species_n          = if (species_col %in% names(dat)) n_distinct(.data[[species_col]]) else NA_integer_,
      .groups = "drop"
    )
  
  effort_summary <- eff %>%
    group_by(lat_bin, lon_bin, month) %>%
    summarise(
      total_users        = if (effort_user_col %in% names(eff)) n_distinct(.data[[effort_user_col]]) else NA_integer_,
      total_observations = n(),
      .groups = "drop"
    )
  
  combined_spat <- full_join(grid_summary, effort_summary, by = c("lat_bin","lon_bin","month")) %>%
    mutate(
      shark_observations = replace_na(shark_observations, 0L),
      validated_obs      = replace_na(validated_obs, 0L),
      auto_obs           = pmax(shark_observations - validated_obs, 0L),
      total_observations = replace_na(total_observations, 0L),
      total_users        = replace_na(total_users, 0L),
      month_index        = month(month),
      year_observed      = year(month),
      month_factor       = factor(month(month), levels = 1:12, labels = month.abb),
      month_num          = as.integer(month_factor)
    ) %>%
    arrange(month, lat_bin, lon_bin)
  
  # --- monthly summaries (time-only) ------------------------------------------
  date_summary <- dat %>%
    group_by(month) %>%
    summarise(
      shark_observations = n(),
      validated_obs      = sum(.data[[validated_col]], na.rm = TRUE),
      auto_obs           = shark_observations - validated_obs,
      species_n          = if (species_col %in% names(dat)) n_distinct(.data[[species_col]]) else NA_integer_,
      .groups = "drop"
    )
  
  effort_date_summary <- eff %>%
    group_by(month) %>%
    summarise(
      total_users        = if (effort_user_col %in% names(eff)) n_distinct(.data[[effort_user_col]]) else NA_integer_,
      total_observations = n(),
      .groups = "drop"
    )
  
  combined_date <- full_join(date_summary, effort_date_summary, by = "month") %>%
    mutate(
      shark_observations = replace_na(shark_observations, 0L),
      validated_obs      = replace_na(validated_obs, 0L),
      auto_obs           = pmax(shark_observations - validated_obs, 0L),
      total_users        = replace_na(total_users, 0L),
      total_observations = replace_na(total_observations, 0L),
      month_index        = month(month),
      year_observed      = year(month),
      month_factor       = factor(month(month), levels = 1:12, labels = month.abb),
      month_num          = as.integer(month_factor)
    ) %>%
    arrange(month)
  
  list(
    combined_spat  = combined_spat,
    combined_date  = combined_date,
    params = list(
      lat_bounds   = lat_bounds,
      lon_bounds   = lon_bounds,
      species_mode = species_mode,
      species      = species,
      source       = source_filter,
      bin_deg      = bin_deg
    )
  )
}
