library(readr)
source("./flickr_data.R")

# Load your inputs

alldata <- read.csv("../../data/processed/flickr_20250827.csv")
flickr_effort = subset(alldata, source_type == "Flickr" & has_spatiotemporal 
                       & !inland & !sd_is_shark & !cs_is_shark & is.na(species_name))


# Hawaii
lat_bounds <- c(18.5, 23.5)
lon_bounds <- c(-160, -154)

# Bahamas
lat_bounds <- c(17.7121, 29.3775)
lon_bounds <- c(-82.0365, -67.7489)
 
# Example 2: Maldives
lat_bounds = c(-2.0242, 10.754)
lon_bounds = c(67.284960, 84.624)

alldat_loc = alldata %>%
  filter(latitude > lat_bounds[1] & latitude < lat_bounds[2]) %>%
  filter(longitude > lon_bounds[1] & longitude < lon_bounds[2])
table(alldat_loc$sd_species)

res_single <- prep_shark_obs(
  data        = alldata,
  effort      = flickr_effort,
  lat_bounds  = lat_bounds,
  lon_bounds  = lon_bounds,
  validated_col = "is_wild",
  species_col = "sd_species",
  species_mode= "single",
  species     = "Orectolobus halei",   # change as needed
  source_filter = "Flickr",
  bin_deg     = 0.5
)
combined_date = res_single$combined_date
str(res_single$combined_spat)
str(res_single$combined_date)
head(res_single$combined_date)

# Quick plot: monthly shark vs total observations (Bahamas, species-specific)
library(ggplot2)
ggplot(res_single$combined_date, aes(month, shark_observations)) +
  geom_col() +
  labs(x = "Month", y = "Shark observations", title = "Bahamas — Galeocerdo cuvier (monthly)") +
  theme_minimal()

# Example 2: Galápagos, multi-species (i.e., keep all)
lat_bounds <- c(-1.803904, 1.254211)
lon_bounds <- c(-92.114611, -88.259070)

res_multi <- prep_shark_obs(
  data         = alldata,
  effort       = effort,
  lat_bounds   = lat_bounds,
  lon_bounds   = lon_bounds,
  species_mode = "multi",     # keep all species
  species      = NULL,        # not needed in "multi" mode
  source_filter = "iNaturalist",
  bin_deg      = 0.5
)

# Sanity checks
dplyr::glimpse(res_multi$combined_spat)
dplyr::glimpse(res_multi$combined_date)

# Quick spatial grid preview for a specific month
one_month <- res_multi$combined_spat %>% dplyr::filter(month == min(month))
ggplot(one_month, aes(lon_bin, lat_bin, fill = shark_observations)) +
  geom_tile() +
  coord_equal() +
  labs(title = paste("Galápagos —", format(min(res_multi$combined_spat$month), "%Y-%m")),
       x = "Longitude (bin)", y = "Latitude (bin)", fill = "Shark obs") +
  theme_minimal()
