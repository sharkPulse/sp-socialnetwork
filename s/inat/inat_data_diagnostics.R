library(readr)
source("./modules/inat/inat_data.R")

# Load your inputs
alldata <- read.csv("./data/raw/inat/alldat_meta_20260310.csv")
# effort  <- read.csv("./data/raw/inat/inat_effort_20260310.csv")
con <- sharkPulseR::connectPelagic("spr", "spr_pass")
effort <- sharkPulseR::selectData(con, "select * from inat_effort;")
DBI::dbDisconnect(con)

effort = effort %>%
  filter(!is.na(latitude) | !is.na(longitude))

effort = sharkPulseR::removeInlands3(effort, -10) %>%
  filter(!inland)

write.csv(effort, "./data/raw/inat/inat_effort_20260310.csv", row.names = FALSE)
# stop()
# Hawaii
lat_bounds <- c(18.5, 23.5)
lon_bounds <- c(-160, -154)

# Example 1: Bahamas, one species
lat_bounds <- c(17.7121, 29.3775)
lon_bounds <- c(-82.0365, -67.7489)
 
# Example 2: Maldives
lat_bounds = c(-2.0242, 10.754)
lon_bounds = c(67.284960, 84.624)
table(alldat_mald$species_name)

alldata = alldata %>%
  mutate(sd_is_shark == TRUE,
         cs_is_shark == TRUE,
         is_wild = if_else(!aquarium | !inland, TRUE, FALSE),
         is_validated == TRUE)

res_single <- prep_shark_obs(
  data        = alldata,
  effort      = effort,
  lat_bounds  = lat_bounds,
  lon_bounds  = lon_bounds,
  species_mode= "single",
  species     = "Carcharhinus melanopterus",   # change as needed
  source_filter = "iNaturalist",
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

combined_date = res_multi$combined_date

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
