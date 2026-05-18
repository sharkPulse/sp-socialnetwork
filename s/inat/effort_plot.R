library(dplyr)
library(ggplot2)
library(sf)
library(maps)
library(lubridate)

lat_bounds <- c(16, 31)
lon_bounds <- c(-85, -68)

effort_clean <- effort %>%
  filter(
    !is.na(latitude),
    !is.na(longitude),
    inland == FALSE,
    latitude >= lat_bounds[1], latitude <= lat_bounds[2],
    longitude >= lon_bounds[1], longitude <= lon_bounds[2]
  )

effort_binned <- effort_clean %>%
  mutate(
    lat_bin = floor(latitude),
    lon_bin = floor(longitude)
  ) %>%
  group_by(lat_bin, lon_bin) %>%
  summarize(effort = n(), .groups = "drop")

effort_time <- effort_clean %>%
  mutate(
    date = as.Date(observed_on),
    year_month = floor_date(date, "month")
  ) %>%
  group_by(year_month) %>%
  summarize(effort = n(), .groups = "drop")

world <- map_data("world")

bahamas_map <- world %>%
  filter(
    lat >= lat_bounds[1], lat <= lat_bounds[2],
    long >= lon_bounds[1], long <= lon_bounds[2]
  )

p_map <- ggplot() +
  
  # Effort bins
  geom_tile(
    data = effort_binned,
    aes(
      x = lon_bin,
      y = lat_bin,
      fill = effort
    ),
    alpha = 0.8
  ) +
  
  # Land
  geom_polygon(
    data = bahamas_map,
    aes(x = long, y = lat, group = group),
    fill = "grey55", color = "gray50"
  ) +
  
  scale_fill_viridis_c(name = "Total Posts") +
  
  coord_fixed(
    xlim = c(lon_bounds[1]+4, lon_bounds[2]-1.9),
    ylim = c(lat_bounds[1]+2.5, lat_bounds[2]-4)
  ) +
  
  labs(
    title = "Total iNaturalist Posting Activity",
    x = "Longitude",
    y = "Latitude"
  ) +
  
  theme_minimal()

labels <- data.frame(
  name = c(
    "Bahamas",
    "Florida",
    "Cuba",
    "Haiti",
    "Dominican Republic",
    "Turks and Caicos",
    "Exuma",
    "Nassau",
    "Freeport"
  ),
  lon = c(
    -76.5,  # Bahamas
    -81,  # Florida
    -79.0,  # Cuba
    -72.25,  # Haiti
    -70.5,  # DR
    -71.5,  # Turks & Caicos
    -76.0,  # Exuma
    -77.35, # Nassau
    -78.7   # Freeport
  ),
  lat = c(
    24.5,  # Bahamas
    26.5,  # Florida
    22,  # Cuba
    19.0,  # Haiti
    19.0,  # DR
    22,  # Turks & Caicos
    23.5,  # Exuma
    25.05, # Nassau
    26.75   # Freeport
  )
)

p_map = p_map +
  geom_text(
    data = labels,
    aes(x = lon, y = lat, label = name),
    size = 3,
    fontface = "bold",
    color = "black"
  )
p_map


p_time <- ggplot(effort_time, aes(x = year_month, y = effort)) +
  
  geom_line(color = "black", linewidth = 0.75) +
  # geom_point(size = 2, color = "#2C7BB6") +
  xlim(as.Date("2000-01-01"),as.Date("2025-01-01")) +
  labs(
    title = "iNaturalist Posts",
    x = "Year",
    y = "Posts"
  ) +
  
  theme_minimal()

p_time

ggsave(p_map, filename = "../../figures/inat_effort.png", device = "png", width = 7, height = 5)
ggsave(p_time, filename = "../../figures/inat_effort_time.jpg", device = "jpg", width = 4, height = 2)
