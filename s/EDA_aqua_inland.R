library(dplyr)

# 1. Tag every row _and_ keep that tag through filtering
alldat2 <- alldat %>%
  mutate(row_id = row_number()) %>%
  mutate(aquarium_sp = as.logical(aquarium_sp))
  

valid <- alldat2 %>%
  filter(!is.na(latitude), latitude != "",
         !is.na(longitude), longitude != "")

# 2. Compute ONLY on valid (which has row_id)
spatial_flags <- valid %>%
  (function(df) removeInlands3(df, -10)) %>%
  (function(df) removeAqua2(aqua, df, max_distance_km = 1)) %>%
  mutate(
    aquarium = if_else(
      source_type %in% c("Instagram","Flickr") & tolower(validated) %in% c("t", "true", "research") &
        !is.na(aquarium_sp) & aquarium_sp != "",
      aquarium_sp,
      aquarium
    )
  ) %>%
  select(row_id, inland, aquarium)

# 3. Merge back onto alldat2
alldat3 <- alldat2 %>%
  left_join(spatial_flags, by = "row_id") %>%
  select(-c(row_id))
