library(ggplot2)
library(dplyr)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(gridExtra)
library(viridis)
library(stringr)
library(purrr)
library(DBI)

# -----------------------------------
# 1) Load & preprocess datasets
# -----------------------------------

library(sharkPulseR)
source("./s/SNgetSharkPulse.R")
con = connectPelagic("spr", "spr_pass")
aqua = selectData(con, "select * from aquaria")
# tx <- sharkDetectoR::get_taxonomy()
tx = dbGetQuery(con, "SELECT species_name from taxonomy3;")
species_list = tx$species_name
print("Sourcing All Social Network Data from SharkPulse...")
alldat = SNgetSharkPulse("spr", "spr_pass") 
# Keep only the three networks of interest
# selected_sources <- c("iNaturalist", "Flickr", "Instagram")
selected_sources <- c("Instagram")
alldat   <- filter(alldat,   source_type %in% selected_sources)
colnames(alldat)
# tx <- sharkDetectoR::get_taxonomy()
dbDisconnect(con)
print("Calculating Distance to Coast and Aquariums...")
# source("./s/EDA_aqua_inland.R") # takes a long time, need to insert into db
# save(alldat3, file = "./data/raw/inat_alldat3_20260322.RData")

if (c("iNaturalist", "GBIF") %in% selected_sources) {
alldat$aquarium = FALSE

# 1. Tag every row _and_ keep that tag through filtering
alldat2 <- alldat %>%
  mutate(row_id = row_number())
  

valid <- alldat2 %>%
  filter(!is.na(latitude), latitude != "",
         !is.na(longitude), longitude != "")

# 2. Compute ONLY on valid (which has row_id)
spatial_flags <- valid %>%
  (function(df) removeInlands3(df, -10)) %>%
  select(row_id, inland)

# 3. Merge back onto alldat2
alldat3 <- alldat2 %>%
  left_join(spatial_flags, by = "row_id") %>%
  select(-c(row_id))
# alldat3$sd_is_shark = if_else(alldat3$source_type=="GBIF", TRUE, alldat3$sd_is_shark)
}
else {
  source("./s/EDA_aqua_inland.R")
}

# --------------------------------------------
# 2) Use saved RData for Quicker Processing
# --------------------------------------------

alldat4 <- alldat3 %>%
  mutate(
    # (2.1) “Is shark?” — sd_species must be one of the shark names
    sd_is_shark = sd_species %in% species_list  | species_name %in% species_list,
    cs_is_shark = species_name %in% species_list,
    # (2.2) “Has spatiotemporal info?” — date, latitude, longitude are not NA
    has_spatiotemporal =
      !is.na(date) &
      !is.na(latitude) &
      !is.na(longitude),
    
    # (2.3) “Is wild?” — must be a shark, have spatiotemporal, and not aquarium/inland
    is_wild = sd_is_shark & has_spatiotemporal &
      (aquarium == "f" | aquarium == FALSE) &
      (inland   == "f" | inland   == FALSE),
    
    # (2.4) “Is validated?” — must be a shark, have spatiotemporal, be wild, and validated
    is_validated = cs_is_shark & has_spatiotemporal & is_wild &
      tolower(validated) %in% c("t", "true", "research")
  )

write.csv(alldat4, "./data/processed/inat_20260322.csv", row.names = FALSE)

# print("DONE PROCESSING!")
# print("Plotting Exploratory Data Analysis Summary...")
# source("./s/EDA_summary.R")




