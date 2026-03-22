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
con = connectPelagic("spr", "spr_pass")
aqua = selectData(con, "select * from aquaria")

query_gbif <- "SELECT
  ''::text                   AS common_name,
  species                    AS species_name,
  decimallatitude            AS latitude,
  decimallongitude           AS longitude,
  make_date(year, month, 1)  AS date,           -- YYYY-MM-01
  stateprovince              AS location,
  img_name,
  datasetid                  AS source,
  ''::text                   AS sd_species,
  ''::text                   AS aquarium_sp,
  TRUE                       AS validated,
  eventid::text              AS user_id,
  recordedby                 AS user_name,
  ''::text                   AS owner,
  gbifid                     AS id,
  ''::text                   AS repost,
  ''::text                   AS hashtag,
  samplingprotocol           AS description,
  TRUE                       AS shark_cs,
  species                    AS expertspecies,
  gbif_occ_url               AS url,
  'GBIF'::text               AS source_type
FROM gbif
WHERE basisofrecord = 'HUMAN_OBSERVATION'
  AND taxonomicstatus = 'ACCEPTED'
  AND occurrencestatus = 'PRESENT'         -- use ILIKE 'present' if mixed case
  AND isincluster = false;
"
alldat = dbGetQuery(con, query_gbif)

# gbif = selectData(con, "select * from gbif;")
# tx <- sharkDetectoR::get_taxonomy()
tx = dbGetQuery(con, "SELECT species_name from taxonomy3;")
species_list = tx$species_name
dbDisconnect(con)

# source("./s/EDA_aqua_inland.R")
# save(alldat3, file = "./data/raw/gbif_alldat3_20260322.RData")

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

alldat4 <- alldat3 %>%
  mutate(
    # (2.1) “Is shark?” — sd_species must be one of the shark names
    sd_is_shark = (sd_species %in% species_list) | species_name %in% species_list,
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

write.csv(alldat4, "./data/processed/gbif_20260322.csv", row.names = FALSE)

