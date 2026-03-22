SNgetEffort = function(dbuser, dbpass) {
  require(lubridate)
  require(httr)
  require(jsonlite)
  require(dplyr)
  library(stringr)
  
  con <- sharkPulseR::connectPelagic(dbuser, dbpass)
  
  
  # iNaturalist
  query3 <- "SELECT latitude, longitude, datetime as date
              FROM inat;" 
  
  inat <- dbGetQuery(con, query3)
  inat$date <- as.Date(ymd_hms(inat$date))
  inat$source_type <- "iNaturalist"
  
  
  # New Flickr -- query removes duplicates of Flickr records
  query5 <- "SELECT latitude, longitude, datetaken as date
               FROM flickr_new where shark = 'f';" 
  
  flickr_new <- dbGetQuery(con, query5)
  flickr_new$date <- as.Date(ymd_hms(flickr_new$date))
  flickr_new$source_type <- "Flickr"
  
  # GBIF
  query6 <- "SELECT decimallatitude as latitude, decimallongitude as longitude, 
              make_date(year, month, 1) AS date
               FROM gbif_effort;" 
  
  gbif <- dbGetQuery(con, query6)
  gbif$date <- as.Date(ymd_hms(gbif$date))
  gbif$source_type <- "GBIF"
  
  # Combine data from different sources into one dataframe
  dat <- rbind(inat, flickr_new, gbif)
  colnames(dat) <- c("latitude", "longitude", "date", "source_type")
  
  dbDisconnect(con)
  
  return(dat)
  
}