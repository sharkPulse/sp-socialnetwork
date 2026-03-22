SNgetSharkPulse = function(dbuser, dbpass) {
  require(lubridate)
  require(httr)
  require(jsonlite)
  require(dplyr)
  library(stringr)
  
    con <- connectPelagic(dbuser, dbpass)
    
    # Sharkpulse
    query1 <- "SELECT common_name, species_name, latitude, longitude, date, location, img_name, source, 
              species_name as sd_species, '' as aquarium_sp, 't' as validated, NULL as user_id, user_name,
              '' as owner, NULL as id, '' as repost, '' as hashtag, notes as description, '' as shark_cs, '' as expertspecies, weblink as url,
              source_type   
              FROM sharkpulse;"
    sharkpulse <- dbGetQuery(con, query1)

    # iNaturalist
    query3 <- "SELECT common_name, scientific_name AS species_name, datetime AS date, latitude, longitude, place_guess AS location, img_name, 'iNaturalist' AS source, 
              scientific_name as sd_species, '' as aquarium_sp, quality_grade as validated, user_id, user_name, '' as owner, id, '' as repost, '' as hashtag,
              tag_list as description, '' as shark_cs, '' as expertspecies, url
              FROM inat;" 
               
    inat <- dbGetQuery(con, query3)
    inat$date <- as.Date(ymd_hms(inat$date))
    inat$source_type <- "iNaturalist"
    
    # Instagram
    query4 <- "SELECT common_name, species_name, latitude, longitude, location, img_name, date, 'Instagram' AS source, 
              sd_species, aquarium as aquarium_sp, validated, NULL as user_id, '' as user_name, '' as owner, NULL as id, 
              repost, hashtag, text as description, '' as shark_cs, expertspecies, post_url as url 
              FROM instagram;" 

    instagram <- dbGetQuery(con, query4)
    instagram$source_type <- "Instagram"
    
    # New Flickr -- query removes duplicates of Flickr records
    query5 <- "SELECT common_name_cs AS common_name, species_name_cs AS species_name, datetaken AS date, latitude, longitude, '' AS location, img_name, 'Flickr' AS source, 
               species_name_1 as sd_species, aquarium as aquarium_sp, validated, NULL as user_id, '' as user_name, owner, id, '' as repost,
               '' as hashtag, title as description, shark_cs, expertspecies, flickr_url as url 
               FROM flickr_new;" 

    flickr_new <- dbGetQuery(con, query5)
    flickr_new$date <- as.Date(ymd_hms(flickr_new$date))
    flickr_new$source_type <- "Flickr"
    
    # Combine data from different sources into one dataframe
    dat <- rbind(sharkpulse, flickr_new, inat, instagram)
    colnames(dat) <- c("common_name", "species_name", "latitude", "longitude", "date", "location", "img_name", "source", "sd_species", "aquarium_sp", "validated", 
    "user_id", "user_name", "owner", "id", "repost", "hashtag", "description", "shark_cs", "expertspecies", "url", "source_type")
    
    dbDisconnect(con)
    
    ## To remove duplicate records
    ## however, need to search for exact image path because some img_name do not have file extension 
    

  dat <- dat %>%
    # 1) create a temp column that always has “.jpg”
    mutate(
      tmp_name = if_else(
        str_detect(img_name, "\\.jpg$"),
        img_name,
        paste0(img_name, ".jpg")
      )
    ) %>%
    # 2) dedupe on that
    distinct(tmp_name, .keep_all = TRUE) %>%
    # 3) strip “.jpg” off tmp_name to restore original img_name
    mutate(
      img_name = str_remove(tmp_name, "\\.jpg$")
    ) %>%
    # 4) drop the temp column
    select(-tmp_name)

    
    return(dat)

}