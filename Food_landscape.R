library(easypackages)

libraries(c("readxl", "readr", "plyr", "dplyr", "ggplot2", "png", "tidyverse", "scales", "rgdal", 'rgeos', "tmaptools", 'sp', 'sf', 'maptools', 'leaflet', 'leaflet.extras', 'spdplyr', 'geojsonio', 'rmapshaper', 'jsonlite', 'httr', 'rvest', 'stringr', 'epitools', 'XML', 'xml2', 'PostcodesioR', 'tidytext', 'wordcloud'))

github_repo_dir <- "~/GitHub/food_landscape"
source_directory <- paste0(github_repo_dir, '/data')
output_directory <- paste0(github_repo_dir, '/outputs')

# Food standards agency API ####

# Verity kindly helped navigate the xml structure reading into R.
route_path <- 'http://ratings.food.gov.uk/OpenDataFiles/'

# Use a loop to grab the endpoints for our areas, you could probably scrape this from the site but the eng urls do not have the district names in so might be difficult. But you could certainly scrape every path on the page (all LTLAs in England and Wales, maybe more)
xml_paths <- c('FHRS323en-GB', 'FHRS324en-GB', 'FHRS325en-GB', 'FHRS326en-GB', 'FHRS327en-GB', 'FHRS328en-GB', 'FHRS329en-GB')

for(i in 1:length(xml_paths)){
  xml_path_x <- xml_paths[i]
  assign(paste0('xmldf_', i),  
         xmlToDataFrame(nodes=getNodeSet(xmlParse(read_xml(paste0(route_path, xml_path_x, '.xml'))), "//EstablishmentDetail")) %>%
           bind_cols(xmlToDataFrame(nodes=getNodeSet(xmlParse(read_xml(paste0(route_path, xml_path_x, '.xml'))), "//EstablishmentDetail//Scores"))) %>%
           bind_cols(xmlToDataFrame(nodes=getNodeSet(xmlParse(read_xml(paste0(route_path, xml_path_x, '.xml'))), "//EstablishmentDetail//Geocode")))
         )
          
  
  # assign() allows you to automate creating named objects and is great for use in loops.
  # The alternative would be to create a dummy empty dataframe and then within the loop keep adding the new data to it. However, there are 20+ fields and that would be a bit clunky 
}

# Bind the rows into a single df
food_hygeine_df <- xmldf_1 %>% 
  bind_rows(xmldf_2) %>%   
  bind_rows(xmldf_3) %>%  
  bind_rows(xmldf_4) %>%  
  bind_rows(xmldf_5) %>%   
  bind_rows(xmldf_6) %>%  
  bind_rows(xmldf_7) %>% 
  mutate(PostCode = ifelse(PostCode == 'RH11 4EL', 'RH11 0EL', ifelse(PostCode == 'RH16 8LL', 'RH17 6PA', ifelse(PostCode == 'RH20 2GH', 'RH20 2BH', ifelse(PostCode == 'PO20 2SE', 'PO20 0SE', PostCode))))) %>% 
  mutate(Latitude = as.numeric(Latitude),
         Longitude = as.numeric(Longitude)) %>% 
  mutate(Postcode = gsub(' ' ,'', PostCode)) %>% 
  select(!c(Scores, Geocode, LocalAuthorityWebSite, LocalAuthorityEmailAddress, SchemeType, RightToReply, NewRatingPending, RatingKey, LocalAuthorityCode))

# I have been a bit lazy here and hard coded that there are seven individual dataframes (this was partly so i could see how the dataframes were coming out of the FSA site).

# There are 7,800 premises here, but not all of them have geolocation information

with_lat <- food_hygeine_df %>% 
  filter(!is.na(Latitude)) %>% 
  nrow() %>% 
  as.numeric()

with_postcode <- food_hygeine_df %>% 
  filter(!is.na(Postcode)) %>% 
  nrow() %>% 
  as.numeric()

with_geo_no_postcode <- food_hygeine_df %>% 
  filter(is.na(PostCode) & !is.na(Latitude)) %>% 
  nrow() %>% 
  as.numeric()

text_1 <- paste0('There are ', format(with_lat, big.mark = ','), ' records with a geolocation (lat/long coordinates) provided out of ', format(nrow(food_hygeine_df), big.mark = ','), ' records overall. This is ', round((with_lat/nrow(food_hygeine_df))*100,1) ,'%. There are ', format(with_postcode, big.mark = ','), ' records with a postcode given (', round((with_postcode/nrow(food_hygeine_df))*100,1) ,'%). There are also ',   format(with_geo_no_postcode, big.mark = ',') , ' records with no postcode given but a geocode. As a result, we need to do some work to geolocate nearly 1,000 more records using postcode lookup from postcodesio as well as the given geolocation data from the original source.')

# Postcode lookup ####

# PostcodesioR is a wrapper function for the postcodesio api, a free, open source geolocation api for the UK. This uses the OS and ONS publicly available data. 

# It is convered by the Open Government Licence. It is updated quarterly to reflect new postcodes and terminated postcodes.

# The point is given the coordinates of the nearest delivery point to the calculated mean position of the delivery points within the postcode unit. This is the notional position of the postcode

fhrs_postcodes <- food_hygeine_df %>% 
  select(PostCode, Postcode) %>% 
  unique() %>% 
  filter(!is.na(PostCode))

lookup_result <- data.frame(postcode = character(), longitude = double(), latitude = double(), lsoa_code = character(), msoa_code = character(), msoa = character())

for(i in 1:nrow(fhrs_postcodes)){
  lookup_result_x <- postcode_lookup(fhrs_postcodes$Postcode[i]) %>% 
    select(postcode, longitude, latitude, lsoa_code, msoa_code, msoa)
  
  lookup_result <- lookup_result_x %>% 
    bind_rows(lookup_result)
}

uncaught_postcodes <- lookup_result %>% 
  filter(is.na(latitude)) %>% 
  filter(postcode != 'BN113SE')

paste0('No postcode data were available for ', nrow(uncaught_postcodes), ' postcodes. These may have been terminated postcodes.')

lookup_result <- lookup_result %>% 
  filter(!postcode %in% uncaught_postcodes$postcode) 

lookup_result_uncaught <- data.frame(postcode = character(), longitude = double(), latitude = double()) 

if(nrow(uncaught_postcodes != 0)){
  for(i in 1:nrow(uncaught_postcodes)){
    
    lookup_result_x <- terminated_postcode(uncaught_postcodes$postcode[i]) %>%
      select(postcode, longitude, latitude)
    
    lookup_result_uncaught <- lookup_result_x %>%
      bind_rows(lookup_result_uncaught)
  }
}  

lookup_result_final <- lookup_result %>% 
  bind_rows(lookup_result_uncaught) %>% 
  rename(Postcode = postcode) %>% 
  mutate(Postcode = gsub(' ', '', Postcode)) %>% 
  rename(lookup_latitude = latitude,
         lookup_longitude = longitude)

# Now we have postcode results for all the postcodes we could find in the FHRS dataset. 

# We want to make sure as many records as possible have geolocation data. In most cases there will be two values to choose (geolocation from FSA, and postcode look up). I am going to favour the geolocation supplied by FSA, where this is unavailable and a postcode lookup based geolocation is available we will use it.


# Whilst the postcodeior tool gave us lsoa data, I want to rebuild this from the GIS coordinates to be consistent. To do this we need to draw in LSOA data.

# We will assign premises to an LSOA and from there append information on deprivation (IMD), population (mid 2020), as well as MSOA, and LTLA. We should build a lookup table first.

# read in national deprivation deciles

# Verity cleverly used the select clause to rename the fields at the same time (mind blown emoji)
lsoa_lookup <- read_csv("https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/845345/File_7_-_All_IoD2019_Scores__Ranks__Deciles_and_Population_Denominators_3.csv", col_types = cols(`LSOA code (2011)` = col_character(), `Index of Multiple Deprivation (IMD) Score` = col_number(), `Index of Multiple Deprivation (IMD) Rank (where 1 is most deprived)` = col_number(), `Index of Multiple Deprivation (IMD) Decile (where 1 is most deprived 10% of LSOAs)` = col_integer())) %>%
  select(LSOA11CD = `LSOA code (2011)`,
         IMD_Score = `Index of Multiple Deprivation (IMD) Score`,
         IMD_Decile = `Index of Multiple Deprivation (IMD) Decile (where 1 is most deprived 10% of LSOAs)`,
         LTLA = `Local Authority District name (2019)`) %>% 
  filter(LTLA %in% c('Adur', 'Arun', 'Chichester', 'Crawley', 'Horsham', 'Mid Sussex', 'Worthing')) %>% 
  left_join(read_csv(paste0(output_directory, '/lsoa_mye_2020.csv')), by = 'LSOA11CD') %>% 
  left_join(read_csv(paste0(output_directory, '/lsoa_2020_pop_density.csv')), by = 'LSOA11CD')

lsoa_clipped_spdf <- geojson_read('https://opendata.arcgis.com/datasets/e9d10c36ebed4ff3865c4389c2c98827_0.geojson',  what = "sp") %>% 
  filter(LSOA11CD %in% lsoa_lookup$LSOA11CD) 

# Thanks to Verity and the traffordDatalab and some work I did on SID (buidling strings) for api build ideas 
# https://medium.com/@traffordDataLab/pushing-the-boundaries-with-the-open-geography-portal-api-4d70637bddc3

# You must escape % with a preceding %
# space_notation <- '%20'
# comma_notation <- '%27'

# You can just about get 101 records to paste together, so we only need five groups.
search_string <- gsub(' ', '', toString(sprintf("%%27%s%%20%%27", lsoa_lookup$LSOA11CD[1:101])))
query_x1<- paste0('https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/Lower_Layer_Super_Output_Areas_December_2011_Boundaries_EW_BFE_V2/FeatureServer/0/query?where=LSOA11CD%20IN%20(', search_string ,')&outFields=*&outSR=4326&f=geojson')
lsoa_1 <- st_read(query_x1)

search_string <- gsub(' ', '', toString(sprintf("%%27%s%%20%%27", lsoa_lookup$LSOA11CD[102:202])))
query_x2<- paste0('https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/Lower_Layer_Super_Output_Areas_December_2011_Boundaries_EW_BFE_V2/FeatureServer/0/query?where=LSOA11CD%20IN%20(', search_string ,')&outFields=*&outSR=4326&f=geojson')
lsoa_2 <- st_read(query_x2)

search_string <- gsub(' ', '', toString(sprintf("%%27%s%%20%%27", lsoa_lookup$LSOA11CD[203:303])))
query_x3<- paste0('https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/Lower_Layer_Super_Output_Areas_December_2011_Boundaries_EW_BFE_V2/FeatureServer/0/query?where=LSOA11CD%20IN%20(', search_string ,')&outFields=*&outSR=4326&f=geojson')
lsoa_3 <- st_read(query_x3)

search_string <- gsub(' ', '', toString(sprintf("%%27%s%%20%%27", lsoa_lookup$LSOA11CD[304:404])))
query_x4<- paste0('https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/Lower_Layer_Super_Output_Areas_December_2011_Boundaries_EW_BFE_V2/FeatureServer/0/query?where=LSOA11CD%20IN%20(', search_string ,')&outFields=*&outSR=4326&f=geojson')
lsoa_4 <- st_read(query_x4)

search_string <- gsub(' ', '', toString(sprintf("%%27%s%%20%%27", lsoa_lookup$LSOA11CD[405:505])))
query_x5<- paste0('https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/Lower_Layer_Super_Output_Areas_December_2011_Boundaries_EW_BFE_V2/FeatureServer/0/query?where=LSOA11CD%20IN%20(', search_string ,')&outFields=*&outSR=4326&f=geojson')
lsoa_5 <- st_read(query_x5)

lsoa <- rbind(lsoa_1, lsoa_2, lsoa_3, lsoa_4, lsoa_5)

# Write the sf (simple features) object to geojson, then read the geojson file using geojson_read() to returm a spatial polygons dataframe. This is the easiest type to work with that I am familiar with. Then use as required.
st_write(lsoa, paste0(output_directory, "/lsoa_fe_2011.geojson"), delete_dsn = TRUE)
lsoa_df <- geojson_read(paste0(output_directory, "/lsoa_fe_2011.geojson"), what = 'sp')
#plot(lsoa_df)

food_hygeine_df_2 <- food_hygeine_df %>% 
  left_join(lookup_result_final[c('Postcode', 'lookup_longitude', 'lookup_latitude')], by = 'Postcode') %>% 
  mutate(final_latitude = ifelse(is.na(Latitude), lookup_latitude, Latitude),
         final_longitude = ifelse(is.na(Longitude), lookup_longitude, Longitude)) %>% 
  mutate(final_latitude = ifelse(FHRSID == '917215', 50.7823345, ifelse(FHRSID == '1087421', 50.8354062, ifelse(FHRSID == '190553', 51.115741, ifelse(FHRSID == '530805', 50.8129842, ifelse(FHRSID == '1439385', 51.0051779, ifelse(FHRSID == '149410', 50.9111175, ifelse(FHRSID == '1221485', 50.859987, ifelse(FHRSID == '147919', 51.034254, ifelse(FHRSID == '190464', 51.1491572, ifelse(FHRSID == '146043', 50.8670513, ifelse(FHRSID == '509150', 50.8805853, final_latitude)))))))))))) %>% 
  mutate(final_longitude = ifelse(FHRSID== '917215', -0.6714866, ifelse(FHRSID == '1087421', -0.7738526, ifelse(FHRSID == '190553', -0.1851306, ifelse(FHRSID == '530805', -0.4834992, ifelse(FHRSID == '1439385', -0.1688246, ifelse(FHRSID == '149410', -0.7569193, ifelse(FHRSID == '1221485', -0.9274641, ifelse(FHRSID == '147919', -0.8714699, ifelse(FHRSID == '190464', -0.1479972, ifelse(FHRSID == '146043', -0.409133, ifelse(FHRSID == '509150', -0.2081717, final_longitude)))))))))))) 

original_lat <- food_hygeine_df_2 %>% 
  filter(!is.na(Latitude)) %>% 
  nrow()

final_lat <- food_hygeine_df_2 %>% 
  filter(!is.na(final_latitude)) %>% 
  nrow()

text_2 <- paste0('The original dataset had geolocation data for ', format(original_lat, big.mark = ','), ' records (', round((original_lat / nrow(food_hygeine_df_2))*100,1), '% of records. We have increased the proportion of records with geolocation data to ', round((final_lat / nrow(food_hygeine_df_2))*100,1),  ', ', format(final_lat, big.mark = ','), ' records.')

unique(food_hygeine_df_2$BusinessTypeID)

food_hygeine_la <- food_hygeine_df_2 %>% 
  filter(BusinessType %in% c('Mobile caterer', 'Takeaway/sandwich shop', 'Retailers - supermarkets/hypermarkets', 'Retailers - other', 'Restaurant/Cafe/Canteen', 'School/college/university', 'Other catering premises')) %>% 
  group_by(LocalAuthorityName, BusinessType) %>% 
  summarise(Premises = n()) %>% 
  mutate(BusinessType = factor(BusinessType, levels = c('Takeaway/sandwich shop', 'Restaurant/Cafe/Canteen', 'Mobile caterer', 'School/college/university', 'Other catering premises', 'Retailers - supermarkets/hypermarkets', 'Retailers - other'))) %>% 
  mutate(LocalAuthorityName = factor(LocalAuthorityName, levels = c('West Sussex', 'Adur', 'Arun', 'Chichester', 'Crawley', 'Horsham', 'Mid Sussex', 'Worthing'))) %>% 
  ungroup() %>% 
  arrange(LocalAuthorityName, BusinessType)

food_hygeine_utla <- food_hygeine_df_2 %>% 
  filter(BusinessType %in% c('Mobile caterer', 'Takeaway/sandwich shop', 'Retailers - supermarkets/hypermarkets', 'Retailers - other', 'Restaurant/Cafe/Canteen', 'School/college/university', 'Other catering premises')) %>% 
  mutate(BusinessType = factor(BusinessType, levels = c('Takeaway/sandwich shop', 'Restaurant/Cafe/Canteen', 'Mobile caterer', 'School/college/university', 'Other catering premises', 'Retailers - supermarkets/hypermarkets', 'Retailers - other'))) %>% 
  mutate(LocalAuthorityName = factor(LocalAuthorityName, levels = c('West Sussex', 'Adur', 'Arun', 'Chichester', 'Crawley', 'Horsham', 'Mid Sussex', 'Worthing'))) %>% 
  group_by(BusinessType) %>% 
  summarise(LocalAuthorityName = 'West Sussex',
         Premises = n()) %>% 
  ungroup()

la_outlets <- food_hygeine_la %>% 
  bind_rows(food_hygeine_utla) %>% 
  mutate(LocalAuthorityName = factor(LocalAuthorityName, levels = c('West Sussex', 'Adur', 'Arun', 'Chichester', 'Crawley', 'Horsham', 'Mid Sussex', 'Worthing'))) %>% 
  arrange(LocalAuthorityName) %>% 
  pivot_wider(names_from = 'BusinessType',
              values_from = 'Premises')

# Analyses of business names ####

selected_types_names <- food_hygeine_df_2 %>% 
  filter(BusinessType %in% c('Mobile caterer', 'Takeaway/sandwich shop', 'Retailers - supermarkets/hypermarkets', 'Retailers - other', 'Restaurant/Cafe/Canteen', 'School/college/university', 'Other catering premises')) %>% 
  select(BusinessName) 

name_parts <- selected_types_names %>% 
  unnest_tokens('Word', 'BusinessName') %>%
  anti_join(stop_words, by = c("Word" = "word")) %>% # anti_join just keeps the rows common to both data sets
  mutate(Word = str_replace(Word, "'s", ""))  # We also want to prevent the analysis in showing t and t's as two separate words

name_parts %>% 
  count(Word, sort = TRUE) %>% 
  head(15)

name_parts %>%
  count(Word) %>%
  with(wordcloud(Word, n, max.words = 500))

text_3 <- paste0('Analysing the words in business names in the FHRS may not help us to decide which additional, if any, names to include with the exceptopn that perhaps bakery, bakes, sweet, ice cream and desserts may be something to consider adding. Instead we can scrape the just eat platform which will give us business brands of food delivery places.')

url_x <- 'https://www.just-eat.co.uk/takeaway/brands/'

page <- read_html(url_x)

just_eat_brands <- page %>% 
  html_nodes('.name') %>% 
  html_text()

just_eat_brands

text_4 <- paste0('What does this tell us about anything? Are we missing anything from the nine key words that represent how fast food has changed? Maybe there is a greater ')


food_hygeine_df_2 %>% 
  filter(.$BusinessName %in% just_eat_brands) %>% 
  view()




flagged_outlets_df <- food_hygeine_df_2 %>% 
  filter(!BusinessType %in% c("Distributors/Transporters",
                            "Farmers/growers",
                            "Hospitals/Childcare/Caring Premises",
                            "Hotel/bed & breakfast/guest house",
                            "Importers/Exporters",
                            "Manufacturers/packers",
                            "Pub/bar/nightclub")) %>%
  mutate(takeaway_flag = ifelse(BusinessType == "Takeaway/sandwich shop", TRUE, FALSE),
         mobile_caterer_flag = ifelse(grepl("burger|chicken|chip|fish bar|pizza|kebab|india|china|chinese", BusinessName, ignore.case = TRUE) == TRUE & BusinessType == "Mobile caterer", TRUE, FALSE),
         other_caterer_flag = ifelse(grepl("burger|chicken|chip|fish bar|pizza|kebab|india|china|chinese|mcdonald|burger king|greggs|subway|costa|kfc|pret|starbucks", BusinessName, ignore.case = TRUE) == TRUE & BusinessType %in% c("Other catering premises", "Restaurant/Cafe/Canteen"), TRUE, FALSE),
         chains_flag = ifelse(grepl("mcdonald|burger king|greggs|subway|costa|kfc|pret|starbucks", BusinessName, ignore.case = TRUE) == TRUE & BusinessType %in% c("Retailers - other","Retailers - supermarkets/hypermarkets", "School/college/university"), TRUE, FALSE)) %>%
  filter(takeaway_flag == TRUE | mobile_caterer_flag == TRUE |other_caterer_flag == TRUE | chains_flag == TRUE)

text_4 <- paste0('There are ', format(nrow(flagged_outlets_df), big.mark = ','), " outlets that have been identified as 'fast food' from all available records.") 

flagged_outlets_la <- flagged_outlets_df %>% 
  mutate(LocalAuthorityName = factor(LocalAuthorityName, levels = c('West Sussex', 'Adur', 'Arun', 'Chichester', 'Crawley', 'Horsham', 'Mid Sussex', 'Worthing'))) %>% 
  group_by(LocalAuthorityName) %>% 
  summarise(`Flagged as 'fast food'` = n())

flagged_outlets_utla <- flagged_outlets_df %>% 
  summarise(`Flagged as 'fast food'` = n(),
            LocalAuthorityName = 'West Sussex')

flagged_outlets_table <- flagged_outlets_la %>% 
  bind_rows(flagged_outlets_utla)

la_mye <- lsoa_lookup %>% 
  group_by(LTLA) %>% 
  summarise(Population = sum(Population, na.rm = TRUE)) %>% 
  ungroup() 

utla_mye <- lsoa_lookup %>% 
  summarise(LTLA = 'West Sussex', 
            Population = sum(Population, na.rm = TRUE))

area_pop <- la_mye %>% 
  bind_rows(utla_mye)

summary_table <- la_outlets %>% 
  left_join(flagged_outlets_table, by = 'LocalAuthorityName') %>% 
  left_join(area_pop, by = c('LocalAuthorityName' = 'LTLA')) %>% 
  rename(Area = LocalAuthorityName)

summary_table %>% 
  toJSON() %>% 
  write_lines(paste0(output_directory, '/outlet_table_ltla'))

flagged_outlets_df_final <- flagged_outlets_df %>% 
  select(FHRSID, LocalAuthorityName, LocalAuthorityBusinessID, BusinessName, AddressLine1, AddressLine2, Postcode, BusinessType, BusinessTypeID, RatingDate, longitude = final_longitude, latitude = final_latitude) %>% # See I can do the rename in select
  filter(!is.na(latitude)) 

business_fct <- factor(unique(flagged_outlets_df_final$BusinessType))
business_colours <- colorFactor(viridis_pal(option = "C", direction = -1)(length(unique(flagged_outlets_df_final$BusinessType))), domain = business_fct)

# TODO filter appropriate business types

# Turn the dataframe into a spatial points dataframe
xy <- flagged_outlets_df_final %>% 
  select(longitude, latitude)

outlets_spdf <- SpatialPointsDataFrame(
  coords = xy, 
  data = flagged_outlets_df_final,
  proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

plot(outlets_spdf)

outlets_lookup <- outlets_spdf %>% 
  as.data.frame() %>% 
  bind_cols(over(outlets_spdf, lsoa_df)) %>% 
  select(FHRSID, LSOA11CD) 

outlets_spdf <- outlets_spdf %>% 
  left_join(outlets_lookup, by = 'FHRSID') %>% 
  filter(!is.na(LSOA11CD))

plot(outlets_spdf)

lsoa_summary <- outlets_spdf %>% 
  as.data.frame() %>% 
  group_by(LSOA11CD) %>% 
  summarise(number_premises = n())

lsoa_df_final <- lsoa_df %>% 
  left_join(lsoa_summary, by = 'LSOA11CD')

text_3 <- paste0('There are ', format(nrow(outlets_spdf), big.mark = ','), ' filtered records with details of a valid geolocation in West Sussex county.')

leaflet(lsoa_df_final) %>% 
  addTiles() %>% 
  addPolygons(stroke = TRUE,
              color = 'maroon',
              weight = 3,
              smoothFactor = 0.3, 
              fillOpacity = 0) %>% 
  addCircleMarkers(data = outlets_spdf,
                   popup = paste0(outlets_spdf$BusinessName, ' ', outlets_spdf$LocalAuthorityName, '<br><br>', outlets_spdf$BusinessType),
                   stroke = FALSE,
                   fillOpacity = 1,
                   radius = 6,
                   color = ~business_colours(BusinessType))%>%
  addScaleBar(position = "bottomleft") %>%
  addLegend(position = 'bottomright',
            pal = business_colours,
            values = business_fct,
            title = 'Food businesses:',
            opacity = 1)

# tidy up environment
rm(xmldf_1, xmldf_2, xmldf_3, xmldf_4, xmldf_5, xmldf_6, xmldf_7, lookup_result, lookup_result_x, lookup_result_uncaught, fhrs_postcodes, lsoa_1, lsoa_2, lsoa_3, lsoa_4, lsoa_5, lsoa, food_hygeine_df, food_hygeine_df_2)

# export ####

geojson_write(geojson_json(outlets_spdf), file = paste0(output_directory, '/fhrs_west_sussex.geojson'))

geojson_write(ms_simplify(geojson_json(lsoa_df_final), keep = .2), file = paste0(output_directory, '/fhrs_west_sussex_lsoa_summary.geojson'))

# # 2018 fast food density by ward and ltla ####
# 
# download.file('https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/741411/FastFoodMetadata_LA_Ward.xlsx', paste0(source_directory, '/ff_density.xlsx'), mode = 'wb')
# 
# ff_density_ward <- read_excel("food_landscape/data/ff_density.xlsx", 
#                          sheet = "Ward Data", 
#                          skip = 2)


# e-food desert index ####

e_food_desert_index <- read_csv('https://data.cdrc.ac.uk/sites/default/files/efdi_england.csv')

# userguide = https://data.cdrc.ac.uk/sites/default/files/efdi_userguide.pdf


# Adult Food Insecurity Index

# https://drive.google.com/file/d/1_arVrQ9Y3t_26E28888SBv7QH5Aax2Zs/view


# food banks ####

# https://www.trusselltrust.org/get-help/find-a-foodbank/

# https://www.foodaidnetwork.org.uk/our-members
# https://www.google.com/maps/d/u/0/viewer?mid=15mnlXFpd8-x0j4O6Ck6U90chPn4bkbWz&ll=54.0948970099395%2C-2.757892499999981&z=6

# Food insecurity lsoa ####

food_insecurity_df <- read_csv('~/GitHub/food_landscape/raw_data/Food Insecurity Risk Indices Simple Sussex.csv') %>% 
  left_join(read_csv('~/GitHub/food_landscape/raw_data/Food Insecurity Risk Indices Complex Sussex.csv'), by = 'LSOA11CD') %>% 
  left_join(read_csv('~/GitHub/food_landscape/raw_data/Food Insecurity Risk Indices Structural Sussex.csv'), by = 'LSOA11CD') %>% 
  left_join(read_csv('~/GitHub/food_landscape/raw_data/Food Insecurity Risk Indices Composition Sussex.csv'), by = 'LSOA11CD')


# The University of Southampton has updated its local food insecurity risk index with the latest published data at a smaller geographical scale than previously available. This new multi-dimensional index estimates the relative rank of food insecurity risk across local neighbourhoods in England. All data are open-source. Maps and data by Local Authority are available for download at https://www.mylocalmap.org.uk/iaahealth/.

# The variables selected for these updated indices were informed by interviews with stakeholders in local governments and food aid, We assessed four approaches to estimating risk. The Simple Index illustrates food poverty risk based on benefits claimants and low-income at a household level and is well suited to estimating risk in urban areas. The Complex index incorporates two domains Compositional (50%) Structural (50%). ‘Structural’ sources of food insecurity risk are particularly impactful in rural areas and ‘Compositional’ sources include direct and indirect economic obstacles, reflecting population characteristics within small areas. Using several indicators in the complex index reduces the bias that may be produced from considering only the economic dimension of food insecurity risk. The two domains were assessed independently and then combined to create a food insecurity risk measure for each LSOA, ranked relative to all other LSOAs. 

# Results were assessed for all of England and then by area classification as urban or rural to reflect differing barriers to food security in these settings. The Simple index is correlated well with the IMD 2019 (rs=0.872). 

# Final maps were shared with stakeholders across several local authorities in Wessex and the North of England to compare with food aid uptake and observations of demand, confirming that the Compositional domain provided an accurate estimate of risk in urban areas, with rural areas gaining further insight from the Structural domain. The importance of including data (such as mental health and qualifications) beyond immediate economic circumstances (Simple index) were highlighted in our interviews, which is reflected in the Compositional domain. The Complex index reveals that most areas are not in the highest decile of risk for both composition and Structural domains, however, poor access creates a double burden for households with economic disadvantage in rural locations.

# Rural areas should use both the composition and structural domains side-by-side to assess local issues of rural access. As a relative measure, the ranking of an LSOA is not an absolute measure of household risk but can be used to compare against the ranked conditions found in other LSOAs, particularly useful for illustrating areas at higher risk. 

# Although the index is measured at the neighbourhood level, this area rank may not reflect the situation of every resident living in an LSOA. 

# The updated food insecurity risk measures can be used to estimate household risk in small areas for England, for the purposes of prioritising interventions to address food insecurity, such as food pantries, holiday hunger activities (HAF) and in JSNAs.

# - For most areas, the compositional domain reflects risk well and is the preferred option. 
# - In rural areas the structural domain can be explored alongside the compositional measure to understand additional barriers to food security.

# Source: Multiple sources: DWP, Census
# Licence: Open
# About Dataset: See metadata in About/FAQ and cite as Smith et al., 2021. In all cases, 1 is the lowest risk. If you downloaded Structural Risk data between 1-15 Oct 2021, this has been updated. Please download a new copy.
# About Column: Simple Risk of Food Insecurity