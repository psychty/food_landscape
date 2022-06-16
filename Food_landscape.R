library(easypackages)

libraries(c("readxl", "readr", "plyr", "dplyr", "ggplot2", "png", "tidyverse", "scales", "rgdal", 'rgeos', "tmaptools", 'sp', 'sf', 'maptools', 'leaflet', 'leaflet.extras', 'spdplyr', 'geojsonio', 'rmapshaper', 'jsonlite', 'httr', 'rvest', 'stringr', 'epitools', 'XML', 'xml2', 'PostcodesioR'))

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
           bind_cols(xmlToDataFrame(nodes=getNodeSet(xmlParse(read_xml(paste0(route_path, xml_path_x, '.xml'))), "//EstablishmentDetail//Geocode"
))))
  
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
  mutate(Postcode = gsub(' ' ,'', PostCode))

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

paste0('There are ', format(with_lat, big.mark = ','), ' records with a geolocation (lat/long coordinates) provided out of ', format(nrow(food_hygeine_df), big.mark = ','), ' records overall. This is ', round((with_lat/nrow(food_hygeine_df))*100,1) ,'%. There are ', format(with_postcode, big.mark = ','), ' records with a postcode given (', round((with_postcode/nrow(food_hygeine_df))*100,1) ,'%). There are also ',   format(with_geo_no_postcode, big.mark = ',') , ' records with no postcode given but a geocode. As a result, we need to do some work to geolocate nearly 1,000 more records using postcode lookup from postcodesio as well as the given geolocation data from the original source.')


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
  filter(LTLA %in% c('Adur', 'Arun', 'Chichester', 'Crawley', 'Horsham', 'Mid Sussex', 'Worthing')) 

lsoa_clipped_spdf <- geojson_read('https://opendata.arcgis.com/datasets/e9d10c36ebed4ff3865c4389c2c98827_0.geojson',  what = "sp") %>% 
  filter(LSOA11CD %in% lsoa_lookup$LSOA11CD) 


# oa_lookup <- GET('https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/OA11_LSOA11_MSOA11_LAD_GB_LU_classificationv2/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson')

#https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/Lower_Layer_Super_Output_Areas_December_2011_Boundaries_EW_BFE_V2/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson


query <- "https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/Lower_Layer_Super_Output_Areas_December_2011_Boundaries_EW_BFE_V2/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson"


# Thanks to Verity and the traffordDatalab and some work I did on SID (buidling strings) for api build ideas 
# https://medium.com/@traffordDataLab/pushing-the-boundaries-with-the-open-geography-portal-api-4d70637bddc3

# You must escape % with a preceding %
# space_notation <- '%20'
# comma_notation <- '%27'

# You can just about get 101 records to paste together, so we only need five groups.
search_string <- gsub(' ', '', toString(sprintf("%%27%s%%20%%27", sample(lsoa_lookup$LSOA11CD, 101))))

query_x1<- paste0('https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/Lower_Layer_Super_Output_Areas_December_2011_Boundaries_EW_BFE_V2/FeatureServer/0/query?where=LSOA11CD%20IN%20(', search_string ,')&outFields=*&outSR=4326&f=geojson')

# Write the sf (simple features) object to geojson, then read the geojson file using geojson_read() to returm a spatial polygons dataframe. This is the easiest type to work with that I am familiar with. Then use as required.
st_write(st_read(query_x1), paste0(output_directory, "/lsoa_fe_2011.geojson"), delete_dsn = TRUE)
lsoa_df <- geojson_read(paste0(output_directory, "/lsoa_fe_2011.geojson"), what = 'sp')
plot(lsoa_df)









food_hygeine_df_final <- food_hygeine_df %>% 
  left_join(lookup_result_final[c('Postcode', 'lookup_longitude', 'lookup_latitude')], by = 'Postcode') %>% 
  mutate(final_latitude = ifelse(is.na(Latitude), lookup_latitude, Latitude),
         final_longitude = ifelse(is.na(Longitude), lookup_longitude, Longitude)) %>% 
  mutate(final_latitude = ifelse(FHRSID == '917215', 50.7823345, ifelse(FHRSID == '1087421', 50.8354062, ifelse(FHRSID == '190553', 51.115741, ifelse(FHRSID == '530805', 50.8129842, ifelse(FHRSID == '1439385', 51.0051779, final_latitude)))))) %>% 
  mutate(final_longitude = ifelse(FHRSID== '917215', -0.6714866, ifelse(FHRSID == '1087421', -0.7738526, ifelse(FHRSID == '190553', -0.1851306, ifelse(FHRSID == '530805', -0.4834992, ifelse(FHRSID == '1439385', -0.1688246, final_longitude))))))

original_lat <- food_hygeine_df_final %>% 
  filter(!is.na(Latitude)) %>% 
  nrow()

final_lat <- food_hygeine_df_final %>% 
  filter(!is.na(final_latitude)) %>% 
  nrow()

paste0('The original dataset had geolocation data for ', format(original_lat, big.mark = ','), ' records (', round((original_lat / nrow(food_hygeine_df_final))*100,1), '% of records. We have increased the proportion of records with geolocation data to ', round((final_lat / nrow(food_hygeine_df_final))*100,1),  ', ', format(final_lat, big.mark = ','), ' records.')


leaflet(lsoa_df) %>% 
  addTiles() %>% 
  addPolygons(stroke = TRUE,
              color = 'purple',
              weight = 3,
              smoothFactor = 0.3, 
              fillOpacity = 0) %>% 
  addCircleMarkers(data = food_hygeine_df_final,
                   lat = ~final_latitude,
                   lng = ~final_longitude,
                   popup = paste0(food_hygeine_df_final$BusinessName, ' ', food_hygeine_df_final$LocalAuthorityName, '<br><br>', food_hygeine_df_final$FHRSID),
                   stroke = FALSE,
                   fillOpacity = 1,
                   radius = 4)

food_hygeine_df_final %>% 
  view()


plot(lsoa_clipped_spdf)


# tidy up environment
rm(xmldf_1, xmldf_2, xmldf_3, xmldf_4, xmldf_5, xmldf_6, xmldf_7, lookup_result, lookup_result_x, lookup_result_uncaught, fhrs_postcodes)



chosen_outlets <- final_food_hygeine_df %>% 
  filter(BusinessType %in% c('Restaurant/Cafe/Canteen', 'Mobile caterer', 'Takeaway/sandwich shop')) %>% 
 


### Get long and lat from your data.frame. Make sure that the order is in lon/lat.

xy <- chosen_outlets %>% 
  select(longitude, latitude)

chosen_outlets_spdf <- SpatialPointsDataFrame(coords = xy, 
                                              data = chosen_outlets,
                                              proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

geojson_write(geojson_json(chosen_outlets_spdf), file = paste0(output_directory, '/fhrs_west_sussex.geojson'))

# Rating areas ####
# Hygienic food handling  (Hygiene)
# Hygienic handling of food including preparation, cooking, re-heating, cooling and storage

# Cleanliness and condition of facilities and building (Structural)
# Cleanliness and condition of facilities and building (including having appropriate layout, ventilation, hand washing facilities and pest control) to enable good food hygiene

# Management of food safety (ConfidenceInManagement)
# System or checks in place to ensure that food sold or served is safe to eat, evidence that staff know about food safety, and the food safety officer has confidence that standards will be maintained in future.
# 


 # addPolygons(data = LSOAs_map, stroke = TRUE, weight = 2, color = "#1a0dab", fill = "#1a0dab", fillOpacity = 0.2, popup = paste("LSOA Name: ",LSOAs_map@data$lsoa11nm, "<br>LSOA Code: ", LSOAs_map@data$lsoa11cd, sep = ""), group = "Show LSOAs") %>% 
  # addLayersControl(baseGroups = c("Show LSOAs", "Show boundary"),overlayGroups = c("Show GPs"),options = layersControlOptions(collapsed = FALSE)) %>%
  # addAwesomeMarkers(GPs_inside_boundary_data, lng = GPs_inside_boundary_data$long, lat = GPs_inside_boundary_data$lat, group = "Show GPs", popup = paste(GPs_inside_boundary_data$Name, sep = ""), icon=GPicons)




# 2018 fast food density by ward and ltla ####

download.file('https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/741411/FastFoodMetadata_LA_Ward.xlsx', paste0(source_directory, '/ff_density.xlsx'), mode = 'wb')

ff_density_ward <- read_excel("food_landscape/data/ff_density.xlsx", 
                         sheet = "Ward Data", 
                         skip = 2)


# e-food desert index ####

e_food_desert_index <- read_csv('https://data.cdrc.ac.uk/sites/default/files/efdi_england.csv')

# userguide = https://data.cdrc.ac.uk/sites/default/files/efdi_userguide.pdf


# Adult Food Insecurity Index


https://drive.google.com/file/d/1_arVrQ9Y3t_26E28888SBv7QH5Aax2Zs/view
