library(easypackages)

libraries(c("readxl", "readr", "plyr", "dplyr", "ggplot2", "png", "tidyverse", "scales", "rgdal", 'rgeos', "tmaptools", 'sp', 'sf', 'maptools', 'leaflet', 'leaflet.extras', 'spdplyr', 'geojsonio', 'rmapshaper', 'jsonlite', 'httr', 'rvest', 'stringr', 'epitools', 'XML', 'xml2', 'PostcodesioR'))

github_repo_dir <- "~/GitHub/food_landscape"
source_directory <- paste0(github_repo_dir, '/data')
output_directory <- paste0(github_repo_dir, '/outputs')

# Food standards agency API ####
route_path <- 'http://ratings.food.gov.uk/OpenDataFiles/'
xml_paths <- c('FHRS323en-GB', 'FHRS324en-GB', 'FHRS325en-GB', 'FHRS326en-GB', 'FHRS327en-GB', 'FHRS328en-GB', 'FHRS329en-GB')

for(i in 1:length(xml_paths)){
  xml_path_x <- xml_paths[i]
  assign(paste0('xmldf_', i),  
         xmlToDataFrame(nodes=getNodeSet(xmlParse(read_xml(paste0(route_path, xml_path_x, '.xml'))), "//EstablishmentDetail")) %>%
           bind_cols(xmlToDataFrame(nodes=getNodeSet(xmlParse(read_xml(paste0(route_path, xml_path_x, '.xml'))), "//EstablishmentDetail//Geocode"))))
}

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

with_lat <- food_hygeine_df %>% 
  filter(!is.na(Latitude)) %>% 
  nrow() %>% 
  as.numeric()

with_postcode <- food_hygeine_df %>% 
  filter(!is.na(Postcode)) %>% 
  nrow() %>% 
  as.numeric()

paste0('There are ', format(with_lat, big.mark = ','), ' records with a geolocation (lat/long coordinates) recorded out of ', format(nrow(food_hygeine_df), big.mark = ','), ' records overall. This is ', round((with_lat/nrow(food_hygeine_df))*100,1) ,'%. There are ', format(with_postcode, big.mark = ','), ' records with a postcode given (', round((with_postcode/nrow(food_hygeine_df))*100,1) ,'%). This means, we can try to geolocate nearly 1,000 more records using postcode lookup from postcodesio.')

fhrs_postcodes <- food_hygeine_df %>% 
  select(PostCode, Postcode) %>% 
  unique() #%>% 
  mutate(Postcode = gsub(' ' ,'', PostCode)) %>% 
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

food_hygeine_df_1 <- food_hygeine_df %>% 
  mutate(Postcode = gsub(' ', '', PostCode)) %>% 
  left_join(lookup_result_final, by = 'Postcode')
 
food_hygeine_df_1 %>% 
  filter(is.na(latitude) & is.na(lookup_latitude)) %>% 
  view()


unique(final_food_hygeine_df$BusinessType)


# read in national deprivation deciles
imd <- read_csv("https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/845345/File_7_-_All_IoD2019_Scores__Ranks__Deciles_and_Population_Denominators_3.csv", col_types = cols(`LSOA code (2011)` = col_character(), `Index of Multiple Deprivation (IMD) Score` = col_number(), `Index of Multiple Deprivation (IMD) Rank (where 1 is most deprived)` = col_number(), `Index of Multiple Deprivation (IMD) Decile (where 1 is most deprived 10% of LSOAs)` = col_integer())) %>%
  select(LSOA11CD = `LSOA code (2011)`,
         IMD_Score = `Index of Multiple Deprivation (IMD) Score`,
         IMD_Decile = `Index of Multiple Deprivation (IMD) Decile (where 1 is most deprived 10% of LSOAs)`)

chosen_outlets <- final_food_hygeine_df %>% 
  filter(BusinessType %in% c('Restaurant/Cafe/Canteen', 'Mobile caterer', 'Takeaway/sandwich shop')) %>% 
  mutate(latitude = ifelse(FHRSID == '917215', 50.7823345, ifelse(FHRSID == '1087421', 50.8354062, latitude))) %>% 
  mutate(longitude = ifelse(FHRSID== '917215', -0.6714866, ifelse(FHRSID == '1087421', -0.7738526, longitude))) %>% 
  filter(!is.na(latitude))


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

leaflet() %>% 
  addTiles() %>% 
  addCircleMarkers(data = chosen_outlets,
             lat = ~latitude,
             lng = ~longitude,
             popup = paste0(chosen_outlets$BusinessName, ' ', chosen_outlets$LocalAuthorityName),
             stroke = FALSE,
             fillOpacity = 1,
             radius = 4)

addCircleMarkers()

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
