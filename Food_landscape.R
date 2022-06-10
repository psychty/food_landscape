library(easypackages)

libraries(c("readxl", "readr", "plyr", "dplyr", "ggplot2", "png", "tidyverse", "scales", "rgdal", 'rgeos', "tmaptools", 'sp', 'sf', 'maptools', 'leaflet', 'leaflet.extras', 'spdplyr', 'geojsonio', 'rmapshaper', 'jsonlite', 'httr', 'rvest', 'stringr', 'epitools', 'XML', 'xml2'))

github_repo_dir <- "~/GitHub/food_landscape"
source_directory <- paste0(github_repo_dir, '/data')
output_directory <- paste0(github_repo_dir, '/outputs')

# Food standards agency API ####

route_path <- 'http://ratings.food.gov.uk/OpenDataFiles/'
xml_paths <- c('FHRS323en-GB', 'FHRS324en-GB', 'FHRS325en-GB', 'FHRS326en-GB', 'FHRS327en-GB', 'FHRS328en-GB', 'FHRS329en-GB')

for(i in 1:length(xml_paths)){
xml_path_x <- xml_paths[i]
assign(paste0('xmldf_', i),  xmlToDataFrame(nodes=getNodeSet(xmlParse(read_xml(paste0(route_path, xml_path_x, '.xml'))), "//EstablishmentDetail")))
}

# xmldf <- read_xml(http://ratings.food.gov.uk/OpenDataFiles/FHRS323en-GB.xml)
# xmldfparse <- xmlParse(xmldf)
# df <- xmlToDataFrame(nodes=getNodeSet(xmldfparse, "//EstablishmentDetail"))

final_food_hygeine_df <- xmldf_1 %>% 
  bind_rows(xmldf_2) %>%   
  bind_rows(xmldf_3) %>%  
  bind_rows(xmldf_4) %>%  
  bind_rows(xmldf_5) %>%   
  bind_rows(xmldf_6) %>%  
  bind_rows(xmldf_7) %>% 
  mutate(lat_position = ifelse(is.na(Geocode), NA, as.numeric(regexpr('.', substr(Geocode, 4, nchar(Geocode)),fixed = TRUE))+1)) %>% 
  mutate(longitude = ifelse(is.na(Geocode), NA, substr(Geocode, 1, (lat_position -1)))) %>% 
  mutate(latitude = ifelse(is.na(Geocode), NA, substr(Geocode, lat_position, nchar(Geocode)))) %>% 
  select(lat_position, Geocode, longitude, latitude)


unique(nchar(final_food_hygeine_df$Geocode))

# Rating areas ####
# Hygienic food handling  (Hygiene)
# Hygienic handling of food including preparation, cooking, re-heating, cooling and storage

# Cleanliness and condition of facilities and building (Structural)
# Cleanliness and condition of facilities and building (including having appropriate layout, ventilation, hand washing facilities and pest control) to enable good food hygiene

# Management of food safety (ConfidenceInManagement)
# System or checks in place to ensure that food sold or served is safe to eat, evidence that staff know about food safety, and the food safety officer has confidence that standards will be maintained in future.
# 
# getColor <- function(GPs_inside_boundary_data) {
#   sapply(GPs_inside_boundary_data$Prescribing_setting, function(Prescribing_setting) {
#     if(Prescribing_setting == "GP Practice") {
#       "green"
#     } else if(Prescribing_setting == "Prison") {
#       "orange"
#     } else {
#       "red"
#     } })
# }
# 
# FSAicons <- awesomeIcons(
#   icon = 'dot-circle-o ',
#   iconColor = 'white',
#   library = 'fa',
#   markerColor = getColor(GPs_inside_boundary_data)
# )

leaflet() %>% 
  addTiles(urlTemplate = "http://{s}.tiles.wmflabs.org/bw-mapnik/{z}/{x}/{y}.png",attribution = '&copy; <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a> <br>Contains National Statistics data ? Crown copyright and database right (2017)') %>%
  addPolygons(data = LSOAs_map, stroke = TRUE, weight = 2, color = "#1a0dab", fill = "#1a0dab", fillOpacity = 0.2, popup = paste("LSOA Name: ",LSOAs_map@data$lsoa11nm, "<br>LSOA Code: ", LSOAs_map@data$lsoa11cd, sep = ""), group = "Show LSOAs") %>% 
  addPolygons(data = DementiaSupport_boundary, stroke = TRUE, weight = 2, color = "#7a1212", fill = "#7a1212", fillOpacity = 0.2, popup = DementiaSupport_boundary@data$area, group = "Show boundary") %>%
  addLayersControl(baseGroups = c("Show LSOAs", "Show boundary"),overlayGroups = c("Show GPs"),options = layersControlOptions(collapsed = FALSE)) %>%
  addAwesomeMarkers(GPs_inside_boundary_data, lng = GPs_inside_boundary_data$long, lat = GPs_inside_boundary_data$lat, group = "Show GPs", popup = paste(GPs_inside_boundary_data$Name, sep = ""), icon=GPicons)




# 2018 fast food density by ward and ltla ####

download.file('https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/741411/FastFoodMetadata_LA_Ward.xlsx', paste0(source_directory, '/ff_density.xlsx'), mode = 'wb')

ff_density_ward <- read_excel("food_landscape/data/ff_density.xlsx", 
                         sheet = "Ward Data", 
                         skip = 2)


# e-food desert index ####

e_food_desert_index <- read_csv('https://data.cdrc.ac.uk/sites/default/files/efdi_england.csv')

# userguide = https://data.cdrc.ac.uk/sites/default/files/efdi_userguide.pdf
