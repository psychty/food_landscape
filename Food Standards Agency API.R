

# Food standards agency #

# http://ratings.food.gov.uk/open-data/en-GB

# install.packages('rjson')
library(rjson); library(XML);library(plyr);library(tmap);library(leaflet);library(rgdal);library(tmaptools);library(raster);library(mapdata);library(rio);library(scales);library(urltools);library(htmlwidgets)

# Ideal way (XML straight to R) ####

# Download the XML file for Adur
Adur <- xmlToList("http://ratings.food.gov.uk/OpenDataFiles/FHRS323en-GB.xml")
# The list is a set of lists within lists (nested) and we want to strip out the top layer out 
Adur <- Adur$EstablishmentCollection

# Some of the observations do not have values for each field. To dandle this we can create a dataframe with all of the possible fields and then use rbind.fill t fill in the blanks where possible
Adur_df <- data.frame(FHRSID = as.character(),	LocalAuthorityBusinessID = as.character(),	BusinessName = as.character(),	BusinessType = as.character(),	BusinessTypeID = as.character(),	AddressLine1 = as.character(),	AddressLine2 = as.character(),	AddressLine3 = as.character(),	PostCode = as.character(),	RatingValue = as.numeric(),	RatingKey = as.character(),	RatingDate = as.character(),	LocalAuthorityCode  = as.character(),	LocalAuthorityName = as.character(),	LocalAuthorityWebSite = as.character(),	LocalAuthorityEmailAddress = as.character(),	Scores.Hygiene = as.numeric(),	Scores.Structural = as.numeric(),	Scores.ConfidenceInManagement = as.character(),	SchemeType = as.character(),	NewRatingPending = as.character(),	Geocode.Longitude = as.character(),	Geocode.Latitude = as.character(),	AddressLine4 = as.character())


# This goes through each element of the list and adds it to the dataframe we just created, getting rid of the beginning part of the colname
for(i in 1:length(Adur)){
est <- as.data.frame(Adur[[i]], check.names = FALSE)
colnames(est) <- gsub("EstablishmentDetail.", "", colnames(est))
Adur_df <- rbind.fill(Adur_df, est)
}

assign(paste("est_", i, sep=""), Adur[i])

for(i in 18:length(Adur)){
  est <- as.data.frame(Adur[[i]], check.names = FALSE)
  colnames(est) <- gsub("EstablishmentDetail.", "", colnames(est))
  Adur_df <- rbind.fill(Adur_df, est)
}

# At work way ####
getwd()
download.file("http://ratings.food.gov.uk/OpenDataFiles/FHRS323en-GB.xml", "./Food Standards Agency/Adur_FSA.xml")
download.file("http://ratings.food.gov.uk/OpenDataFiles/FHRS324en-GB.xml", "./Food Standards Agency/Arun_FSA.xml")
download.file("http://ratings.food.gov.uk/OpenDataFiles/FHRS325en-GB.xml", "./Food Standards Agency/Chichester_FSA.xml")
download.file("http://ratings.food.gov.uk/OpenDataFiles/FHRS326en-GB.xml", "./Food Standards Agency/Crawley_FSA.xml")
download.file("http://ratings.food.gov.uk/OpenDataFiles/FHRS327en-GB.xml", "./Food Standards Agency/Horsham_FSA.xml")
download.file("http://ratings.food.gov.uk/OpenDataFiles/FHRS328en-GB.xml", "./Food Standards Agency/Mid_Sussex_FSA.xml")
download.file("http://ratings.food.gov.uk/OpenDataFiles/FHRS329en-GB.xml", "./Food Standards Agency/Worthing_FSA.xml")

# Then open Excel and run the macro 'Extract FSA data' VBA given below

# Sub Extract_FSA_data()
# '
# ' Extract_FSA_data Macro
# '
# 
# '
# Application.DisplayAlerts = False
# ActiveWorkbook.XmlImport URL:= _
# "\\typhon\groups2.bu\Public Health Directorate\PH Research Unit\R\Food Standards Agency\Adur_FSA.xml" _
# , ImportMap:=Nothing, Overwrite:=True, Destination:=Range("$A$1")
# ActiveWorkbook.SaveAs Filename:= _
# "\\typhon\groups2.bu\Public Health Directorate\PH Research Unit\R\Food Standards Agency\Adur_FSA.csv" _
# , FileFormat:=xlCSV, CreateBackup:=False
# Sheets.add After:=Sheets(Sheets.Count)
# Sheets("Adur_FSA").Select
# ActiveWindow.SelectedSheets.Delete
# 
# ActiveWorkbook.XmlImport URL:= _
# "\\typhon\groups2.bu\Public Health Directorate\PH Research Unit\R\Food Standards Agency\Arun_FSA.xml" _
# , ImportMap:=Nothing, Overwrite:=True, Destination:=Range("$A$1")
# ActiveWorkbook.SaveAs Filename:= _
# "\\typhon\groups2.bu\Public Health Directorate\PH Research Unit\R\Food Standards Agency\Arun_FSA.csv" _
# , FileFormat:=xlCSV, CreateBackup:=False
# Sheets.add After:=Sheets(Sheets.Count)
# Sheets("Arun_FSA").Select
# ActiveWindow.SelectedSheets.Delete
# 
# ActiveWorkbook.XmlImport URL:= _
# "\\typhon\groups2.bu\Public Health Directorate\PH Research Unit\R\Food Standards Agency\Chichester_FSA.xml" _
# , ImportMap:=Nothing, Overwrite:=True, Destination:=Range("$A$1")
# ActiveWorkbook.SaveAs Filename:= _
# "\\typhon\groups2.bu\Public Health Directorate\PH Research Unit\R\Food Standards Agency\Chichester_FSA.csv" _
# , FileFormat:=xlCSV, CreateBackup:=False
# Sheets.add After:=Sheets(Sheets.Count)
# Sheets("Chichester_FSA").Select
# ActiveWindow.SelectedSheets.Delete
# 
# ActiveWorkbook.XmlImport URL:= _
# "\\typhon\groups2.bu\Public Health Directorate\PH Research Unit\R\Food Standards Agency\Crawley_FSA.xml" _
# , ImportMap:=Nothing, Overwrite:=True, Destination:=Range("$A$1")
# ActiveWorkbook.SaveAs Filename:= _
# "\\typhon\groups2.bu\Public Health Directorate\PH Research Unit\R\Food Standards Agency\Crawley_FSA.csv" _
# , FileFormat:=xlCSV, CreateBackup:=False
# Sheets.add After:=Sheets(Sheets.Count)
# Sheets("Crawley_FSA").Select
# ActiveWindow.SelectedSheets.Delete
# 
# ActiveWorkbook.XmlImport URL:= _
# "\\typhon\groups2.bu\Public Health Directorate\PH Research Unit\R\Food Standards Agency\Horsham_FSA.xml" _
# , ImportMap:=Nothing, Overwrite:=True, Destination:=Range("$A$1")
# ActiveWorkbook.SaveAs Filename:= _
# "\\typhon\groups2.bu\Public Health Directorate\PH Research Unit\R\Food Standards Agency\Horsham_FSA.csv" _
# , FileFormat:=xlCSV, CreateBackup:=False
# Sheets.add After:=Sheets(Sheets.Count)
# Sheets("Horsham_FSA").Select
# ActiveWindow.SelectedSheets.Delete
# 
# ActiveWorkbook.XmlImport URL:= _
# "\\typhon\groups2.bu\Public Health Directorate\PH Research Unit\R\Food Standards Agency\Mid_Sussex_FSA.xml" _
# , ImportMap:=Nothing, Overwrite:=True, Destination:=Range("$A$1")
# ActiveWorkbook.SaveAs Filename:= _
# "\\typhon\groups2.bu\Public Health Directorate\PH Research Unit\R\Food Standards Agency\Mid_Sussex_FSA.csv" _
# , FileFormat:=xlCSV, CreateBackup:=False
# Sheets.add After:=Sheets(Sheets.Count)
# Sheets("Mid_Sussex_FSA").Select
# ActiveWindow.SelectedSheets.Delete
# 
# ActiveWorkbook.XmlImport URL:= _
# "\\typhon\groups2.bu\Public Health Directorate\PH Research Unit\R\Food Standards Agency\Worthing_FSA.xml" _
# , ImportMap:=Nothing, Overwrite:=True, Destination:=Range("$A$1")
# ActiveWorkbook.SaveAs Filename:= _
# "\\typhon\groups2.bu\Public Health Directorate\PH Research Unit\R\Food Standards Agency\Worthing_FSA.csv" _
# , FileFormat:=xlCSV, CreateBackup:=False
# Sheets.add After:=Sheets(Sheets.Count)
# Sheets("Worthing_FSA").Select
# ActiveWindow.SelectedSheets.Delete
# Application.DisplayAlerts = True
# End Sub

Adur_FSA <- read.csv("./Food Standards Agency/Adur_FSA.csv", header = TRUE, stringsAsFactors = FALSE)
Arun_FSA <- read.csv("./Food Standards Agency/Arun_FSA.csv", header = TRUE, stringsAsFactors = FALSE)
Chichester_FSA <- read.csv("./Food Standards Agency/Chichester_FSA.csv", header = TRUE, stringsAsFactors = FALSE)
Crawley_FSA <- read.csv("./Food Standards Agency/Crawley_FSA.csv", header = TRUE, stringsAsFactors = FALSE)
Horsham_FSA <- read.csv("./Food Standards Agency/Horsham_FSA.csv", header = TRUE, stringsAsFactors = FALSE)
Mid_Sussex_FSA <- read.csv("./Food Standards Agency/Mid_Sussex_FSA.csv", header = TRUE, stringsAsFactors = FALSE)
Worthing_FSA <- read.csv("./Food Standards Agency/Worthing_FSA.csv", header = TRUE, stringsAsFactors = FALSE)

FSA_data <- rbind.fill(Adur_FSA, Arun_FSA, Chichester_FSA, Crawley_FSA, Horsham_FSA, Mid_Sussex_FSA,Worthing_FSA)

rm(Adur_FSA, Arun_FSA, Chichester_FSA, Crawley_FSA, Horsham_FSA, Mid_Sussex_FSA,Worthing_FSA)

FSA_data$ItemCount <- NULL
FSA_data$ReturnCode <- NULL
FSA_data$SchemeType <- NULL

FSA_data$BusinessType <- factor(FSA_data$BusinessType)

FSA_data$RatingName <- ifelse(FSA_data$RatingValue == 0 , "Urgent Improvement Necessary", ifelse(FSA_data$RatingValue == 1, "Major Improvement Necessary", ifelse(FSA_data$RatingValue == 2, "Improvement Necessary", ifelse(FSA_data$RatingValue == 3, "Generally Satisfactory", ifelse(FSA_data$RatingValue == 4, "Good", ifelse(FSA_data$RatingValue == 5, "Very Good", ifelse(FSA_data$RatingValue == "Exempt", "Exempt", ifelse(FSA_data$RatingValue == "AwaitingInspection", "Awaiting Inspection", FSA_data$RatingValue))))))))

FSA_data$RatingName <- factor(FSA_data$RatingName, levels = c("Exempt", "Awaiting Inspection", "Urgent Improvement Necessary", "Major Improvement Necessary", "Improvement Necessary", "Generally Satisfactory", "Good", "Very Good"))

# Two groups of business where you or your family might eat or buy food are not given food hygiene ratings - these are 'exempt'. These 'exempt' businesses are inspected by a local authority food safety officer but they are not given a rating.

# The two groups of exempt businesses are:   businesses that are a low-risk to people's health in terms of food safety and that you perhaps wouldn't normally think of as a food business - for example, newsagents, chemist shops or visitor centres selling tins of biscuits childminders and businesses that offer caring services at home

Mobile_caterers <- subset(FSA_data, BusinessType == "Mobile caterer")
View(Mobile_caterers)

Geocoded_FSA_data <- subset(FSA_data, !(is.na(Longitude)))
nrow(Geocoded_FSA_data) /nrow(FSA_data) * 100

paste("Geolocation data is available for ", format(nrow(Geocoded_FSA_data),big.mark = ","), " businesses (", round(nrow(Geocoded_FSA_data) /nrow(FSA_data) * 100,1), "% of all businesses in West Sussex).", sep = "")

Rated_businesses <- subset(FSA_data, !(RatingName %in% c("Exempt", "Awaiting Inspection")))
Rated_businesses <- droplevels(Rated_businesses)
table(Rated_businesses$RatingName)

Rated_businesses_geocoded <- subset(Geocoded_FSA_data, !(RatingName %in% c("Exempt", "Awaiting Inspection")))

Counties_map = readOGR(dsn = "./Boundaries", layer = "Counties_December_2016_Full_Extent_Boundaries_in_England") 
Counties_map <- subset(Counties_map, Counties_map@data$cty16nm == "West Sussex")
Counties_map <- spTransform(Counties_map, CRS("+proj=longlat +datum=WGS84 +no_defs"))


Rated_businesses_geocoded <- subset(Rated_businesses_geocoded, BusinessType %in% c("Restaurant/Cafe/Canteen","Pub/bar/nightclub", "Takeaway/sandwich shop", "Retailers - supermarkets/hypermarkets", "Hotel/bed & breakfast/guest house"))

getColor <- function(Rated_businesses_geocoded) {
  sapply(Rated_businesses_geocoded$BusinessType, function(BusinessType) {
    if(BusinessType == "Restaurant/Cafe/Canteen") {
      "green"
    } else if(BusinessType == "Pub/bar/nightclub") {
      "orange"
    } else if(BusinessType == "Takeaway/sandwich shop" ) {
      "red"
    } else if(BusinessType == "Retailers - supermarkets/hypermarkets" ) {
      "purple"
    } else if(BusinessType == "Hotel/bed & breakfast/guest house" ) {
      "darkgreen" 
    } else {
      "cadetblue"
       } } )
}


FSAicons <- awesomeIcons(icon = 'dot-circle-o ',  iconColor = 'white',  library = 'fa',  markerColor = getColor(Rated_businesses_geocoded))

FSA <- leaflet() %>% 
  addTiles(urlTemplate = "http://{s}.tiles.wmflabs.org/bw-mapnik/{z}/{x}/{y}.png",attribution = '&copy; <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a> <br>Contains National Statistics data © Crown copyright and database right (2017)') %>%
  addAwesomeMarkers(Rated_businesses_geocoded, lng = Rated_businesses_geocoded$Longitude, lat = Rated_businesses_geocoded$Latitude, group = "Show GPs", popup = paste(Rated_businesses_geocoded$BusinessName, "<br>Overall rating: ", Rated_businesses_geocoded$RatingName, "<br>Date inspected: ", Rated_businesses_geocoded$RatingDate, sep = ""), icon=FSAicons) %>%
  addPolygons(data = Counties_map, stroke = TRUE, color = "#000000", weight = 2, fill = 0)

unique(FSA_data$BusinessType)

saveWidget(FSA, "./FSA.html", selfcontained = TRUE)

# The information and services at food.gov.uk/ratings are covered by the Open Government Licence (OGL).

# The data provide the food hygiene rating or inspection result given to a business and reflect the standards of food hygiene found on the date of inspection or visit by the local authority. Businesses include restaurants, pubs, cafés, takeaways, hotels and other places consumers eat, as well as supermarkets and other food shops.

# Rating areas ####
# Hygienic food handling  (Hygience)
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
  addTiles(urlTemplate = "http://{s}.tiles.wmflabs.org/bw-mapnik/{z}/{x}/{y}.png",attribution = '&copy; <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a> <br>Contains National Statistics data © Crown copyright and database right (2017)') %>%
  addPolygons(data = LSOAs_map, stroke = TRUE, weight = 2, color = "#1a0dab", fill = "#1a0dab", fillOpacity = 0.2, popup = paste("LSOA Name: ",LSOAs_map@data$lsoa11nm, "<br>LSOA Code: ", LSOAs_map@data$lsoa11cd, sep = ""), group = "Show LSOAs") %>% 
  addPolygons(data = DementiaSupport_boundary, stroke = TRUE, weight = 2, color = "#7a1212", fill = "#7a1212", fillOpacity = 0.2, popup = DementiaSupport_boundary@data$area, group = "Show boundary") %>%
  addLayersControl(baseGroups = c("Show LSOAs", "Show boundary"),overlayGroups = c("Show GPs"),options = layersControlOptions(collapsed = FALSE)) %>%
  addAwesomeMarkers(GPs_inside_boundary_data, lng = GPs_inside_boundary_data$long, lat = GPs_inside_boundary_data$lat, group = "Show GPs", popup = paste(GPs_inside_boundary_data$Name, sep = ""), icon=GPicons)