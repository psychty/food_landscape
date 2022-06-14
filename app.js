

// Load Food Hygeine Rating Service data geojson
var fhrs_geojson = $.ajax({
    url: "./outputs/fhrs_west_sussex.geojson",
    dataType: "json",
    success: console.log("FHRS data successfully loaded."),
    error: function (xhr) {
      alert(xhr.statusText);
    },
  });
  
  window.onload = () => {
    // loadTable_pcn_numbers_in_quintiles(PCN_deprivation_data);
    // loadTable_gp_numbers_in_quintiles(chosen_PCN_gp_quintile);
    // loadTable_ccg_af_prevalence(chosen_af_cvd_prevent_data);
    // loadTable_ccg_hyp_prevalence(chosen_hyp_cvd_prevent_data);
    // loadTable_ccg_ckd_prevalence(chosen_ckd_cvd_prevent_data);
  };
  
  wsx_areas = ['Adur', 'Arun', 'Chichester', 'Crawley', 'Horsham', 'Mid Sussex', 'Worthing']
  
  var width = window.innerWidth * 0.8 - 20;
  // var width = document.getElementById("daily_case_bars").offsetWidth;
  if (width > 1200) {
    var width = 1200;
  }
  var width_margin = width * 0.15;
  var height = window.innerHeight * .5;
  
  var formatPercent = d3.format(".1%");
  var formatPercent_1 = d3.format(".0%");
  

  // function lsoa_deprivation_colour(feature) {
  //   return {
  //     fillColor: lsoa_covid_imd_colour_func(feature.properties.IMD_2019_decile),
  //     color: lsoa_covid_imd_colour_func(feature.properties.IMD_2019_decile),
  //     // color: 'blue',
  //     weight: 1,
  //     fillOpacity: 0.85
  //   }
  // }
  
  // function core20_deprivation_colour(feature) {
  //   return {
  //     fillColor: 'red',
  //     color: 'red',
  //     weight: 2,
  //     fillOpacity: 0.25
  //   }
  // }
  
  // Define the background tiles for our maps 
  // This tile layer is coloured
  // var tileUrl = "https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png";
  
  // This tile layer is black and white
  var tileUrl = "https://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}{r}.png";
  // Define an attribution statement to go onto our maps
  var attribution =
    '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors, <a href="https://creativecommons.org/licenses/by-sa/2.0/">CC-BY-SA</a>, Contains Ordnance Survey data © Crown copyright and database right 2022';
  
  // Specify that this code should run once the PCN_geojson data request is complete
  $.when(fhrs_geojson).done(function () {
  
  // Create a leaflet map (L.map) in the element map_1_id
  var map_1 = L.map("map_1_id");
   
  // add the background and attribution to the map
  L.tileLayer(tileUrl, { attribution })
   .addTo(map_1);
  
  // var pcn_boundary = L.geoJSON(PCN_geojson.responseJSON, { style: pcn_boundary_colour })
  //  .addTo(map_1)
  //  .bindPopup(function (layer) {
  //     return (
  //       "Primary Care Network: <Strong>" +
  //       layer.feature.properties.PCN_Code +
  //       " " +
  //       layer.feature.properties.PCN_Name +
  //       "</Strong>"
  //     );
  //  });
  
  // map_1.fitBounds(pcn_boundary.getBounds());
  
  // TODO fix gp markers
  // This loops through the dataframe and plots a marker for every record.
  
  var pane1 = map_1.createPane('markers1');
  
   for (var i = 0; i < fhrs_geojson.length; i++) {
   gps = new L.circleMarker([fhrs_geojson[i]['lat'], fhrs_geojson[i]['long']],
        {
        pane: 'markers1',
        radius: 6,
        color: '#000',
        weight: .5,
        // fillColor: setPCNcolour_by_name(GP_location[i]['PCN_Name']),
        fillOpacity: 1})
      // .bindPopup('<Strong>' + fhrs_geojson[i]['Area_Code'] + ' ' + GP_location[i]['Area_Name'] + '</Strong><br><br>This practice is part of the ' + GP_location[i]['PCN_Code'] + ' ' + GP_location[i]['PCN_Name'] + '. There are ' + d3.format(',.0f')(GP_location[i]['Total']) +' patients registered to this practice.')
      .addTo(map_1) 
     }
  
      var baseMaps_map_1 = {
        "Show premises boundary": pcn_boundary,
        // "Show GP practices": markers1, 
      };
    
       L.control
       .layers(null, baseMaps_map_1, { collapsed: false })
       .addTo(map_1);
  
  });