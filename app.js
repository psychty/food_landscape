

// Load Food Hygeine Rating Service data geojson
var fhrs_points_geojson = $.ajax({
    url: "./outputs/fhrs_west_sussex.geojson",
    dataType: "json",
    success: console.log("FHRS data successfully loaded."),
    error: function (xhr) {
      alert(xhr.statusText);
    },
  });

// Load Food Hygeine Rating Service data geojson
var fhrs_lsoa_geojson = $.ajax({
  url: "./outputs/fhrs_west_sussex_lsoa_summary.geojson",
  dataType: "json",
  success: console.log("FHRS lsoa data successfully loaded."),
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
  
  function lsoa_fhrs_colour(feature) {
    return {
      // fillColor: lsoa_covid_imd_colour_func(feature.properties.IMD_2019_decile),
      // color: lsoa_covid_imd_colour_func(feature.properties.IMD_2019_decile),
      fillColor: 'orange',
      color: 'blue',
      weight: 1,
      fillOpacity: 0.5
    }
  }
  
  // function core20_deprivation_colour(feature) {
  //   return {
  //     fillColor: 'red',
  //     color: 'red',
  //     weight: 2,
  //     fillOpacity: 0.25
  //   }
  // }
  
// L. is leaflet
// This tile layer is coloured
var tileUrl_coloured = "https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png";

// This tile layer is black and white
var tileUrl_bw = "https://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}{r}.png";

// Define an attribution statement to go onto our maps
var attribution =
  '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors, <a href="https://creativecommons.org/licenses/by-sa/2.0/">CC-BY-SA</a>, Contains Ordnance Survey data © Crown copyright and database right 2022';

// These are styles for the markers
var OutletMarkerOptions = {
  radius: .01,
  color: '#000',
  weight: .01,
  fillColor: 'green',
  fillOpacity: 1
};



// Specify that this code should run once the fhrs_lsoa_geojson AND fhrs_points_geojson data request are complete
$.when(fhrs_lsoa_geojson, fhrs_points_geojson).done(function () {

    // Add the UK boundary polygons, with the uk_boundary_colour style to map_1 
var lsoa_boundary_1 = L.geoJSON(fhrs_lsoa_geojson.responseJSON, { style: lsoa_fhrs_colour })

  // Create a leaflet map (L.map) in the element map_1_id
var map_1 = L.map("map_1_id", {
  zoomControl: true , 
  scrollWheelZoom: true, 
  doubleClickZoom: true,
  touchZoom: true, }); // We have disabled zooming on this map

L.control.scale().addTo(map_1); // This adds a scale bar to the bottom left by default

// add the background and attribution to the map 
// Note - we have used the tileUrl_bw, swap this for tileUrl_coloured to see what happens
L.tileLayer(tileUrl_coloured, { attribution })
 .addTo(map_1);

 lsoa_boundary_1.addTo(map_1) // Note that this is the part that draws the polygons on the map itself
 
map_1.fitBounds(lsoa_boundary_1.getBounds()); // We use the uk_boundary polygons to zoom the map to the whole of the UK. This will happen regardless of whether we use addTo() to draw the polygons
console.log(fhrs_points_geojson)
// This plots the geojson file as circlemarkers
// var outlet_locations = L.geoJSON(fhrs_points_geojson, {
//   pointToLayer: function (feature, latlng){
//     return L.circleMarker(latlng, OutletMarkerOptions)
//   }})
//   .bindPopup(function (layer) {
//     return (
//       "Business name: <Strong>" +
//         layer.feature.properties.BusinessName +
//       "</Strong><br>Business type: <Strong>" + 
//       layer.feature.properties.BusinessType +
//       "</Strong>" 
//       )
//     }) // add tooltip
  // .addTo(map_1); // draw it on the map



      // var baseMaps_map_1 = {
      //   "Show premises": markers1, 
      // };
    
      //  L.control
      //  .layers(null, baseMaps_map_1, { collapsed: false })
      //  .addTo(map_1);


  
// var legend_le_map = L.control({position: 'bottomright'});
// legend_le_map.onAdd = function (map) {
    
//        var div = L.DomUtil.create('div', 'info legend'),
//             grades = [70, 72.5, 75, 77.5, 80, 82.5, 85, 87.5, 90],
//             labels = ['Life expectancy<br>at birth (years)'];
    
//        for (var i = 0; i < grades.length; i++) {
//             div.innerHTML +=
//             labels.push(
//                 '<i style="background:' + getLEColor(grades[i] + 1) + '"></i> ' +
//                 grades[i] + (grades[i + 1] ? '&ndash;' + grades[i + 1] + ' years' : '+ years'));
//        }
//        div.innerHTML = labels.join('<br>');
//        return div;
//     };
    
//     legend_le_map.addTo(map);

  map_1.fitBounds(lsoa_boundary_1.getBounds());

  var marker_chosen = L.marker([0, 0]).addTo(map_1);

  //search event
  $(document).on("click", "#btnPostcode", function () {
    var input = $("#txtPostcode").val();
    var url = "https://api.postcodes.io/postcodes/" + input;

    post(url).done(function (postcode) {
      var chosen_lsoa = postcode["result"]["lsoa"];
      var chosen_lat = postcode["result"]["latitude"];
      var chosen_long = postcode["result"]["longitude"];

      marker_chosen.setLatLng([chosen_lat, chosen_long]);
      map_1.setView([chosen_lat, chosen_long], 11);

      var fhrs_lsoa_geojson_data_chosen = fhrs_lsoa_geojson.filter(function (d) {
        return d.LSOA11NM == chosen_lsoa;
      });

      console.log(fhrs_lsoa_geojson_data_chosen);

      d3.select("#local_pinpoint_1")
        .data(msoa_summary_data_chosen)
        .html(function (d) {
          return d.MSOA11NM + " (" + d.msoa11hclnm + ")";
        });

    });
  });

  //enter event - search
  $("#txtPostcode").keypress(function (e) {
    if (e.which === 13) {
      $("#btnPostcode").click();
    }
  });

  //ajax call
  function post(url) {
    return $.ajax({
      url: url,
      success: function () {
        //woop
      },
      error: function (desc, err) {
        $("#result_text").html("Details: " + desc.responseText);

        d3.select("#local_pinpoint_1").html(function (d) {
          return "The postcode you entered does not seem to be valid, please check and try again.";
        });
        d3.select("#local_pinpoint_2").html(function (d) {
          return "This could be because there is a problem with the postcode look up tool we are using.";
        });
      },
    });
  }
});





