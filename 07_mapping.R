

source("01_ConfigInputs.R")

library(leaflet)

figure_location <- "Output/"

# Specify the path where you want to create the new folder
output_path <- paste0(figure_location, "Model_results/", as.character(Sys.Date()))

# Well information

well_info <- pgown_well_info_all

aquifer_info <- readr::read_csv("https://apps.nrs.gov.bc.ca/gwells/api/v1/aquifers/csv") %>%
  dplyr::select(aquifer_id, aquifer_name, material, subtype)


wells_sf <- bcdata::bcdc_query_geodata("e4731a85-ffca-4112-8caf-cb0a96905778") %>%
  dplyr::filter(!is.na(.data$OBSERVATION_WELL_NUMBER)) %>%
  dplyr::collect() %>%
  dplyr::select(Well = OBSERVATION_WELL_NUMBER,
                aquifer_id = AQUIFER_ID) %>%
  dplyr::mutate(Well = paste0("OW", Well)) %>%
  dplyr::left_join(well_info, by = join_by(Well, aquifer_id)) %>%
  dplyr::left_join(aquifer_info, by = join_by(aquifer_id)) %>%
  dplyr::mutate(type = ifelse(Snow_influenced == 0, "rain", "snow"),
                link_jpeg = paste0("<a href = 'https://nrs.objectstore.gov.bc.ca/rfc-conditions/gw_forecasting/outputs/Well_",Well,"_Model_Predictions.pdf' target='_blank' > Hydrograph and Forecast (pdf) </a>")) %>% 
  filter(Well %in% well_info$Well)
# mapview::mapview(wells_sf)

# Well forecast


latest_model_date <- list.dirs("Output/Model_results", full.names = FALSE) %>% 
  as.Date() %>% max(na.rm = TRUE)

latest_csv_files <- list.files(paste0("Output/Model_results/",latest_model_date), pattern = "csv", recursive = FALSE)


forecast <- bind_rows(lapply(latest_csv_files, function(file){
  read_csv(paste0("Output/Model_results/",latest_model_date,"/",file)) %>% 
    mutate(Date_predicted = as.Date(Date_predicted))
})) %>% 
  dplyr::left_join(wells_sf, by = join_by(Well))  %>%
  sf::st_as_sf() %>%
  sf::st_transform(crs = 4326) %>%
  dplyr::mutate(#latest_percentile_category_map = case_when(latest_percentile == 75 ~ "High",
                #                                            latest_percentile >= 90 ~ "Much Above Normal",
                #                                            latest_percentile > 75 ~ "Above Normal",
                #                                            latest_percentile >= 25 ~ "Normal",
                #                                            latest_percentile < 25 ~ "Below Normal",
                #                                            latest_percentile <= 10 ~ "Much Below Normal",
                #                                            latest_percentile == 0 ~ "Low"),
                # latest_percentile_category = case_when(latest_percentile > 75 ~ "Above Normal",
                #                                        latest_percentile >= 25 ~ "Normal",
                #                                        latest_percentile < 25 ~ "Below Normal"),
                likelihood_category = case_when(likelihood > 90 ~ "90-100% - Extremely Likely",
                                                likelihood > 75 ~ "75-90% - Very Likely",
                                                likelihood > 50 ~ "50-75% - Likely",
                                                likelihood > 25 ~ "25-50% - Somewhat Likely",
                                                likelihood > 10 ~ "10-25% - Unlikely",
                                                likelihood >= 0  ~ "0-10% - Very Unlikely",
                                                is.na(likelihood) ~ "Not Available"),
                likelihood_category = factor(likelihood_category, levels = c("90-100% - Extremely Likely",
                                                                             "75-90% - Very Likely",
                                                                             "50-75% - Likely",
                                                                             "25-50% - Somewhat Likely",
                                                                             "10-25% - Unlikely",
                                                                             "0-10% - Very Unlikely",
                                                                             "Not Available")),
                likelihood_label = case_when(likelihood < 5 ~ "<5",
                                             likelihood >95 ~ ">95",
                                             TRUE ~ as.character(round(likelihood)))
  )



forecast_14d <- forecast %>%
  filter(lag_day == 14 | is.na(lag_day))
date_14d <- as.Date(max(unique(forecast_14d$Date_predicted), na.rm = TRUE))
date_14d_range <- seq((date_14d-2), date_14d, by = "1 day")

forecast_30d <- forecast %>%
  filter(lag_day == 30 | is.na(lag_day))
date_30d <- as.Date(max(unique(forecast_30d$Date_predicted), na.rm = TRUE))
date_30d_range <- seq((date_30d-2), date_30d, by = "1 day")

forecast_60d <- forecast %>%
  filter(lag_day == 60 | is.na(lag_day))
date_60d <- as.Date(max(unique(forecast_60d$Date_predicted), na.rm = TRUE))
date_60d_range <- seq((date_60d-2), date_60d, by = "1 day")

forecast_90d <- forecast %>%
  filter(lag_day == 90 | is.na(lag_day))
date_90d <- as.Date(max(unique(forecast_90d$Date_predicted), na.rm = TRUE))
date_90d_range <- seq((date_90d-2), date_90d, by = "1 day")


latest_date <- max(as.Date(forecast$Date), na.rm = TRUE)

# mapview::mapview(forecast_14d, zcol = "likelihood_category")



cols_likelihood <- c("#F95D06", "#FFA500", "#FFEA00",
                     "#B5E48C", "#75C9B7", "#A7D8F0",
                     "gray80")
levs_likelihood <- c("90-100% - Extremely Likely",
                    "75-90% - Very Likely",
                    "50-75% - Likely",
                    "25-50% - Somewhat Likely",
                    "10-25% - Unlikely",
                    "0-10% - Very Unlikely",
                    "Not Available")
vals_likelihood <- factor(levs_likelihood, levels = levs_likelihood)
pal_likelihood <- colorFactor(palette = cols_likelihood,
                             levels = levs_likelihood)




# current percentiles
levs_gw_percentile <- c("Low", "<10", "10-25", "25-75", "75-90", ">90", "High")
cols_gw_percentile <- c("red", "#800000", "#FFCC00", "#00CF00", "cyan", "blue", "black")
vals_gw_percentile <- factor(levs_gw_percentile, levels = rev(levs_gw_percentile))
pal_gw_percentile <- colorFactor(palette = cols_gw_percentile,
                                 levels = levs_gw_percentile,
                                 ordered = FALSE)

bc_bound_line <- readRDS(file = "data/spatial/bcmaps_bcbound_line.rds")


#* Flow Forecasts ----



gw_map <- leaflet::leaflet(options = leaflet::leafletOptions(attributionControl = FALSE,
                                                             zoomSnap = 0.5)) %>%
  leaflet::addTiles(group = "OpenStreetMap") %>%
  leaflet::addProviderTiles(leaflet::providers$Esri.NatGeoWorldMap, group = "NatGeoWorldMap (ESRI)") %>%
  leaflet::addProviderTiles(leaflet::providers$Esri.WorldImagery, group = "WorldImagery (ESRI)") %>%
  leaflet::addProviderTiles(leaflet::providers$Esri.WorldTopoMap, group = "Topographic (ESRI)") %>%
  leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron, group = "Positron (CartoDB)") %>%
  leaflet::addMapPane("points", zIndex = 430) %>%
  leaflet::addMapPane("polygons", zIndex = 420) %>%
  leaflet::addMapPane("cdm", zIndex = 410) %>%
  leaflet::addMapPane("bc", zIndex = 400) %>%
  # # Current Percentiles
  # leaflet::addCircleMarkers(data = forecast_14d %>% filter(conditions == "Below Normal"),
  #                           fillOpacity = 100, color = "black", radius = 6, weight = 1,
  #                           fillColor = ~pal_ptiles_gw(latest_percentile_category_map),
  #                           group = "Latest Conditions",
  #                           options = leaflet::pathOptions(pane = "points"),
  #                           label = ~paste0(Well, " (", Location, ") - ", "Latest Percentile: ",
  #                                           latest_percentile," (", latest_percentile_category_map,")"),
  #                           popup = ~paste0(
  #                             "<b>", Well, " - ", Location ,"</b>",
  #                             "<br><b>Aquifer ID:</b> ", aquifer_id, " (", aquifer_name ,")",
  #                             "<br><b>Aquifer Subtype:</b> ", subtype,
  #                             "<br><br><b>Latest Date</b>: ", Date_latest,
  #                             "<br><b>Latest Level</b>: ", latest_value, " m below ground",
  #                             "<br><b>Latest Percentile</b>: ", latest_percentile, " (",latest_percentile_category,")",
  #                             "<br>", link_jpeg)) %>%
  # leaflet::addLegend("bottomleft",
  #                    pal = pal_ptiles_gw,
  #                    values = levs_factor_gw,
  #                    opacity = 1,
  #                    title =  paste0("Groundwater Levels",
  #                                    "<br>at Provincial Observation",
  #                                    "<br>Wells as of ", format( latest_date, "%b %d"),
  #                                    "<br><br>Percentile Class"),
                     # group = "Latest Conditions") %>%
  leaflet::addCircleMarkers(data = forecast_14d %>% filter(conditions == "Below Normal"),
                            fillOpacity = 100, color = "black", radius = 6, weight = 1,
                            fillColor = ~pal_likelihood(likelihood_category),
                            group = "Below Normal - 14 days",
                            options = leaflet::pathOptions(pane = "points"),
                            label = ~paste0(Well, " (", Location, ") - ", "Likelihood Below Normal: ",likelihood_label, "%"),
                            popup = ~paste0(
                              "<b>", Well, " - ", Location ,"</b>",
                              "<br><b>Aquifer ID:</b> ", aquifer_id, " (", aquifer_name ,")",
                              "<br><b>Aquifer Subtype:</b> ", subtype,
                              "<br><br><b>Latest Date</b>: ", Date,
                              # "<br><b>Latest Level</b>: ", latest_value, " m below ground",
                              # "<br><b>Latest Percentile</b>: ", latest_percentile, " (",latest_percentile_category,")",
                              "<br><b>Predicted Date</b>: ", Date_predicted, " - ", lag_day, " days ahead",
                              "<br><b>Likelihood of Below Normal Conditions</b>: ", likelihood_label, "%",
                              "<br>", link_jpeg)) %>%
  leaflet::addLegend("bottomleft",
                     pal = pal_likelihood,
                     values = vals_likelihood,
                     opacity = 1,
                     title = paste0("Likelihood of ",
                                    "<br>Below Normal",
                                    "<br>Groundwater Levels",
                                    "<br>in 14 days (", format(as.Date(date_14d), "%b %d"),")",
                                    "<br><br>Likelihood") ,
                     group = "Below Normal - 14 days") %>%
  leaflet::addCircleMarkers(data = forecast_30d %>% filter(conditions == "Below Normal"),
                            fillOpacity = 100, color = "black", radius = 6, weight = 1,
                            fillColor = ~pal_likelihood(likelihood_category),
                            group = "Below Normal - 30 days",
                            options = leaflet::pathOptions(pane = "points"),
                            label = ~paste0(Well, " (", Location, ") - ", "Likelihood Below Normal: ",likelihood_label, "%"),
                            popup = ~paste0(
                              "<b>", Well, " - ", Location ,"</b>",
                              "<br><b>Aquifer ID:</b> ", aquifer_id, " (", aquifer_name ,")",
                              "<br><b>Aquifer Subtype:</b> ", subtype,
                              "<br><br><b>Latest Date</b>: ", Date,
                              # "<br><b>Latest Level</b>: ", latest_value, " m below ground",
                              # "<br><b>Latest Percentile</b>: ", latest_percentile, " (",latest_percentile_category,")",
                              "<br><b>Predicted Date</b>: ", Date_predicted, " - ", lag_day, " days ahead",
                              "<br><b>Likelihood of Below Normal Conditions</b>: ", likelihood_label, "%",
                              "<br>", link_jpeg)) %>%
  leaflet::addLegend("bottomleft",
                     pal = pal_likelihood,
                     values = vals_likelihood,
                     opacity = 1,
                     title = paste0("Likelihood of ",
                                    "<br>Below Normal",
                                    "<br>Groundwater Levels",
                                    "<br>in 30 days (", format(as.Date(date_30d), "%b %d"),")",
                                    "<br><br>Likelihood") ,
                     group = "Below Normal - 30 days") %>% 
  leaflet::addCircleMarkers(data = forecast_60d %>% filter(conditions == "Below Normal"),
                            fillOpacity = 100, color = "black", radius = 6, weight = 1,
                            fillColor = ~pal_likelihood(likelihood_category),
                            group = "Below Normal - 60 days",
                            options = leaflet::pathOptions(pane = "points"),
                            label = ~paste0(Well, " (", Location, ") - ", "Likelihood Below Normal: ",likelihood_label, "%"),
                            popup = ~paste0(
                              "<b>", Well, " - ", Location ,"</b>",
                              "<br><b>Aquifer ID:</b> ", aquifer_id, " (", aquifer_name ,")",
                              "<br><b>Aquifer Subtype:</b> ", subtype,
                              "<br><br><b>Latest Date</b>: ", Date,
                              # "<br><b>Latest Level</b>: ", latest_value, " m below ground",
                              # "<br><b>Latest Percentile</b>: ", latest_percentile, " (",latest_percentile_category,")",
                              "<br><b>Predicted Date</b>: ", Date_predicted, " - ", lag_day, " days ahead",
                              "<br><b>Likelihood of Below Normal Conditions</b>: ", likelihood_label, "%",
                              "<br>", link_jpeg)) %>%
  leaflet::addLegend("bottomleft",
                     pal = pal_likelihood,
                     values = vals_likelihood,
                     opacity = 1,
                     title = paste0("Likelihood of ",
                                    "<br>Below Normal",
                                    "<br>Groundwater Levels",
                                    "<br>in 60 days (", format(as.Date(date_60d), "%b %d"),")",
                                    "<br><br>Likelihood") ,
                     group = "Below Normal - 60 days") %>% 
  leaflet::addCircleMarkers(data = forecast_90d %>% filter(conditions == "Below Normal"),
                            fillOpacity = 100, color = "black", radius = 6, weight = 1,
                            fillColor = ~pal_likelihood(likelihood_category),
                            group = "Below Normal - 90 days",
                            options = leaflet::pathOptions(pane = "points"),
                            label = ~paste0(Well, " (", Location, ") - ", "Likelihood Below Normal: ",likelihood_label, "%"),
                            popup = ~paste0(
                              "<b>", Well, " - ", Location ,"</b>",
                              "<br><b>Aquifer ID:</b> ", aquifer_id, " (", aquifer_name ,")",
                              "<br><b>Aquifer Subtype:</b> ", subtype,
                              "<br><br><b>Latest Date</b>: ", Date,
                              # "<br><b>Latest Level</b>: ", latest_value, " m below ground",
                              # "<br><b>Latest Percentile</b>: ", latest_percentile, " (",latest_percentile_category,")",
                              "<br><b>Predicted Date</b>: ", Date_predicted, " - ", lag_day, " days ahead",
                              "<br><b>Likelihood of Below Normal Conditions</b>: ", likelihood_label, "%",
                              "<br>", link_jpeg)) %>%
  leaflet::addLegend("bottomleft",
                     pal = pal_likelihood,
                     values = vals_likelihood,
                     opacity = 1,
                     title = paste0("Likelihood of ",
                                    "<br>Below Normal",
                                    "<br>Groundwater Levels",
                                    "<br>in 90 days (", format(as.Date(date_90d), "%b %d"),")",
                                    "<br><br>Likelihood") ,
                     group = "Below Normal - 90 days") %>% 
  addPolylines(data = bc_bound_line,
               color = "black", weight = 1,
               group = "Province",
               options = pathOptions(pane = "bc")) %>% 
  leaflet::addLayersControl(baseGroups = c("Topographic (ESRI)", "OpenStreetMap",
                                           "NatGeoWorldMap (ESRI)", "WorldImagery (ESRI)",
                                           "Positron (CartoDB)"),
                            #overlayGroups = c("Above Normal", "Normal", "Below Normal","Combined Likelihood"),
                            overlayGroups = c("Latest Conditions",
                                              "Below Normal - 14 days","Below Normal - 30 days",
                                              "Below Normal - 60 days","Below Normal - 90 days"),
                            options = leaflet::layersControlOptions(collapsed = TRUE))  %>%
  leaflet::hideGroup(group = c("Latest Conditions",
                               "Below Normal - 30 days", "Below Normal - 60 days","Below Normal - 90 days")) # "Below Normal",
#leaflet::hideGroup(group = c("Above Normal", "Normal","Combined Likelihood")) # "Below Normal",
# gw_map



map_title_text <- paste0("<b><u>Groundwater Drought Forecasting - DEMONSTRATION PURPOSES ONLY</u></b>")

map_title_text_body <- paste0("This project is under development and should not be used for any decision making.
                              This map is for demonstration and internal purposes at this time.
                              Updated: <b>", format(Sys.time(), format = "%H:%M %a. %b. %d, %Y"), "</b>.")


headerCSS <- "
<style>
  .leaflet-container {
    position: relative;
  }
  #header {
    position: absolute;
    top: 0;
    width: 100%;
    background-color: rgba(35, 64, 117, 0.7); /* Change background color and transparency here */
    color: white;
    text-align: center;
    padding: 5px 50px 5px 50px; /* 10px top/bottom and 20px left/right */
    font-size: 14px;
    z-index: 1000; /* Ensure the header stays above other map elements */
    max-width: calc(100% - 0px); /* Limit width to 100% minus padding (20px on each side) */
    margin: 0 auto; /* Center the header horizontally */
    font-family: 'Calibri', sans-serif; /* Change font type here */
    border-bottom: 4px solid #e3a82b; /* Add thin line at the bottom with hex color */
  }
</style>
"

# HTML for the header
headerHTML <- paste0("
<div id='header'>
  <font size='4'>",map_title_text,"</font><br>
  ",map_title_text_body,"
</div>
")
gw_map <- gw_map  %>%
  htmlwidgets::prependContent(htmltools::HTML(headerCSS)) %>%
  htmlwidgets::prependContent(htmltools::HTML(headerHTML))


#gw_map

htmlwidgets::saveWidget(widget = gw_map, 
                        file = paste0(output_path, "/Groundwater_Drought_Forecasting_Map.html"),
                        selfcontained = TRUE,
                        title = "B.C. Groundwater Drought Forecasting")

