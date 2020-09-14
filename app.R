library(shiny)
library(shinythemes)
library(shinyjs)
library(shinydashboard)
library(leaflet)
library(sf)
library(tidyverse)
library(reshape2)
library(DT)
library(htmltools)

`%ni%` <-  Negate(`%in%`)

# load datasets for all intersections
PDI_default <- read.csv("default/pdi/pdi_score.csv", stringsAsFactors = TRUE)
PPI_default <- read.csv("default/ppi/ppi_score.csv", stringsAsFactors = TRUE)
PDI_raw <- read.csv("www/PDI_variables_raw.csv", stringsAsFactors = TRUE)
PPI_raw <- read.csv("www/PPI_variables_raw.csv", stringsAsFactors = TRUE)
PDIVars <- setdiff(colnames(PDI_raw), "INTERSECTION_ID")
PPIVars <- setdiff(colnames(PPI_raw), "INTERSECTION_ID")

pred_ped_AADT <- read.csv("www/pred_ped_bike_AADT_2019.csv") %>%
  select(INTERSECTION_ID, Actual_MEAN = AADT, Predicted_MEAN = fit,
         Actual_LOWER = AADT_LOWER, Predicted_LOWER = lower,
         Actual_UPPER = AADT_UPPER, Predicted_UPPER = upper) %>%
  pivot_longer(-INTERSECTION_ID,
               names_to = c("TYPE", ".value"),
               names_pattern = "(.+)_(.+)")

pred_ped_AADT_rank <- read.csv("www/pred_ped_bike_AADT_2019.csv") %>%
  select(INTERSECTION_ID, RANK_PERCENTILE)

pred_traffic_AADT <- read.csv("www/pred_non_ped_bike_AADT_2019.csv") %>%
  select(INTERSECTION_ID, Actual_MEAN = AADT, Predicted_MEAN = fit,
         Actual_LOWER = AADT_LOWER, Predicted_LOWER = lower,
         Actual_UPPER = AADT_UPPER, Predicted_UPPER = upper) %>%
  pivot_longer(-INTERSECTION_ID,
               names_to = c("TYPE", ".value"),
               names_pattern = "(.+)_(.+)")

pred_traffic_AADT_rank <- read.csv("www/pred_non_ped_bike_AADT_2019.csv") %>%
  select(INTERSECTION_ID, RANK_PERCENTILE)

inters <- st_read("www/intersections_with_neighbourhoods.geojson", quiet = TRUE) %>%
  st_transform(crs = 4326) %>%
  mutate(NAME = as.character(NAME))

all_ids <- unique(inters$INTERSECTION_ID)

# filtered data
all_ids_filtered <- st_read("www/intersection_no_crosswalk.geojson", quiet = TRUE) %>%
  pull(INTERSECTION_ID) %>%
  unique(.)

PDI_default_filtered <- PDI_default %>%
  filter(INTERSECTION_ID %in% all_ids_filtered) %>%
  mutate(RANK = rank(SCORE, ties.method = "min")) %>%
  group_by(RANK) %>%
  mutate(RANK_PERCENTILE = round((RANK - 1 + 0.5 * n()) / nrow(.) * 100, digits = 2)) %>%
  ungroup()

PPI_default_filtered <- PPI_default %>%
  filter(INTERSECTION_ID %in% all_ids_filtered) %>%
  mutate(RANK = rank(SCORE, ties.method = "min")) %>%
  group_by(RANK) %>%
  mutate(RANK_PERCENTILE = round((RANK - 1 + 0.5 * n()) / nrow(.) * 100, digits = 2)) %>%
  ungroup()

PDI_raw_filtered <- PDI_raw %>% filter(INTERSECTION_ID %in% all_ids_filtered)
PPI_raw_filtered <- PPI_raw %>% filter(INTERSECTION_ID %in% all_ids_filtered)

pred_ped_AADT_filtered <- pred_ped_AADT %>% filter(INTERSECTION_ID %in% all_ids_filtered)
pred_ped_AADT_rank_filtered <- pred_ped_AADT_rank %>% filter(INTERSECTION_ID %in% all_ids_filtered)
pred_traffic_AADT_filtered <- pred_traffic_AADT %>% filter(INTERSECTION_ID %in% all_ids_filtered)
pred_traffic_AADT_rank_filtered <- pred_traffic_AADT_rank %>% filter(INTERSECTION_ID %in% all_ids_filtered)

inters_filtered <- inters %>% filter(INTERSECTION_ID %in% all_ids_filtered)

# the following datasets are not affected by switching to filtered intersections
neighbourhood <- st_read("www/surrey_neighbours.geojson", quiet = TRUE) %>%
  st_transform(crs = 4326)

neighbourhood_names <- neighbourhood$NEIGHBOURHOOD %>%
  #droplevels() %>%
  as.character(.) %>%
  sort()

elementary <- st_read("www/school.geojson", quiet = TRUE) %>%
  st_transform(crs = 4326) %>%
  st_centroid(.) %>%
  filter(FACILITY_SUBTYPE %in% c("Elementary", "Private", "Francophone")) %>%
  mutate(TYPE = "elementary",
         NAME = str_to_title(NAME)) %>%
  select(TYPE, NAME, geometry)

secondary <- st_read("www/school.geojson", quiet = TRUE) %>%
  st_transform(crs = 4326) %>%
  st_centroid(.) %>%
  filter(FACILITY_SUBTYPE %in% c("Secondary")) %>%
  mutate(TYPE = "secondary",
         NAME = str_to_title(NAME)) %>%
  select(TYPE, NAME, geometry)

existing_crosswalk <- st_read("www/crosswalk_inventory.geojson", quiet = TRUE) %>%
  st_transform(crs = 4326) %>%
  select(geometry)

poi <- read.csv("www/places_of_interest.csv", header = TRUE) %>%
  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4326)
request <- st_read("www/crosswalk_requests.geojson", quiet = TRUE) %>%
  st_transform(crs = 4326)
transit <- st_read("www/bus_stops.geojson", quiet = TRUE) %>%
  st_transform(crs = 4326)
parks <- poi %>%
  filter(FACILITY_TYPE %in% c("Water Park", "Park", "Skate Park", "Bike Park"))
hospital <- poi %>%
  filter(FACILITY_TYPE == "Hospital")
shopping <- poi %>%
  filter(FACILITY_TYPE == "Shopping Centre")
seniors <- poi %>%
  filter(FACILITY_TYPE == "Seniors Centre")
community_hall <- poi %>%
  filter(FACILITY_TYPE %in% c("Community Hall/Centre", "Recreation Centre"))
library <- poi %>%
  filter(FACILITY_TYPE == "Library")
youth <- poi %>%
  filter(FACILITY_TYPE == "Youth Centre")
movie <- poi %>%
  filter(FACILITY_TYPE == "Movie Theatre")
attraction <- poi %>%
  filter(FACILITY_TYPE == "Attraction")
recreation <- poi %>%
  filter(FACILITY_TYPE == "Recreation Centre")

employment <- st_read("www/property.geojson", quiet = TRUE) %>%
  st_transform(crs = 4326) %>%
  filter(EMPLOYMENT > 20)

ped_crash <- st_read("www/ped_crash_1yr_0fill_500_Meters_2.geojson", quiet = TRUE) %>%
  st_transform(crs = 4326) %>%
  filter(PATTERN != "No Pattern Detected") %>%
  select(FID, PATTERN, geometry)

ped_crash$PATTERN <- fct_relevel(ped_crash$PATTERN, "Intensifying Hot Spot", "Persistent Hot Spot", "Consecutive Hot Spot", "Sporadic Hot Spot", "New Hot Spot")
pal_ped_crash <- colorFactor(c("#BD0026", "#F03B20", "#FD8D3C", "#FECC5C", "#FFFFB2"),
                              levels = c("Intensifying Hot Spot", "Persistent Hot Spot", "Consecutive Hot Spot", "Sporadic Hot Spot", "New Hot Spot"))

pal.piedonut.pdi <- c("#FEF0D9", "#FDD49E", "#FDBB84", "#FC8D59", "#E34A33", "#B30000", "#990000")
pal.piedonut.ppi <- c("#F4F5FD", "#E0E1F2", "#CBCDE6", "#B7B9DB", "#A3A5D0", "#8E91C4", "#7A7DB9", "#6668AE", "#5154A2", "#3D4097", "#292C8C", "#000475")
length_to_pad <- max(length(pal.piedonut.pdi), length(pal.piedonut.ppi))
pal.piedonut <- c(pal.piedonut.pdi, pal.piedonut.ppi, sapply(seq_len(length_to_pad), function(i) "white"))
names(pal.piedonut) <- c(PDIVars, "PDI", PPIVars, "PPI", sapply(seq_len(length_to_pad), function(i) i))

varNameLabelMap <- list(
  "PDI" = "PDI",
  "PPI" = "PPI",
  "SPEED" = "Speed",
  "LANES" = "Lanes",
  "WIDTH" = "Width",
  "DISTANCE" = "Closest crosswalk",
  "PATTERN" = "Ped crash",
  "PRED_NON_PED_AADT" = "Vehicle AADT",
  "WALKWAY_DISTANCE" = "Walkway",
  "ELEMENTARY_DISTANCE" = "Elementary school",
  "SECONDARY_DISTANCE" = "Secondary school",
  "BUS_STOP_DISTANCE" = "Transit stop",
  "PARK_DISTANCE" = "Park",
  "COMMUNITY_CENTRE_DISTANCE" = "Community centre",
  "EMPLOYMENT_DISTANCE" = "Employment",
  "COMMERCIAL_KDE" = "Commercial",
  "BUS_USAGE_KNN" = "Bus usage (KNN)",
  "BUS_USAGE_NEAREST" = "Bus usage (nearest)",
  "LOCAL_INTEREST_DISTANCE" = "Local interest"
)

basemap <- leaflet() %>%
  addTiles(attribution = "Icons made by <a href='http://www.freepik.com/'>Freepik</a>") %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addLayersControl(
    position = "bottomright",
    overlayGroups = c("Crosswalk",
                      "Crosswalk request",
                      "Elementary school",
                      "Secondary school",
                      "Transit",
                      "Community hall",
                      "Seniors",
                      "Youth centre",
                      "Library",
                      "Recreation centre",
                      "Employment",
                      "Hospital",
                      "Parks",
                      "Shopping",
                      "Attraction",
                      "Movie theatre"),
    options = layersControlOptions(collapsed = TRUE)) %>%
  hideGroup(c("Crosswalk",
              "Crosswalk request",
              "Elementary school",
              "Secondary school",
              "Transit",
              "Community hall",
              "Seniors",
              "Youth centre",
              "Library",
              "Recreation centre",
              "Employment",
              "Hospital",
              "Parks",
              "Shopping",
              "Attraction",
              "Movie theatre")) %>%
  addMeasure(
    position = "bottomleft",
    primaryLengthUnit = "meters",
    primaryAreaUnit = "sqmeters",
    activeColor = "#3D535D",
    completedColor = "#7D4479") %>%
  addMarkers(data = existing_crosswalk, icon = icons(iconUrl = "www/crosswalk.png", iconWidth = 12, iconHeight = 12),group = "Crosswalk") %>%
  addMarkers(data = elementary, icon = icons(iconUrl = "www/elementary.png", iconWidth = 12, iconHeight = 12), group = "Elementary school", popup = ~htmlEscape(NAME)) %>%
  addMarkers(data = secondary, icon = icons(iconUrl = "www/secondary.png", iconWidth = 12, iconHeight = 12), group = "Secondary school", popup = ~htmlEscape(NAME)) %>%
  addMarkers(data = request, icon = icons(iconUrl = "www/black_request.png", iconWidth = 10, iconHeight = 10), popup = ~htmlEscape(STATUS), group = "Crosswalk request") %>%
  addMarkers(data = transit, icon = icons(iconUrl = "www/blue_transit.png", iconWidth = 7, iconHeight = 7), group = "Transit") %>%
  addMarkers(data = parks, icon = icons(iconUrl = "www/green_park.png", iconWidth = 20, iconHeight = 20), popup = ~htmlEscape(NAME), group = "Parks") %>%
  addMarkers(data = hospital, icon = icons(iconUrl = "www/pink_hospital.png", iconWidth = 25, iconHeight = 25), popup = ~htmlEscape(NAME), group = "Hospital") %>%
  addMarkers(data = shopping, icon = icons(iconUrl = "www/shopping.png", iconWidth = 20, iconHeight = 20),popup = ~htmlEscape(NAME), group = "Shopping") %>%
  addMarkers(data = seniors, icon = icons(iconUrl = "www/seniors.png", iconWidth = 20, iconHeight = 20), popup = ~htmlEscape(NAME), group = "Seniors") %>%
  addMarkers(data = community_hall, icon = icons(iconUrl = "www/community.png", iconWidth = 20, iconHeight = 20), popup = ~htmlEscape(NAME), group = "Community hall") %>%
  addMarkers(data = employment, icon = icons(iconUrl = "www/business.png", iconWidth = 10, iconHeight = 10), group = "Employment") %>%
  addMarkers(data = library, icon = icons(iconUrl = "www/library.png", iconWidth = 20, iconHeight = 20), group = "Library") %>%
  addMarkers(data = youth, icon = icons(iconUrl = "www/youth.png", iconWidth = 20, iconHeight = 20), group = "Youth centre") %>%
  addMarkers(data = movie, icon = icons(iconUrl = "www/movie.png", iconWidth = 20, iconHeight = 20), group = "Movie theatre") %>%
  addMarkers(data = attraction, icon = icons(iconUrl = "www/attraction.png", iconWidth = 18, iconHeight = 18), group = "Attraction") %>%
  addMarkers(data = recreation, icon = icons(iconUrl = "www/recreation.png", iconWidth = 20, iconHeight = 20), group = "Recreation centre")

ped_crash_map <- leaflet(data = ped_crash) %>%
  addTiles() %>%
  addProviderTiles(providers$CartoDB.DarkMatter) %>%
  setView(lng = -122.7953, lat = 49.10714, zoom = 11, 12) %>%
  addPolygons(color = "#656565", weight = 3, smoothFactor = 0.5, fillOpacity = 0, opacity = 1,
              data = neighbourhood %>%
                filter(NEIGHBOURHOOD %in% c("City Centre", "Cloverdale", "Fleetwood", "Guildford",
                                            "Newton", "South Surrey", "Whalley"))) %>%
  addCircleMarkers(color = ~pal_ped_crash(PATTERN), stroke = FALSE, fillOpacity = 0.5, radius = 3, popup = ~htmlEscape(PATTERN), layerId = ~as.character(FID)) %>%
  addLegend("bottomright", pal = pal_ped_crash, values = ~PATTERN,
            title = "Hot Spot Pattern",
            opacity = 1)

piedonut <- function(id, ppi, pdi){
  # filter out data for intersection
  data.pdi <- filter(pdi, INTERSECTION_ID == id)
  data.ppi <- filter(ppi, INTERSECTION_ID == id)

  if (nrow(data.pdi) == 0 | nrow(data.ppi) == 0) return()

  #combine index
  PDI_melt <- melt(data = data.pdi, id.vars = "INTERSECTION_ID", measure.vars = PDIVars)
  PDI_melt$index <- "PDI"
  pdi_var_names <- PDI_melt %>% filter(value != 0) %>% pull(variable) %>% as.character()

  PPI_melt <- melt(data = data.ppi, id.vars = "INTERSECTION_ID", measure.vars = PPIVars)
  PPI_melt$index <- "PPI"
  ppi_var_names <- PPI_melt %>% filter(value != 0) %>% pull(variable) %>% as.character()

  data.all_index <- bind_rows(PDI_melt, PPI_melt)

  # pad white levels to align legend
  length_to_pad <- abs(length(ppi_var_names) - length(pdi_var_names))
  max_len <- max(length(ppi_var_names), length(pdi_var_names))
  pdi_var_names <- c(pdi_var_names, seq_len(max_len - length(pdi_var_names)))
  ppi_var_names <- c(ppi_var_names, seq_len(max_len - length(ppi_var_names)))

  data1 <- aggregate(x = data.all_index$value, by = list(data.all_index$index), FUN = sum) %>%
    rename("index" = "Group.1", "total" = "x") %>%
    mutate(CUM = cumsum(total),
           pos = CUM - total/2)

  data2 <- data.all_index[,-1] %>%
    filter(value != 0) %>%
    mutate(CUM = cumsum(value),
           Pos = CUM - value/2,
           variable = factor(variable, levels = c(PDIVars, PPIVars, seq_len(length_to_pad))))

  ggplot() +
    geom_col(data = data1,
             aes(x = 2, y = total, fill = index),
             color = "white",
             width = 1.25,
             position_stack(vjust = 1, reverse = TRUE),
             show.legend=TRUE) +
    geom_col(data = data2,
             aes(x = 3, y = value, fill = variable),
             alpha = 0.75,
             color = "white",
             width = 0.75,
             position_stack(vjust = 1, reverse = TRUE),
             show.legend=FALSE) +
    scale_fill_manual(guide = guide_legend(ncol = 2),
                      values = pal.piedonut,
                      labels = function(br) varNameLabelMap[br],
                      breaks = c("PDI", pdi_var_names, "PPI", ppi_var_names),
                      drop = FALSE) +
    geom_text(aes(label = value, x = 3, y = Pos, group = index),
              data = subset(data2, value != 0),
              size = 5,
              check_overlap = TRUE,
              color = "grey28") +
    geom_text(aes(label = total, x = 2, y = pos, group = index),
              data = data1,
              size = 5,
              check_overlap = TRUE,
              color = "white") +
    xlim(0, 3.5) + labs(x = NULL, y = NULL) +
    theme(plot.margin = margin(),
          panel.grid = element_blank(),
          panel.background = element_rect(fill = "transparent"),
          legend.position = "bottom",
          legend.direction = "horizontal",
          legend.title = element_blank(),
          axis.ticks = element_blank(),
          axis.text = element_blank(),
          axis.title = element_blank()) +
    coord_polar(theta = "y")
}

# Activate menuItem when there subMenuItem exists
convertMenuItem <- function(mi,tabName) {
  mi$children[[1]]$attribs['data-toggle']="tab"
  mi$children[[1]]$attribs['data-value'] = tabName
  if(length(mi$attribs$class)>0 && mi$attribs$class == "treeview"){
    mi$attribs$class=NULL
  }
  mi
}

menuSubMenuForIndex <- function(label, tabName, varNames) {
  convertMenuItem(menuItem(
    label,
    tabName = tabName,
    icon = icon("calculator"),
    selected = T,
    lapply(varNames, function(var) {
      tags$li(a(varNameLabelMap[var], href = paste0("#", var), id = paste0(var, "_nav")))
    }),
    saveVariableScoresUI(tabName)
  ),
  tabName)
}

expandMenuSubItemOnClick <- function(varNames, nsPrefix) {
  lapply(varNames, function(var) {
    onclick(paste0(var, "_nav"),
            runjs(code = paste0('let element = document.getElementById("', nsPrefix, "-", var, '");
                                 element.parentNode.classList.remove("collapsed-box");
                                 element.parentNode.getElementsByTagName("i")[0].className = "fa fa-minus";
                                 element.style.display = "block";')))
  })
}

join_PPI_PDI <- function(PPI, PDI) {
  inner_join(PPI, PDI, by = "INTERSECTION_ID") %>%
    mutate(PPI_SCORE = SCORE.x,
           PDI_SCORE = SCORE.y,
           COMBINED_SCORE = PPI_SCORE + PDI_SCORE,
           PPI_RANK_PERCENTILE = RANK_PERCENTILE.x,
           PDI_RANK_PERCENTILE = RANK_PERCENTILE.y,
           COMBINED_RANK = rank(COMBINED_SCORE, ties.method = "min")) %>%
    group_by(COMBINED_RANK) %>%
    mutate(COMBINED_RANK_PERCENTILE = round((COMBINED_RANK - 1 + 0.5 * n()) / nrow(.) * 100, digits = 2)) %>%
    ungroup()
}

ui <- dashboardPage(skin = "green",
  dashboardHeader(title = "Surrey Crosswalk", titleWidth = 200),
  dashboardSidebar(width = 200,
                   collapsed = TRUE,
                   sidebarMenu(
                     menuItem("PPI/PDI Map", icon = icon("map"), tabName = "mapper"),
                     menuItem("Ped Crash Map", icon = icon("map"), tabName = "ped_crash_map"),
                     menuSubMenuForIndex("PPI Weight", "ppi", PPIVars),
                     menuSubMenuForIndex("PDI Weight", "pdi", PDIVars),
                     menuItem("Weight Tables", tabName = "tables", icon = icon("table"),
                              menuSubItem("PPI Weight/Score", tabName = "ppi_weight"),
                              menuSubItem("PDI Weight/Score", tabName = "pdi_weight"),
                              menuSubItem("Combined PPI/PDI Score", tabName = "rank")))),
  dashboardBody(
    tags$script(HTML("$('body').addClass('fixed');")), # fix header and sidebar
    tags$head(includeCSS("www/style.css")),
    tabItems(
      tabItem(tabName = "mapper",
              div(class = "outer",
                  leafletOutput("mymap", width = "100%", height = "100%")),
              absolutePanel(id = "controls", class = "panel panel-default",
                            top = 60, left = 55, width = 250, fixed = TRUE,
                            draggable = TRUE, height = "auto",
                            radioButtons(inputId = "intersection_data",
                                         label = h4("Intersection data:"),
                                         choices = c("All intersections" = "all",
                                                     "Intersections without crosswalks" = "filtered")),
                            selectInput(inputId = "neighbourhood_names",
                                        label = h4("Select a neighbourhood:"),
                                        choices = neighbourhood_names,
                                        selected = "Surrey"),
                            numericInput(inputId = "search_intersection_id", value = NULL, label = h4("Search by intersection ID:"), step = 1, min = 1, max = 80366),
                            div(actionButton(inputId = "search_intersection_id_go", label = "GO"), align = "right")),
              absolutePanel(id = "plots", class = "panel panel-default",
                            top = 60, right = 65, width = 250, fixed = TRUE,
                            draggable = TRUE, height = "auto", style = "overflow-y:scroll",
                            h4(textOutput(outputId = "intersection_id"), align = "center"),
                            h5(textOutput(outputId = "intersection_name"), align = "center"),
                            box(title = "Ranks", status = "success",
                                collapsible = TRUE, collapsed = TRUE,
                                width = 250, align = "center",
                                plotOutput(outputId = "rank_plot", width = "95%", height = 150)),
                            box(title = "AADT", status = "success",
                                collapsible = TRUE, collapsed = TRUE,
                                width = 250, align = "center",
                                plotOutput(outputId = "ped_aadt", width = "95%", height = 75),
                                br(),
                                plotOutput(outputId = "traffic_aadt", width = "95%", height = 75)),
                            div(plotOutput("pie"), align = "center")
                            )
      ),
      tabItem("ped_crash_map", div(class = "outer", leafletOutput("ped_crash_map_leaflet", width = "100%", height = "100%"))),
      tabItem("ppi", variableScoreUI("ppi", PPI_raw_filtered)),
      tabItem("pdi", variableScoreUI("pdi", PDI_raw_filtered)),
      tabItem("ppi_weight", dataTableOutput(outputId = "ppi_table")),
      tabItem("pdi_weight", dataTableOutput(outputId = "pdi_table")),
      tabItem("rank", dataTableOutput(outputId = "ranked_table"))
  ))
)

server <- function(input, output, session) {
  # initialize PDI, PPI as reactive values
  indices <- reactiveValues(inters = NULL,
                            all_ids = NULL,
                            PPI = NULL,
                            PDI = NULL,
                            combined = NULL,
                            pred_traffic_AADT = NULL,
                            pred_traffic_AADT_rank = NULL,
                            pred_ped_AADT = NULL,
                            pred_ped_AADT_rank = NULL)

  observeEvent(input$intersection_data, {
    if (input$intersection_data == "all") {
      indices$inters <- inters
      indices$all_ids <- all_ids
      indices$PPI <- PPI_default
      indices$PDI <- PDI_default
      indices$pred_traffic_AADT <- pred_traffic_AADT
      indices$pred_traffic_AADT_rank <- pred_traffic_AADT_rank
      indices$pred_ped_AADT <- pred_ped_AADT
      indices$pred_ped_AADT_rank <- pred_ped_AADT_rank
    } else if (input$intersection_data == "filtered") {
      indices$inters <- inters_filtered
      indices$all_ids <- all_ids_filtered
      indices$PPI <- PPI_default_filtered
      indices$PDI <- PDI_default_filtered
      indices$pred_traffic_AADT <- pred_traffic_AADT_filtered
      indices$pred_traffic_AADT_rank <- pred_traffic_AADT_rank_filtered
      indices$pred_ped_AADT <- pred_ped_AADT_filtered
      indices$pred_ped_AADT_rank <- pred_ped_AADT_rank_filtered
    }
  })

  # updates combined score when either PPI or PDI changes
  observe({
    indices$combined <- join_PPI_PDI(indices$PPI, indices$PDI)
  })

  # load maps
  output$mymap <- renderLeaflet(basemap)
  output$ped_crash_map_leaflet <- renderLeaflet(ped_crash_map)

  # track which intersection is currently selected
  current <- reactiveValues(clicked = NULL, lng = NULL, lat = NULL)

  # when the user switches to a different neighbourhood
  observeEvent(list(input$neighbourhood_names, indices$combined), {
    sum_all <- indices$combined

    if (input$neighbourhood_names == "Surrey") {
      data <- neighbourhood %>%
        filter(NEIGHBOURHOOD %in% c("City Centre", "Cloverdale", "Fleetwood", "Guildford",
                                    "Newton", "South Surrey", "Whalley"))
      inters_included <- indices$inters %>%
        filter(NEIGHBOURHOOD %in% c("Cloverdale", "Fleetwood", "Guildford",
                                    "Newton", "South Surrey", "Whalley")) %>%
        left_join(indices$combined, by = "INTERSECTION_ID")
    } else {
      data <- neighbourhood %>% filter(NEIGHBOURHOOD == input$neighbourhood_names)
      inters_included <- indices$inters %>%
        filter(NEIGHBOURHOOD == input$neighbourhood_names) %>%
        left_join(sum_all, by = "INTERSECTION_ID")
    }

    current_id <- ifelse(is.null(current$clicked),
                         -1,
                         str_split(string = current$clicked, pattern = "_")[[1]][2])

    if (current_id %ni% inters_included$INTERSECTION_ID) {
      output$intersection_id <- renderText(NULL)
      output$intersection_name <- renderText(NULL)
      output$pie <- renderPlot(NULL)
      output$ped_aadt <- renderPlot(NULL)
      output$traffic_aadt <- renderPlot(NULL)
      output$rank_plot <- renderPlot(NULL)

      current$clicked <- NULL
      current$lng <- NULL
      current$lat <- NULL
    }

    colorScale <- colorFactor(palette = "viridis", sum_all$COMBINED_SCORE)
    leafletProxy("mymap", data = data) %>%
      setView(lng = ifelse(input$neighbourhood_names == "Surrey", -122.7953, data$CENTROID_LONG),
              lat = ifelse(input$neighbourhood_names == "Surrey", 49.10714, data$CENTROID_LAT),
              zoom = ifelse(input$neighbourhood_names == "Surrey", 11, 12)) %>%
      clearShapes() %>%
      clearControls() %>%
      removeMarker(layerId = as.character(all_ids)) %>%
      #removeMarker(layerId = current$clicked) %>%
      addPolygons(color = "#141722", weight = 3, smoothFactor = 0.5,
                  fillOpacity = 0.1, opacity = 1) %>%
      addCircleMarkers(data = inters_included, radius = 1, fillOpacity = 1,
                       stroke = TRUE, color = ~colorScale(COMBINED_SCORE), layerId = ~as.character(INTERSECTION_ID), ) %>%
      addLegend("bottomleft", pal = colorScale, values = ~round(seq(min(sum_all$COMBINED_SCORE), max(sum_all$COMBINED_SCORE), length.out = 5), digits = 0),
                title = "<small>Combined PPI/PDI</small>") %>%
      {ifelse(current_id %ni% inters_included$INTERSECTION_ID,
              removeMarker(layerId = paste("marker", current_id, sep = "_"), map = .),
              addCircleMarkers(lng = current$lng, lat = current$lat, radius = 5, fillOpacity = 0,
                               stroke = TRUE, color = "red",
                               layerId = paste("marker", current_id, sep = "_"), map = .))}

    })

  # track which marker is hovered over
  ped_crash_current <- reactiveValues(hovered = NULL)

  # when the user hovers over a particular marker
  observeEvent(input$ped_crash_map_leaflet_marker_mouseover, {
    marker <- input$ped_crash_map_leaflet_marker_mouseover
    if (is.null(marker$id)) return()

    if (str_starts(marker$id, "hovered")) {
      marker$id <- str_split(string = marker$id, pattern = "_")[[1]][2]
    }

    ped_crash_hovered <- ped_crash %>%
      filter(FID == as.integer(marker$id))

    leafletProxy("ped_crash_map_leaflet") %>%
      #clearControls() %>%
      #clearPopups() %>%
      removeMarker(layerId = ped_crash_current$hovered) %>%
      addCircleMarkers(data = ped_crash_hovered, radius = 5, fillOpacity = 1,
                       stroke = TRUE, color = ~pal_ped_crash(PATTERN), layerId = paste("hovered", marker$id, sep = "_"))
      #addPopups(data = ped_crash_hovered, popup = ~htmlEscape(PATTERN))

    ped_crash_current$hovered <- paste("hovered", marker$id, sep = "_")

  })

  # when the user selects a particular intersection
  observeEvent(list(input$mymap_marker_click, input$intersection_data), {
    marker <- input$mymap_marker_click
    if (is.null(marker$id)) return()

    if (str_starts(marker$id, "marker")) {
      marker$id <- str_split(string = marker$id, pattern = "_")[[1]][2]
    }

    selected <- indices$combined %>% filter(INTERSECTION_ID == as.integer(marker$id))
    intersection_name <- indices$inters %>% filter(INTERSECTION_ID == as.integer(marker$id)) %>% slice(1L) %>% pull(NAME)
    output$intersection_id <- renderText(paste("Intersection ID:", marker$id, paste0("(", selected$COMBINED_RANK_PERCENTILE, "%)"), sep = " "))
    output$intersection_name <- renderText(intersection_name)

    output$pie <- renderPlot({
      piedonut(marker$id, indices$PPI, indices$PDI)
    }, bg = "transparent")

    output$ped_aadt <- renderPlot({
      sub_AADT <- isolate(indices$pred_ped_AADT) %>%
        filter(INTERSECTION_ID == marker$id)

    ggplot() +
        theme_minimal() +
        geom_errorbar(data = sub_AADT, aes(x = TYPE, ymin = LOWER, ymax = UPPER),
                      width = 0.25, size = 1, color = "#6180A7") +
        geom_point(data = sub_AADT, aes(x = TYPE, y = MEAN),
                   size = 5, shape = 21, fill = "white", color = "#6180A7") +
        coord_flip() +
        labs(title = "Pedestrian AADT") +
        theme(axis.title = element_blank(),
              plot.margin = margin(),
              plot.title = element_text(hjust = 0.5))

    })

    output$traffic_aadt <- renderPlot({
      sub_AADT <- isolate(indices$pred_traffic_AADT) %>%
        filter(INTERSECTION_ID == marker$id)

      ggplot() +
        theme_minimal() +
        geom_errorbar(data = sub_AADT, aes(x = TYPE, ymin = LOWER, ymax = UPPER),
                      width = 0.25, size = 1, color = "#DB9846") +
        geom_point(data = sub_AADT, aes(x = TYPE, y = MEAN),
                   size = 5, shape = 21, fill = "white", color = "#DB9846") +
        coord_flip() +
        labs(title = "Vehicle AADT") +
        theme(axis.title = element_blank(),
              plot.margin = margin(),
              plot.title = element_text(hjust = 0.5))

    })

    output$rank_plot <- renderPlot({
      ranks <- isolate(indices$combined) %>%
        filter(INTERSECTION_ID == marker$id) %>%
        select(INTERSECTION_ID, PPI_RANK_PERCENTILE, PDI_RANK_PERCENTILE, COMBINED_RANK_PERCENTILE) %>%
        left_join(indices$pred_ped_AADT_rank, by = c("INTERSECTION_ID")) %>%
        rename(PED_AADT_RANK_PERCENTILE = RANK_PERCENTILE) %>%
        left_join(indices$pred_traffic_AADT_rank, by = c("INTERSECTION_ID")) %>%
        rename(TRAFFIC_AADT_RANK_PERCENTILE = RANK_PERCENTILE) %>%
        pivot_longer(!INTERSECTION_ID, names_to = "Rank", values_to = "Percentile")

      ranks %>%
        ggplot(aes(x = factor(Rank, levels = c("COMBINED_RANK_PERCENTILE", "PPI_RANK_PERCENTILE", "PDI_RANK_PERCENTILE", "PED_AADT_RANK_PERCENTILE", "TRAFFIC_AADT_RANK_PERCENTILE")), y = Percentile, color = Rank, label = Percentile)) +
        theme_minimal() +
        geom_point(size = 2, shape = 21, fill = "transparent") +
        scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 10)) +
        scale_x_discrete("",
                         labels = c("PPI_RANK_PERCENTILE" = "PPI",
                                    "PDI_RANK_PERCENTILE" = "PDI",
                                    "COMBINED_RANK_PERCENTILE" = "Combined",
                                    "TRAFFIC_AADT_RANK_PERCENTILE" = "Vehicle",
                                    "PED_AADT_RANK_PERCENTILE" = "Pedestrian")) +
        scale_color_manual(values = c("COMBINED_RANK_PERCENTILE" = "#323E36",
                                      "PPI_RANK_PERCENTILE" = "#096772",
                                      "PDI_RANK_PERCENTILE" = "#7DA6BC",
                                      "PED_AADT_RANK_PERCENTILE" = "#D8896A",
                                      "TRAFFIC_AADT_RANK_PERCENTILE" = "#C24624")) +
        geom_text(vjust = 0, nudge_y = ifelse(ranks$Percentile >= 50, -10, 5)) +
        theme(plot.margin = margin(),
              legend.position = "none",
              panel.grid.minor.y = element_blank())
    })

    leafletProxy("mymap") %>%
      removeMarker(layerId = current$clicked) %>%
      addCircleMarkers(lng = marker$lng, lat = marker$lat, radius = 5, fillOpacity = 0,
                       stroke = TRUE, color = "red",
                       layerId = paste("marker", marker$id, sep = "_"))

    current$clicked <- paste("marker", marker$id, sep = "_")
    current$lng <- marker$lng
    current$lat <- marker$lat
  })

  # when the user search for a particular intersection using intersection ID
  observeEvent(input$search_intersection_id_go, {
    id <- input$search_intersection_id
    if (!is.integer(id)) {
      sendSweetAlert(
        session = session,
        title = "Error!",
        text = "Please only input an integer ID.",
        type = "error"
      )
      return()
    } else if (id %ni% indices$all_ids) {
      sendSweetAlert(
        session = session,
        title = "Error!",
        text = "The input ID does not exist.",
        type = "error"
      )
      return()
    }
    sum_all <- indices$combined

    output$pie <- renderPlot({
      piedonut(id, indices$PPI, indices$PDI)
    }, bg = "transparent")

    output$ped_aadt <- renderPlot({
      sub_AADT <- isolate(indices$pred_ped_AADT) %>%
        filter(INTERSECTION_ID == id)

      ggplot() +
        theme_minimal() +
        geom_errorbar(data = sub_AADT, aes(x = TYPE, ymin = LOWER, ymax = UPPER),
                      width = 0.25, size = 1, color = "#6180A7") +
        geom_point(data = sub_AADT, aes(x = TYPE, y = MEAN),
                   size = 5, shape = 21, fill = "white", color = "#6180A7") +
        coord_flip() +
        labs(title = "Pedestrian AADT") +
        theme(axis.title = element_blank(),
              plot.margin = margin(),
              plot.title = element_text(hjust = 0.5))

    })

    output$rank_plot <- renderPlot({
      ranks <- isolate(indices$combined) %>%
        filter(INTERSECTION_ID == id) %>%
        select(INTERSECTION_ID, PPI_RANK_PERCENTILE, PDI_RANK_PERCENTILE, COMBINED_RANK_PERCENTILE) %>%
        left_join(indices$pred_ped_AADT_rank, by = c("INTERSECTION_ID")) %>%
        rename(PED_AADT_RANK_PERCENTILE = RANK_PERCENTILE) %>%
        left_join(indices$pred_traffic_AADT_rank, by = c("INTERSECTION_ID")) %>%
        rename(TRAFFIC_AADT_RANK_PERCENTILE = RANK_PERCENTILE) %>%
        pivot_longer(!INTERSECTION_ID, names_to = "Rank", values_to = "Percentile")

      ranks %>%
        ggplot(aes(x = factor(Rank, levels = c("COMBINED_RANK_PERCENTILE", "PPI_RANK_PERCENTILE", "PDI_RANK_PERCENTILE", "PED_AADT_RANK_PERCENTILE", "TRAFFIC_AADT_RANK_PERCENTILE")), y = Percentile, color = Rank, label = Percentile)) +
        theme_minimal() +
        geom_point(size = 2, shape = 21, fill = "transparent") +
        scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 10)) +
        scale_x_discrete("",
                         labels = c("PPI_RANK_PERCENTILE" = "PPI",
                                    "PDI_RANK_PERCENTILE" = "PDI",
                                    "COMBINED_RANK_PERCENTILE" = "Combined",
                                    "TRAFFIC_AADT_RANK_PERCENTILE" = "Vehicle",
                                    "PED_AADT_RANK_PERCENTILE" = "Pedestrian")) +
        scale_color_manual(values = c("COMBINED_RANK_PERCENTILE" = "#323E36",
                                      "PPI_RANK_PERCENTILE" = "#096772",
                                      "PDI_RANK_PERCENTILE" = "#7DA6BC",
                                      "PED_AADT_RANK_PERCENTILE" = "#D8896A",
                                      "TRAFFIC_AADT_RANK_PERCENTILE" = "#C24624")) +
        geom_text(vjust = 0, nudge_y = ifelse(ranks$Percentile >= 50, -10, 5)) +
        theme(plot.margin = margin(),
              legend.position = "none",
              panel.grid.minor.y = element_blank())
    })

    output$traffic_aadt <- renderPlot({
      sub_AADT <- isolate(indices$pred_traffic_AADT) %>%
        filter(INTERSECTION_ID == id)

      ggplot() +
        theme_minimal() +
        geom_errorbar(data = sub_AADT, aes(x = TYPE, ymin = LOWER, ymax = UPPER),
                      width = 0.25, size = 1, color = "#DB9846") +
        geom_point(data = sub_AADT, aes(x = TYPE, y = MEAN),
                   size = 5, shape = 21, fill = "white", color = "#DB9846") +
        coord_flip() +
        labs(title = "Vehicle AADT") +
        theme(axis.title = element_blank(),
              plot.margin = margin(),
              plot.title = element_text(hjust = 0.5))

    })

    data <- neighbourhood %>%
      filter(NEIGHBOURHOOD %in% c("City Centre", "Cloverdale", "Fleetwood", "Guildford",
                                  "Newton", "South Surrey", "Whalley"))
    inters_included <- indices$inters %>%
      filter(NEIGHBOURHOOD %in% c("Cloverdale", "Fleetwood", "Guildford",
                                  "Newton", "South Surrey", "Whalley")) %>%
      left_join(indices$combined, by = "INTERSECTION_ID")

    selected_inter <- inters_included %>%
      filter(INTERSECTION_ID == as.integer(id))
    selected_long <- st_coordinates(selected_inter)[[1]]
    selected_lat <- st_coordinates(selected_inter)[[2]]

    intersection_name <- selected_inter %>% pull(NAME)

    colorScale <- colorFactor(palette = "viridis", indices$combined$COMBINED_SCORE)

    output$intersection_id <- renderText(paste("Intersection ID:", id, paste0("(", selected_inter$COMBINED_RANK_PERCENTILE, "%)"), sep = " "))
    output$intersection_name <- renderText(intersection_name)

    leafletProxy("mymap") %>%
      setView(lng = selected_long, lat = selected_lat, zoom = 14) %>%
      clearShapes() %>%
      clearControls() %>%
      removeMarker(layerId = as.character(all_ids)) %>%
      removeMarker(layerId = current$clicked) %>%
      addPolygons(color = "#141722", weight = 3, smoothFactor = 0.5,
                  fillOpacity = 0.1, opacity = 1, data = data) %>%
      addCircleMarkers(data = inters_included, radius = 1, fillOpacity = 1,
                       stroke = TRUE, color = ~colorScale(COMBINED_SCORE), layerId = ~as.character(INTERSECTION_ID)) %>%
      addCircleMarkers(data = selected_inter, radius = 5, fillOpacity = 0,
                       stroke = TRUE, color = "red",
                       layerId = paste("marker", id, sep = "_"))

    current$clicked <- paste("marker", id, sep = "_")
    current$lng <- selected_long
    current$lat <- selected_lat

  })

  expandMenuSubItemOnClick(PPIVars, "ppi")
  expandMenuSubItemOnClick(PDIVars, "pdi")

  ppiScores <- variableScoreServer("ppi", PPI_raw_filtered)
  pdiScores <- variableScoreServer("pdi", PDI_raw_filtered)

  # Update PDI and PPI according to user input
  ppi_saved <- saveVariableScoresServer("ppi", PPI_raw, ppiScores, all_ids_filtered)
  pdi_saved <- saveVariableScoresServer("pdi", PDI_raw, pdiScores, all_ids_filtered)

  observe({
    if (!is.null(ppi_saved$all) && !is.null(ppi_saved$filtered)) {
      PPI_default <<- ppi_saved$all
      PPI_default_filtered <<- ppi_saved$filtered
      if (input$intersection_data == "all") {
        indices$PPI <- PPI_default
      } else if (input$intersection_data == "filtered") {
        indices$PPI <- PPI_default_filtered
      }
    }
  })
  observe({
    if (!is.null(pdi_saved$all) && !is.null(pdi_saved$filtered)) {
      PDI_default <<- pdi_saved$all
      PDI_default_filtered <<- pdi_saved$filtered
      if (input$intersection_data == "all") {
        indices$PDI <- PDI_default
      } else if (input$intersection_data == "filtered") {
        indices$PDI <- PDI_default_filtered
      }
    }
  })

  output$ppi_table <- renderDataTable({
    DT::datatable(data = indices$PPI,
                  rownames = FALSE,
                  options = list(pageLength = 100,
                                 autoWidth = FALSE,
                                 scrollX = TRUE))
  })

  output$pdi_table <- renderDataTable({
    DT::datatable(data = indices$PDI,
                  rownames = FALSE,
                  options = list(pageLength = 100,
                                 autoWidth = FALSE,
                                 scrollX = TRUE))
  })

  output$ranked_table <- renderDataTable({
    sum_all <- indices$combined %>%
      select(INTERSECTION_ID, PPI_SCORE, PPI_RANK_PERCENTILE,
             PDI_SCORE, PDI_RANK_PERCENTILE,
             COMBINED_SCORE, COMBINED_RANK_PERCENTILE)

    DT::datatable(data = sum_all,
                  rownames = FALSE,
                  options = list(pageLength = 100,
                                 autoWidth = FALSE,
                                 scrollX = TRUE))
  })
}

shinyApp(ui, server)

