# setwd("/Users/khen/Documents/Keru_Git/US_stabilityzone/LandExplorer/")

library(shiny)
library(leaflet)
library(rgdal)
library(sp)
library(rmapshaper)
library(dplyr)

stabilityzone = read.csv("county_stability_zones.csv")

county_stability = stabilityzone[, c('region', 'subregion', 'zone1')] %>% 
  group_by(region, subregion) %>%
  summarise(first(zone1))

colnames(county_stability) = c('region', 'subregion', 'zone1')

## first tried to use existing county boundry shapefile -- very slow
county_boundry = readOGR(dsn = '.', layer="conus_state_county")

county_simplified <- ms_simplify(county_boundry, keep = 0.01, keep_shapes = TRUE)

county_simplified@data$region = tolower(county_simplified@data$State)
county_simplified@data$subregion = tolower(county_simplified@data$County)
county_simplified@data = merge(county_simplified@data, county_stability, by = c("region", 'subregion'),  all.x = T)

county_simplified$zone1 = factor(county_simplified$zone1,levels(county_simplified$zone1)[c(1,3,2, 4)])

ui <- fluidPage(
  titlePanel("Land Explorer"),
  
  fluidRow(
    mainPanel(
      tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}"),
      leafletOutput("map"), width = "100%", height = 1000)
  )
)

server <- function(input, output){
  
  pal <- colorFactor("RdYlGn", county_simplified$zone1, reverse = T)
  
  output$map <- renderLeaflet({
    
    leaflet() %>% 
      addTiles() %>%
      addPolygons(data = county_simplified, weight = 1,
                  color = "white",
                  dashArray = '3',
                  fillOpacity = 0.7,
                  fillColor = ~pal(zone1)) %>%
      addLegend(pal = pal, value = county_simplified$zone1, position = "bottomleft")
  })
  
}

shinyApp(ui = ui, server = server)
