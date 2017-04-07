library(shiny)
library(tidyverse)
library(leaflet)
source('Readers.R')
source('Mapper.R')

reps = read_reps()
if (file.exists('data/bounds.RData')) {
  load('data/bounds.RData')
} else {
  bounds = make_boundaries(reps)

}
bounds$label = paste(bounds$official_full, bounds$code, sep=', ')
comm_xref = house_committee_xref()

server <- function(input, output) {
  bbox = bounds@bbox
  output$map <- renderLeaflet({
    leaflet(bounds) %>% addProviderTiles('Esri.WorldTopoMap') %>% 
      fitBounds(bbox['x', 'min'], bbox['y', 'min'], 
                -66.9513812, bbox['y', 'max']) %>% 
    addPolygons(color='black', 
                fillColor=~color, 
                weight=2, fillOpacity=0.4,
                label=~label, 
                popup=~popup,
                highlightOptions = 
                highlightOptions(opacity=1, fillOpacity=0.6,
                                 weight=3, bringToFront = TRUE))

  })
  
  observe({
    selected_comms = comm_xref %>% filter(name %in% input$comms)
    selected_bounds = bounds
    if (nrow(selected_comms) > 0)
      selected_bounds = selected_bounds[bounds$bioguide %in% selected_comms$bioguide,]
    leafletProxy('map', data=selected_bounds) %>% 
      clearShapes() %>% 
    addPolygons(color='black', 
                fillColor=~color, 
                weight=2, fillOpacity=0.4,
                label=~label, 
                popup=~popup,
                highlightOptions = 
                highlightOptions(opacity=1, fillOpacity=0.6,
                                 weight=3, bringToFront = TRUE))

  })
}

ui <- fluidPage(title = "Committees of the US House of Representatives",
  titlePanel("Committees of the US House of Representatives"),
  fluidRow(
    column(6, offset=1,
    selectizeInput('comms', 'Committee', comm_xref$name, multiple = FALSE,
               options = NULL, width='400px')
  )),
  fluidRow(
      leafletOutput('map', width='95%', height=600)
)
)


shinyApp(ui = ui, server = server)
