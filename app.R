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
comm_xref = house_committee_xref() %>% mutate(
  link = paste0('<a href="', url, '" target="_blank">', name, '</a>')
)

make_popup = function(d) {
  lapply(1:nrow(d),
     function(i) {
       row = d[i,]
 HTML(as.character(p(
   tags$a(tags$b(row$official_full), href=row$url, target='_blank'), br(),
   row$code, br(),
   row$phone, br(),
   HTML(paste(comm_xref$link[comm_xref$bioguide==row$bioguide], collapse='<br>'))
   )))
 })
}

bounds$popup = make_popup(bounds@data)

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
  
  output$zoom = renderText(input$map_zoom)
  
  # Zoom levels
  level = reactiveVal(1)
  
  # Watch for zoom changes. Only propagate them when level changes.
  observe({
      level_ = if (is.null(input$map_zoom) || input$map_zoom <= 5) 1
      else if (input$map_zoom <= 7) 2
      else 3
    if (level() != level_) level(level_)
  })
  
  observe({
    selected_comms = comm_xref %>% filter(name %in% input$comms)
    selected_bounds = bounds
    if (nrow(selected_comms) > 0)
      selected_bounds = selected_bounds[bounds$bioguide %in% selected_comms$bioguide,]
    
    extra_highlight = level() == 1
    if (level() == 1) {
      weight = 4
      opacity = 1
      fillOpacity = 0.6
      highlightWeight = 5
      highlightFillOpacity = 0.6
    } else {
      weight = 2
      opacity = 0.5
      highlightWeight = 3
      fillOpacity = if (level() == 3) 0.1 else 0.4
      highlightFillOpacity = if (level()==3) 0.1 else 0.6
    }
    
    # Add Democrats last, their districts tend to be small
    leafletProxy('map') %>% 
      clearShapes() %>% 
    addPolygons(data=selected_bounds[selected_bounds$party!='Democrat',],
                color=~color, 
                fillColor=~color,  opacity=opacity,
                weight=weight, fillOpacity=fillOpacity,
                label=~label, 
                popup=~popup,
                highlightOptions = 
                highlightOptions(opacity=1, fillOpacity=highlightFillOpacity,
                                 weight=highlightWeight, bringToFront = TRUE)) %>% 
    addPolygons(data=selected_bounds[selected_bounds$party=='Democrat',],
                color=~(if (extra_highlight) color else 'black'), 
                fillColor=~color,  opacity=opacity,
                weight=weight, fillOpacity=fillOpacity,
                label=~label, 
                popup=~popup,
                highlightOptions = 
                highlightOptions(opacity=1, fillOpacity=highlightFillOpacity,
                                 weight=highlightWeight, bringToFront = TRUE))

  })
}

ui <- navbarPage(title = "Committees of the US House of Representatives",
                 inverse=TRUE,
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", 
href = "https://maxcdn.bootstrapcdn.com/bootswatch/3.3.7/spacelab/bootstrap.min.css")),
  tabPanel(""),
  fluidRow(
    column(6, offset=1,
    selectizeInput('comms', 'Choose a committee:', 
               unique(comm_xref$name), multiple = FALSE,
               options = NULL, width='400px',
               selected="House Permanent Select Committee on Intelligence"))
  ),
  fluidRow(
      leafletOutput('map', width='95%', height=600)
  )
)


shinyApp(ui = ui, server = server)
