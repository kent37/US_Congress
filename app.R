# Shiny app to show US House committee assignments
library(shiny)
library(tidyverse)
library(leaflet)

source('Readers.R')
source('Mapper.R')

# Read representatives and districts
reps = read_reps()
if (file.exists('data/bounds.RData')) {
  load('data/bounds.RData')
} else {
  bounds = make_boundaries(reps)
}

# Read committee assignments
comm_xref = house_committee_xref() %>% mutate(
  link = paste0('<a href="', url, '" target="_blank">', name, '</a>')
)

# Make mouseover label, dropdown entry and popup text for reps
bounds$label = paste(bounds$official_full, bounds$code, sep=', ')
bounds$lookup = paste(bounds$last, ', ', bounds$first, ' (', bounds$code, ')', sep='')

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

# Back end
server <- function(input, output, session) {
  # Base map
  bbox = bounds@bbox
  output$map <- renderLeaflet({
    leaflet() %>% addProviderTiles('Esri.WorldTopoMap') %>% 
      fitBounds(bbox['x', 'min'], bbox['y', 'min'], 
                -66.9513812, bbox['y', 'max'])
  })

  # Zoom level, stratified to just differences we care about
  level = reactiveVal(1)
  
  # Selected reps and district boundaries
  selected_bounds = reactiveVal(bounds)
  
  # Watch for zoom changes. Only propagate them when level changes.
  observe({
    level_ = if (is.null(input$map_zoom) || input$map_zoom <= 5) 1
      else if (input$map_zoom <= 7) 2
      else 3
    if (level() != level_) level(level_)
  })
  
  # Watch for committee changes
  observe({
    if (!is.null(input$comms) && input$comms != '') {
      selected_comms = comm_xref %>% filter(name %in% input$comms)
      if (nrow(selected_comms) > 0) {
        selected_bounds(bounds[bounds$bioguide %in% selected_comms$bioguide,])
        updateSelectizeInput(session, 'rep', selected='')
      }
      else(selected_bounds(bounds))
    }
  })
  
  # Watch for rep changes
  observe({
    if (!is.null(input$rep) && input$rep != '') {
      selected_bounds(bounds[bounds$lookup == input$rep,])
      updateSelectizeInput(session, 'comms', selected='')
    }
  })
  
  # Update the map if level or selected_bounds changes
  observe({
    
    # Map style changes depending on zoom level
    if (level() == 1) {
      # Zoomed out, make everything bold and dark
      weight = 4
      opacity = 1
      fillOpacity = 0.6
      highlightWeight = 5
      highlightFillOpacity = 0.6
    } else {
      # Lighten up when zoomed it
      weight = 2
      opacity = 0.5
      highlightWeight = 3
      # At max zoom make the fill light enough that the map can be read
      fillOpacity = if (level() == 3) 0.1 else 0.4
      highlightFillOpacity = if (level()==3) 0.1 else 0.6
    }
    
    # Add to the map. Add Democrats second, their districts tend to be small.
    selected = selected_bounds()
    leafletProxy('map') %>% 
      clearShapes()
    
    if (sum(selected$party != 'Democrat') > 0) {
    leafletProxy('map') %>% 
    addPolygons(data=selected[selected$party!='Democrat',],
                color=~color, 
                fillColor=~color,  opacity=opacity,
                weight=weight, fillOpacity=fillOpacity,
                label=~label, 
                popup=~popup,
                highlightOptions = 
                highlightOptions(opacity=1, fillOpacity=highlightFillOpacity,
                                 weight=highlightWeight, bringToFront = TRUE))
      }
    if (sum(selected$party == 'Democrat') > 0) {
    leafletProxy('map') %>% 
    addPolygons(data=selected[selected$party=='Democrat',],
                color=~color, 
                fillColor=~color,  opacity=opacity,
                weight=weight, fillOpacity=fillOpacity,
                label=~label, 
                popup=~popup,
                highlightOptions = 
                highlightOptions(opacity=1, fillOpacity=highlightFillOpacity,
                                 weight=highlightWeight, bringToFront = TRUE))
    }
  })
}

# Front end
ui <- navbarPage(title = "Committees of the US House of Representatives",
                 inverse=TRUE,
    # Bootswatch style sheet
    tags$head(tags$link(rel = "stylesheet", type = "text/css", 
href = "https://maxcdn.bootstrapcdn.com/bootswatch/3.3.7/spacelab/bootstrap.min.css")),
  tabPanel(""),
  fluidRow(
    column(6, offset=1,
      selectizeInput('comms', 'Choose a committee:', 
               c('', unique(comm_xref$name)), multiple = FALSE,
               options = NULL, width='400px',
               selected="House Permanent Select Committee on Intelligence")),
    column(5,
      selectizeInput('rep', 'Choose a representative:', 
               c('', sort(bounds$lookup)), multiple = FALSE,
               options = NULL, width='400px'))
  ),
  fluidRow(
      leafletOutput('map', width='95%', height=600)
  )
)

shinyApp(ui = ui, server = server)
