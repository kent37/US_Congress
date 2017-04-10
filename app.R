# Shiny app to show US House committee assignments
library(shiny)
library(tidyverse)
library(leaflet)

source('Readers.R')
source('Mapper.R')

# Read committee assignments
comm_xref = house_committee_xref() %>% mutate(
  link = paste0('<a href="', url, '" target="_blank">', name, '</a>')
)

# Read representatives and districts
reps = read_reps()
if (file.exists('data/bounds.RData')) {
  load('data/bounds.RData')
} else {
  bounds = make_boundaries(reps)
  
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
  save(bounds, file='data/bounds.RData')
}

# Make the lists for the two select inputs
# Using thomas_id for committee and bioguide code for reps keeps the URL
# length down
comm_list = local({
  comm_unique = unique(comm_xref %>% select(thomas_id, name))
  comm_unique$thomas_id %>% 
    set_names(comm_unique$name) %>% 
    `[`(order(comm_unique$name))
})

rep_list = bounds$bioguide %>% set_names(bounds$lookup) %>% `[`(order(bounds$lookup))

# Back end
server <- function(input, output, session) {
  # Update query string with bookmark
  observe({
    # Trigger this observer every time an input changes
    reactiveValuesToList(input)
    session$doBookmark()
  })
  onBookmarked(function(url) {
    updateQueryString(url)
  })
  setBookmarkExclude(c('map_bounds', 'map_shape_mouseout', 'map_shape_mouseover',
                       'map_shape_click'))
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
      selected_comms = comm_xref %>% filter(thomas_id %in% input$comms)
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
      selected_bounds(bounds[bounds$bioguide %in% input$rep,])
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
    bbox = selected@bbox
    leafletProxy('map') %>% 
      clearShapes() %>% 
      fitBounds(bbox['x', 'min'], bbox['y', 'min'], 
                min(bbox['x', 'max'], -66.9513812), bbox['y', 'max'])

    
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
ui <- function(request) {
  navbarPage(title = "Committees of the US House of Representatives",
                 inverse=TRUE, selected='Map',
    # Bootswatch style sheet
    tags$head(tags$link(rel = "stylesheet", type = "text/css", 
href = "https://maxcdn.bootstrapcdn.com/bootswatch/3.3.7/spacelab/bootstrap.min.css")),
  tabPanel("Map",
  fluidRow(
    column(6, offset=1,
      selectizeInput('comms', 'Choose a committee:', 
               c('', comm_list), multiple = FALSE,
               options = NULL, width='400px',
               selected="HLIG")),
    column(5,
      selectizeInput('rep', 'Choose a representative:', 
               c('', sort(rep_list)), multiple = TRUE,
               options = NULL, width='400px'))
  ),
  fluidRow(
      leafletOutput('map', width='95%', height=600)
  )
  ),
  tabPanel('About',
           p('Information about Members of Congress from', 
             a('https://github.com/unitedstates/congress-legislators',
               href='https://github.com/unitedstates/congress-legislators', 
               target='_blank'), '.'
           ),
           p('District boundaries from  US Census', 
             a('TIGER/Line shapefiles', 
               href='https://www.census.gov/geo/maps-data/data/tiger-line.html',
               target='_blank'),
             'for the 115th Congress.', 
             'Note: The boundaries were simplified using',
             a('rmapshaper', 
               href='https://cran.r-project.org/web/packages/rmapshaper/index.html',
               target='_blank'), 'so they are not exact.'),
           p('Mapping by Kent S Johnson for', 
             a('Indivisible Somerville', 
               href='https://indivisiblesomerville.org', target='_blank'), '.')
           )
)}

shinyApp(ui = ui, server = server, enableBookmarking='url')
