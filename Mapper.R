# Make maps

library(htmltools)
library(leaflet)
library(sp)
source('Readers.R')

# Map by district
to_map = read_reps() %>% by_district(districts)

# Map by committee
to_map = read_reps() %>% by_committee('HLIG')

map_reps = function(to_map) {
  fips = read_fips()
   to_map_join = to_map %>% 
    left_join(fips, by=c(state='USPS'))
  
  # boundaries = raster::shapefile('data/tl_2016_us_cd115/tl_2016_us_cd115.shp')
  # bsimp = rmapshaper::ms_simplify(boundaries)
  # raster::shapefile(bsimp, 'data/cd115_simp/cd115_simp.shp')
  boundaries = raster::shapefile('data/cd115_simp/cd115_simp.shp')
  bounds = merge(boundaries, to_map_join, 
                     by.x=c('STATEFP', 'CD115FP'), by.y=c('FIPS', 'district'),
                     all.x=FALSE) %>% 
    spTransform(CRS("+init=EPSG:4326"))
  
  make_popup = function(d) {
    lapply(1:nrow(d),
       function(i) {
         row = d[i,]
   HTML(as.character(p(
     tags$a(tags$b(row$official_full), href=row$url, target='_blank'), br(),
     row$code, br(),
     row$phone
     )))
   })
  }
  
  bounds$popup = make_popup(bounds@data)
  bounds$color = ifelse(bounds$party=='Democrat', 'blue', 
                        ifelse(bounds$party=='Republican', 'red', 'darkyellow'))
  
  leaflet(bounds) %>% addProviderTiles('Esri.WorldTopoMap') %>% 
    addPolygons(color='black', fillColor=~color, weight=2, fillOpacity=0.4,
                label=~paste(official_full, code, sep=', '), popup=~popup,
                highlightOptions = 
                highlightOptions(opacity=1, fillOpacity=0.6,
                                 weight=3, bringToFront = TRUE))
}
