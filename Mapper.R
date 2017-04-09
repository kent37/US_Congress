# Make maps

library(htmltools)
library(leaflet)
library(sp)
source('Readers.R')

# Map by district
#to_map = read_reps() %>% by_district(districts)

# Map by committee
#to_map = read_reps() %>% by_committee('HLIG')

make_boundaries = function(reps) {
  fips = read_fips()
   reps_join = reps %>% 
    left_join(fips, by=c(state='USPS'))
  
  # boundaries = raster::shapefile('data/tl_2016_us_cd115/tl_2016_us_cd115.shp')
  # bsimp = rmapshaper::ms_simplify(boundaries)
  # raster::shapefile(bsimp, 'data/cd115_simp/cd115_simp.shp')
  boundaries = raster::shapefile('data/cd115_simp/cd115_simp.shp')
  bounds = merge(boundaries, reps_join, 
                     by.x=c('STATEFP', 'CD115FP'), by.y=c('FIPS', 'district'),
                     all.x=FALSE) %>% 
    spTransform(CRS("+init=EPSG:4326"))
  
  bounds$color = ifelse(bounds$party=='Democrat', 'blue', 
                        ifelse(bounds$party=='Republican', 'red', 'darkyellow'))
  bounds
}

map_reps = function(to_map) {
  bounds = make_boundaries(to_map)
  leaflet(bounds) %>% addProviderTiles('Esri.WorldTopoMap') %>% 
    addPolygons(color='black', fillColor=~color, weight=2, fillOpacity=0.4,
                label=~paste(official_full, code, sep=', '), popup=~popup,
                highlightOptions = 
                highlightOptions(opacity=1, fillOpacity=0.6,
                                 weight=3, bringToFront = TRUE))
}
