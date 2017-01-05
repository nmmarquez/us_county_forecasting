rm(list=ls())
library(sp)
library(data.table)
library(rgdal)
library(maptools)
library(leaflet)

folder <- "/home/j/Project/us_counties/locations/counties/prepped_shp/"
f_in <- paste0(folder, "mcnty_mapping_shape_file.rdata")
load(f_in)

mcnty_map@proj4string
nrow(mcnty_map@data)
p4s <- "+title=WGS 84 (long/lat) +proj=longlat +ellps=WGS84 +datum=WGS84 "#+units=degrees"

mcnty_map <- spTransform(mcnty_map, CRS(p4s))

mcnty_map@data$data <- rnorm(nrow(mcnty_map@data))


popup <- paste0("GEOID: ", mcnty_map@data$data)

pal <- colorNumeric(
    palette = "YlGnBu",
    domain = mcnty_map@data$data
)

map1<-leaflet() %>%
    addProviderTiles("CartoDB.Positron") %>%
    addPolygons(data = mcnty_map, 
                fillColor = ~pal(data), 
                color = "#b2aeae", # you need to use hex colors
                fillOpacity = 0.7, 
                weight = 0.3, 
                smoothFactor = 0.2,
                popup = popup)
map1
