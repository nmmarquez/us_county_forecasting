rm(list=ls())
library(sp)
library(data.table)
library(rgdal)
library(maptools)
library(leaflet)
library(surveillance)

# download the 2015 shape file from us census 
file_zip <- tempfile()
extr_dir <- tempdir() 
url_ <- "http://www2.census.gov/geo/tiger/GENZ2015/shp/cb_2015_us_county_5m.zip"
download.file(url_, file_zip)
unzip(file_zip, exdir=extr_dir)

# pull the shape file into mem
df <- readOGR(extr_dir)
unlink(file_zip)
unlink(extr_dir)

# check the current proj string
df@proj4string
# translate to desired proj4 string
p4s <- "+title=WGS 84 (long/lat) +proj=longlat +ellps=WGS84 +datum=WGS84"
df2 <- spTransform(df, CRS(p4s))

# remove locs outside lower 48 + dc
remove_locs <- c("Puerto Rico"="72", "Hawaii"="15", "Alaska"="02", "Guam"="66",
                 "American Somoa"="60", "Virgin Islands"="78", "Mariana"="69")

df2 <- df2[!(df2@data$STATEFP %in% remove_locs),]
rownames(df2@data) <- 1:nrow(df2@data)

# do we have it?
length(unique(df2@data$STATEFP)) == 49

# remove islands with no neighbors
adj_mat <- poly2adjmat(df2)
islands <- which(rowSums(adj_mat) == 0)
df2 <- df2[-islands,]
rownames(df2@data) <- 1:nrow(df2@data)

# simulate some data to visualize
df2@data$data <- rnorm(nrow(df2@data))
# pop up info
popup <- paste0("County Name: ", df2@data$NAME, "<br> Value: ", df2@data$data)

# color palette
pal <- colorNumeric(
    palette = "YlGnBu",
    domain = df2@data$data
)

# see map
map1<-leaflet() %>%
    addProviderTiles("CartoDB.Positron") %>%
    addPolygons(data = df2, 
                fillColor = ~pal(data), 
                color = "#b2aeae", 
                fillOpacity = 0.7, 
                weight = 0.3, 
                smoothFactor = 0.2,
                popup = popup)
map1

# save data
df2@data$data <- NULL
save(df2, file="~/Documents/county_forecasting/data/sp_data.RData")
