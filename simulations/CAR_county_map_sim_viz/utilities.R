rm(list=ls())
library(sp)
library(data.table)
library(rgdal)
library(maptools)
library(leaflet)
library(surveillance)
library(spdep)
library(MASS)
library(sparseMVN)
library(Matrix)
library(ggplot2)

# these steps are slow and we always need this data so just doing it once
# and making the variables accessible globally
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

# translate to desired proj4 string
p4s <- "+title=WGS 84 (long/lat) +proj=longlat +ellps=WGS84 +datum=WGS84"
df2 <- spTransform(df, CRS(p4s))

# remove locs outside lower 48 + dc
remove_locs <- c("Puerto Rico"="72", "Hawaii"="15", "Alaska"="02", "Guam"="66",
                 "American Somoa"="60", "Virgin Islands"="78", "Mariana"="69")

df2 <- df2[!(df2@data$STATEFP %in% remove_locs),]

# remove islands with no neighbors
adj_mat <- poly2adjmat(df2)
islands <- which(rowSums(adj_mat) == 0)
df2 <- df2[-islands,]

US_POLY_DF <- df2
GRAPH <- poly2adjmat(US_POLY_DF)
OPTIONS <- c("Q_pCAR", "Q_lCAR")

Q_pCAR <- function(sigma, rho, graph){
    sigma**-1 * (diag(rowSums(graph)) - rho * graph)
}

Q_lCAR <- function(sigma, rho, graph){
    D <- diag(rowSums(graph))
    I <- diag(nrow(graph))
    sigma**-1 * (rho * (D - graph) + (1 - rho) * I)
}

sim_county_data <- function(seed, rho, sigma, method){
    set.seed(seed)
    Q <- get(method)(sigma, rho, GRAPH)
    obs <- rmvn.sparse(1, rep(0, nrow(Q)), Cholesky(Matrix(Q, sparse=TRUE)), T)
    US_POLY_DF@data$data <- c(obs)
    return(US_POLY_DF)
}

hist_plot <- function(df){
    ggplot(df@data, aes(x=data)) + geom_histogram(fill="seagreen") + 
        xlab("County Simulation Values") + ylab("Count")
}

ar_counties_map <- function(df){
    # pop up info
    popup <- paste0("County Name: ", df@data$NAME, "<br> Value: ", df@data$data)
    
    # color palette
    pal <- colorNumeric(palette="YlGnBu", domain=df@data$data)
    
    # see map
    map1<-leaflet() %>%
        addProviderTiles("CartoDB.Positron") %>%
        addPolygons(data=df, fillColor=~pal(data), color="#b2aeae", weight=0.3,
                    fillOpacity=0.7, smoothFactor=0.2, popup=popup) %>%
        addLegend("bottomright", pal=pal, values=df$data,
                  title = "Simulated Values", opacity = 1)
    map1
}