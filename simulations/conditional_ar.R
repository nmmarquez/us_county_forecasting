rm(list=ls())
library(sp)
library(data.table)
library(rgdal)
library(maptools)
library(leaflet)
library(surveillance)
library(INLA)
library(mvtnorm)
library(MASS)
library(sparseMVN)
library(Matrix)

Q_ar1 <- function(N, sigma, rho){
    Q <- matrix(0, nrow=N, ncol=N)
    Q[1,1] <- 1.
    for(i in 2:N){
        Q[i,i] <- 1 + rho**2
        Q[i-1,i] <- -1 * rho
        Q[i,i-1] <- -1 * rho
    }
    Q[N,N] <- 1.
    (1 / sigma**2) * Q
}


Q_iCAR <- function(N, sigma, rho, graph){
    mat <- graph 
    diag(mat) <- 0.
    mat <- mat * -rho
    diag(mat) <- rowSums(graph)
    mat * (sigma**-1)
}

load("~/Documents/county_forecasting/data/sp_data.RData")
df2 <- df2[df2@data$STATEFP %in% c("48", "22"),]
graph <- poly2adjmat(df2)

N <- nrow(df2@data)
rho <- .99
sigma <- .7
Q <- Q_iCAR(N, sigma, rho, graph)

system.time(obs <- rmvnorm(1, sigma=ginv(Q)))
print(mean(c(obs)))

# now with sparse matricies
system.time(obs <- rmvn.sparse(1, rep(0, nrow(Q)), 
                               Cholesky(Matrix(Q, sparse=TRUE)), T))
print(mean(c(obs)))

# simulate some data to visualize
df2@data$data <- c(obs)

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
