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
    diag(mat) <- 1.
    mat * sigma**-1
}

load("~/Documents/county_forecasting/data/sp_data.RData")
df2 <- df2[df2@data$STATEFP %in% c("48", "22"),]
mat <- poly2adjmat(df2)

#Create Q from an adjacency matrix
n_delta_i <- rowSums(mat)
Q <- mat * -1
diag(Q) <- n_delta_i


# N <- nrow(df2@data)
# max_J <- max(rowSums(mat))
# rho_indv <- .999999999
# rho <- (rho_indv / max_J)
# sigma <- 1.2
# Q <- Q_iCAR(N, sigma, rho, mat)

Sigma <- ginv(Q)
obs <- rmvnorm(1, sigma=Sigma)

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
