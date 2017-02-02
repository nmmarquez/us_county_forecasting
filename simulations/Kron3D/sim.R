rm(list=ls())
pacman::p_load(ar.matrix, TMB, data.table, sp, surveillance, Matrix)

load("~/Documents/county_forecasting/data/sp_data.RData")

# get unique demographic indicators iqnoring sex for now
geoid <- as.character(df2@data$GEOID)
age <- 2:21
year <- 1990:2015

length(geoid) * length(age) * length(year)

# create data table with all combinations of geoid, age, and year
DT <- as.data.table(expand.grid(GEOID=geoid, age=age, year=year))

# Create three precision matricies for each demographic then combine with kron
sigmas <- list(A=1.4, T=.7, L=1.2)
rhos <- list(A=.85, T=.99, L=.90)
N <- list(L=poly2adjmat(df2), A=length(age), T=length(year))
funcs <- list(L=Q.lCAR, A=Q.AR1, T=Q.AR1)
Qlist <- lapply(c(L="L", A="A", T="T"), function(x) 
    Matrix(funcs[[x]](N[[x]], sigmas[[x]], rhos[[x]]), sparse=TRUE))
Qlat <- kronecker(kronecker(Qlist$L, Qlist$A), Qlist$T)

system.time(DT[,y:=sim.AR(1, Qlat)])
