rm(list=ls())
library(data.table)
library(ggplot2)
library(scales)
library(gridExtra)
library(RColorBrewer)
library(ggthemes)
library(INLA)
library(RandomFields)

# Simulate locations
loc_xy = cbind( "x"=runif(10), "y"=runif(10))

# create mesh
mesh = inla.mesh.create( loc_xy, plot.delay=NULL, refine=FALSE)

# Create matrices in INLA
spde <- inla.spde2.matern(mesh, alpha=2)

par( mfrow=c(1,2), mar=c(0,0,2,0), mgp=c(2,0.5,0), tck=-0.02)
# Plot samples
plot( loc_xy, xlim=range(mesh$loc[,1]), ylim=range(mesh$loc[,2]), main="Sample locations")
# Plot mesh
text( x=mesh$loc[,1], y=mesh$loc[,2], labels=1:mesh$n, col=ifelse(1:mesh$n%in%mesh$idx$loc,"blue","black"))
title("Mesh composed of triangles")

par( mfrow=c(1,3), mar=c(2,2,2,0), mgp=c(2,0.5,0), tck=-0.02, xaxs="i", yaxs='i')
# Visualize SPDE approx.
Col = colorRampPalette(colors=c("blue","white","red"))
Points = function(X,Y,Z){
    DF = cbind( expand.grid('X'=as.vector(X), 'Y'=as.vector(Y)), 'Z'=as.vector(Z) )
    DF = DF[which(DF[,'Z']!=0),]
    points( x=DF[,'X'], y=DF[,'Y'], pch=20)
}
image(z=as.matrix(spde$param.inla$M0), x=1:mesh$n, y=1:mesh$n, main="M0", zlim=c(-1,1)*max(abs(spde$param.inla$M0)), col=Col(11), xlab="", ylab=""); box()
Points( X=1:mesh$n, Y=1:mesh$n, Z=as.matrix(spde$param.inla$M0))
image(z=as.matrix(spde$param.inla$M1), x=1:mesh$n, y=1:mesh$n, main="M1", zlim=c(-1,1)*max(abs(spde$param.inla$M1)), col=Col(11), xlab="", ylab=""); box()
Points( X=1:mesh$n, Y=1:mesh$n, Z=as.matrix(spde$param.inla$M1))
image(z=as.matrix(spde$param.inla$M2), x=1:mesh$n, y=1:mesh$n, main="M2", zlim=c(-1,1)*max(abs(spde$param.inla$M2)), col=Col(11), xlab="", ylab=""); box()
Points( X=1:mesh$n, Y=1:mesh$n, Z=as.matrix(spde$param.inla$M2))

###################
# Show Matern correlation function
###################

Matern_Correlation = function( distance_vec, Nu, Scale ){
    Correlation = 2^(1-Nu) * gamma(Nu)^(-1) * (sqrt(2*Nu)*distance_vec/Scale)^Nu * besselK(sqrt(2*Nu) * distance_vec/Scale, nu=Nu)
    return( Correlation )
}

# Distance with 10% correlation is approximately Scale/2
Distance = seq(0,3, length=1e4)
Nu = c(0.5, 1, 2, 10, 100)
Corr_Matrix = sapply( Nu, FUN=Matern_Correlation, distance_vec=Distance, Scale=1)

# Plot
par( mar=c(2,2,1,0), mgp=c(2,0.5,0), tck=-0.02, xaxs="i", yaxs='i')
matplot( y=Corr_Matrix, x=Distance, type="l", lwd=3, ylim=c(0,1.2), lty="solid", ylab="Correlation", xlab="Distance" )
abline( h=0.1, lty="dotted" )
legend( "topright", legend=Nu, fill=1:6, bty="n", title=expression(nu))
title( "Matern correlation function (Scale=1)" )


###################
# Simulate data
###################

Dim = c("n_x"=10, "n_y"=10)
loc_xy = expand.grid("x"=1:Dim['n_x'], "y"=1:Dim['n_y'])
Scale = 2
Sigma2 = (0.5) ^ 2
beta0 = 3
prob_missing = 0.2

# Simulate spatial process
RMmodel = RMgauss(var=Sigma2, scale=Scale)
epsilon_xy = array(RFsimulate(model=RMmodel, x=loc_xy[,'x'], y=loc_xy[,'y'])@data[,1], dim=Dim)
image( z=epsilon_xy )

# SImulate counts
c_xy = array(NA, dim=dim(epsilon_xy))
for(x in 1:nrow(c_xy)){
    for(y in 1:ncol(c_xy)){
        c_xy[x,y] = rpois(1, exp(beta0 + epsilon_xy[x,y]) )
        if( rbinom(n=1, size=1, prob=prob_missing)==1) c_xy[x,y] = NA
    }}
true_abundance =  sum( exp(beta0 + epsilon_xy) )
head(c_xy)

