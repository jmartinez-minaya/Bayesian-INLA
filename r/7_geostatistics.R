#' Geostatistics using INLA. Galicia
#' We present a database which has information about the presence/absence of a parasite
# (Fasciola) affecting cattle. The dataset has information about 400 animals. Information
# on the study region and demographic information of each animal is available. Our
# objective is find the model that best explain the occurrence of this parasite.


### --- 0. Loading libraries and functions --- ####

### --- Bayesian inference --- ###
library(INLA)
library(splancs)

### --- maps --- ###
library(lattice)
library(fields)
library(plotKML)

### --- spatial objects --- ###
library(raster)
library(rgdal)
library(sp)

### --- colors --- ####
library(viridis)

### --- function to convert matrix to raster --- ####
rotate <- function(x)(apply(x,2,rev))

matrix_to_raster <- function(m, proj.grid.mat = proj.grid.mat)
{
  raster(rotate(t(m)), 
         xmn = min(proj.grid.mat$lattice$loc[,1]), 
         xmx = max(proj.grid.mat$lattice$loc[,1]), 
         ymn = min(proj.grid.mat$lattice$loc[,2]), 
         ymx = max(proj.grid.mat$lattice$loc[,2]), 
         crs = proj4string(galicia_sp))   
}

### --- 1. Loading the data --- ####
galicia <- read.table("data/galicia_fasc/data_galicia2.txt", 
                      header=TRUE)

### --- Cartography of Galicia --- ###
galicia_sp <- readRDS("data/galicia_fasc/galicia_sp.rds")
plot(galicia_sp, border="blue")

### --- plot data --- ###
#png("images/data_galicia.png", width = 800, height = 700, res = 150)
par(mar=c(0,0,0,0))
plot(galicia_sp, lwd=2)
points(galicia[,1:2], 
       col = c("green4", "red4")[as.factor(galicia$InfFasc)],
       pch = 20, 
       cex = 1)
legend("topleft", legend=c("Absence", "Presence"), 
       pch=20, col=c("green4", "red4"), cex=1)
#dev.off()

### --- 2. Partition of the study area: mesh --- ####
### --- segment from spatial polygon --- ###
borinla <- inla.sp2segment(galicia_sp)

### --- mesh --- ###
mesh <- inla.mesh.2d(boundary  = borinla,  
                     max.edge  = c(10000, 20000),
                     cutoff    = 5000, 
                     min.angle = 30,
                     offset    = c(-0.9, -0.25))

### --- Plot the mesh --- ###
#png("images/mesh.png", width = 1000, height = 1000, res = 100)
par(mar=c(0,0,0,0))
#plot(galicia_sp)  
plot(mesh)
  #plot(galicia_sp, add=TRUE, lwd=3)
points(galicia[,1:2], 
       col = c("green4", "red4")[as.factor(galicia$InfFasc)],
       pch = 20, 
       cex = 1)  #plot(galicia_sp, add=TRUE, lwd=3)
#dev.off()


### --- 3. Definition of the SPDE --- ####
size <- min(c(diff(range(galicia$X)), diff(range(galicia$Y))))
range0 <- size / 2 	# ~ default




# - The way of setting these priors for sigma is that we do need to set sigma0 
# and p such that P(sigma > sigma 0) = p. In our example we will set 
# sigma0 = 1 and p = 0.01 and these values are passed to the inla.spde2.matern() 
# function as a vector in the next code. 
#
# - For the practical range parameter the setting is that we have to choose r0 
# and p such that P(r < r0) = p. We can set the PC-prior for 
# the median by setting p = 0.5 and in the next code we consider a prior median 
# equal to the diameter of the data divided by 2.

spde <- inla.spde2.pcmatern(
  # Mesh 
  mesh = mesh, 
  # P(practic.range < range0) = 0.5
  prior.range = c(range0, 0.5),
  # P(sigma > 1) = 0.01
  prior.sigma = c(1, 0.01)) 




### --- 4. Matrix which link data with the mesh. The projector matrix --- ####
A.est <- inla.spde.make.A(mesh, loc = cbind(galicia$X, galicia$Y))

### --- 5. Estimation --- ####
### --- inla.stack to stimate --- ###
stk.est <- inla.stack(data    = list(y = galicia$InfFasc),
                      A       = list(A.est, 1),
                      effects = list(spatial = 1:spde$n.spde,
                                     data.frame(beta0 = 1, 
                                                galicia)),
                      tag     = 'est')

### --- fitting the model --- ###
formula.1 <- y ~ -1 + beta0 + Temperature + f(spatial, model = spde)

model.est <- inla(formula.1,
                  data              = inla.stack.data(stk.est), 
                  family            = "binomial" ,
                  control.compute   = list(dic              = TRUE,
                                           cpo              = TRUE, 
                                           waic             = TRUE, 
                                           return.marginals = TRUE), 
                  control.predictor = list(A       = inla.stack.A(stk.est), 
                                           compute = TRUE),
                  control.fixed     = list(prec.intercept = 1),
                  #control.inla=list(strategy = "laplace"),
                  num.threads       = 2,
                  verbose           = FALSE)


saveRDS(model.est, "rds/model_est_gal.rds")
model.est <- readRDS("rds/model_est_gal.rds")
model.est$logfile
summary(model.est)

plot(model.est$marginals.hyperpar$`Range for spatial`)


### --- 6. Posterior distribution hyperpars --- ####
### --- Plot --- ###
par(mfrow = c(1,2), mar = c(3,3,1,0.5)) 
plot(model.est$marginals.hyperpar$`Range for spatial`, xlim = c(0, 100000), type = "l")
plot(model.est$marginals.hyperpar$`Stdev for spatial`, type='l', xlim = c(0, 2)) 


### --- 7. Interpolate the posterior mean and sd --- ####
### --- plot in a grid 50 X 50 --- ###
### --- Customize the grid to predict --- ###
bbox(galicia_sp)
(dxy <- apply(bbox(galicia_sp),1, diff))
(r <- dxy[1]/dxy[2])

m <- 50
#m <- 50

proj.grid.mat <- 
  inla.mesh.projector(mesh, 
                      xlim=bbox(galicia_sp)[1,] + c(-10000, +10000),
                      ylim=bbox(galicia_sp)[2,] + c(-10000, +10000),
                      dims=c(r, 1)*m)



plot(galicia_sp)
points(proj.grid.mat$lattice$loc, pch  = 20, cex = 0.5)

### --- clean (set NA to the values outside boundary) --- ###
ov <- over(SpatialPoints(proj.grid.mat$lattice$loc, galicia_sp@proj4string),
           galicia_sp)

### --- check grid points inside the map --- ###
i.map <- is.na(ov)

### Plot the points where we will predict ###
par(mar=c(0,0,0,0))
plot(galicia_sp)
points(proj.grid.mat$lattice$loc[!i.map,], col="red", cex=0.2)
points(proj.grid.mat$lattice$loc[i.map,], col="blue", cex=0.2)

### --- consider only those inside map --- ###
proj.grid.mat$lattice$loc[i.map, ]

### --- Project the values of the mean and sd of the spatial effect --- ###
mean.g <- inla.mesh.project(proj.grid.mat, model.est$summary.random$spatial$mean)
sd.g <- inla.mesh.project(proj.grid.mat, model.est$summary.random$spatial$sd)
sd.g[i.map] <- mean.g[i.map] <- NA

### --- Convert to raster --- ###
mean.g.r <- matrix_to_raster(m = mean.g, proj.grid.mat = proj.grid.mat)
sd.g.r <- matrix_to_raster(m = sd.g, proj.grid.mat = proj.grid.mat)

### --- visualize --- ###

#png("images/spatial.effect.png", width = 1000, height = 500, res = 90)

par(mfrow=c(1,2))
# image.plot(mean.g)
# image.plot(sd.g)

par(mar=c(0,0,0,6))
plot(galicia_sp)
plot(mean.g.r, 
     col = rev(viridis_pal(option = "B")(200)), 
     add = TRUE,
     legend.width  = 3, 
     legend.shrink = 0.7)

plot(galicia_sp, add = TRUE)

plot(galicia_sp)
plot(sd.g.r, col = rev(viridis_pal(option = "D")(200)), 
     add = TRUE, 
     legend.width  = 3, 
     legend.shrink = 0.7)
plot(galicia_sp, add = TRUE)

#dev.off()

### --- 8. Prediction --- ####
### --- Read the raster to predict --- ###
temperature <- raster("data/galicia_fasc/temperaturas_utm2")

#png("images/temperature.png", width = 1000, height = 700, res = 120)
par(mar=c(0,0,0,0))
plot(galicia_sp)
plot(temperature, 
     col = rev(heat.colors(100)),     
     legend.width  = 2, 
     legend.shrink = 0.7,
     add = TRUE)
plot(galicia_sp, add = TRUE)
#dev.off()

### --- Matrix which link the mesh with coordinates to predict--- ###
A.pred <- inla.spde.make.A(mesh, loc = proj.grid.mat$lattice$loc[!i.map, ])
i.temp <- is.na(extract(temperature, proj.grid.mat$lattice$loc))
A.pred <- inla.spde.make.A(mesh, loc = proj.grid.mat$lattice$loc[!i.temp, ])


### --- Stack to predict --- ###
stk.pred <- inla.stack(data      = list(y=NA),
                         A       = list(A.pred, 1), 
                         effects = list(spatial=1:spde$n.spde,
                                        data.frame(beta0 = 1, 
                                                   Temperature = extract(temperature, proj.grid.mat$lattice$loc[!i.temp, ]))),
                         tag     = 'pred')

stk <- inla.stack(stk.est, stk.pred)


#### --- model --- ###
model.pred <- inla(formula.1, 
                   data = inla.stack.data(stk), 
                   family="binomial",
                   control.predictor = list(A       = inla.stack.A(stk), 
                                            compute = TRUE, 
                                            link    = 1), #link:link is a vector of
                   #length given by the size of the response variable with values 1 if the corresponding
                   #data is missing and NA otherwise
                   control.inla      = list(strategy = "simplified.laplace"), # Strategy
                   control.mode      = list(theta = model.est$mode$theta, 
                                            restart = TRUE), #Mode 
                   # control.results   = list(return.marginals.random = FALSE,
                   #                    return.marginals.predictor  = FALSE), # Avoid some marginals
                   control.fixed     = list(prec.intercept = 1),
                   num.threads       = 4,
                   verbose  = TRUE)

saveRDS(model.pred, "rds/model_pred_galicia2.rds")
model.pred <- readRDS("rds/model_pred_galicia2.rds")

### index for the prediction data
idx <- inla.stack.index(stk, 'pred')$data

summary(model.pred$summary.fitted.values$mean[idx])

### --- Organize probabilities into a matrix to visualize --- ###
prob.mean <- prob.sd <- matrix(NA, proj.grid.mat$lattice$dims[1],
                               proj.grid.mat$lattice$dims[2])
prob.mean[!i.temp] <- c(model.pred$summary.fitted.val$mean[idx])
prob.sd[!i.temp] <- c(model.pred$summary.fitted.val$sd[idx])

prob.mean[!i.temp] <- c(model.pred$summary.fitted.val$mean[idx])
prob.sd[!i.temp] <- c(model.pred$summary.fitted.val$sd[idx])



#### --- plot --- ###
par(mfrow=c(1,2))
image.plot(prob.mean)
image.plot(prob.sd)



### --- Generate the raster --- ###
### --- mean --- ###
prob.mean.r <- matrix_to_raster(prob.mean, proj.grid.mat = proj.grid.mat)
prob.sd.r <- matrix_to_raster(prob.sd, proj.grid.mat = proj.grid.mat)


### --- Plotting --- ####

png("images/predictive_effect.png", width = 1000, height = 500, res = 90)

par(mfrow=c(1,2))
# image.plot(mean.g)
# image.plot(sd.g)

par(mar=c(0,0,0,6))
plot(galicia_sp)
plot(prob.mean.r, 
     col = rev(viridis_pal(option = "B")(200)), 
     add = TRUE,
     legend.width  = 3, 
     legend.shrink = 0.7)

plot(galicia_sp, add = TRUE)

plot(galicia_sp)
plot(prob.sd.r, col = rev(viridis_pal(option = "D")(200)), 
     add = TRUE, 
     legend.width  = 3, 
     legend.shrink = 0.7)
plot(galicia_sp, add = TRUE)
dev.off()


### --- plot in google earth --- ###
prob.mean.sg <- as(prob.mean.r, "SpatialGridDataFrame")
plotKML(prob.mean.sg, file.name = "galicia_pred.kml")


plot(prob.mean.sg)
### --- sd --- ###
prob.sd.sg <- as(prob.sd.r, "SpatialGridDataFrame")
plotKML(prob.sd.sg, file.name = "galicia_pred_sd.kml")



### --- 9. Exercise --- ####
# Arabidopsis thaliana is an important plant for genetists. We are interested in predict the behaviour of this
# species under different global climate change scenarios. In order to do so, we need to understand the distribution
# of this species in the present. We want to model the distribution of the species in terms of some covariates and
# modeling the spatial autocorrelation. 
# All the data jointly with the scripts are in https://zenodo.org/record/2552025#.Xb_zF9F7n80
### --- The data --- ####
data_arb <- read.csv2("data/spain_arabid/ath_accessions.csv", sep = ",", dec = ".", header = TRUE)
head(data_arb)
summary(data_arb)

### --- The spatial polygon --- ####
load("data/spain_arabid/penin_sp.RData")
plot(penin_sp)








