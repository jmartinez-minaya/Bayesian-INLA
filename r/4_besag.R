#' 

### --- 0. Loading the packages --- ####
library(maptools)
library(rgdal)
library(spdep)
library(lattice)
library(latticeExtra)
library(viridis)
library(gridExtra)
library(RColorBrewer)
library(INLA)

#BiocManager::install(c("graph", "Rgraphviz"), dep=TRUE)
library(Rgraphviz)
library(graph)


### --- 1. Loading the data --- ####
#Dataset
data <- readRDS("data/london_suic/london_suicides.RDS")
Nareas <- length(data[,1])

#Spatial polygon data frame
london.gen <- readOGR("data/london_suic", "LDNSuicides")


### --- 2. Checking if the data of the sp and the data.frame match --- ####
# The order of the areas needs to be the same between 
# the data and the spatial polygon object obtained importing 
# the shapefile, so we re-order the data.
data$NAME == london.gen$NAME #Not the same
boroughs <- london.gen
data.boroughs <- attr(boroughs, "data")
order <- match(data.boroughs$NAME,data$NAME)
data <- data[order,]
data$NAME == london.gen$NAME

### ----- 2.1. Plotting the data --- ####
london.gen$SMR_raw <- data$y/data$E 
SMR_raw.cutoff<- c(0.6, 0.9, 1.0, 1.1,  1.8)
SMR_raw_disc = cut(london.gen$SMR_raw,
               breaks         = SMR_raw.cutoff,
               include.lowest = TRUE)

london.gen$SMR_raw_disc <- SMR_raw_disc

#png("images/SMR_raw.png", width = 1000, height = 800, res = 150)
spplot(london.gen,
       c("SMR_raw_disc"),
       col.regions = brewer.pal(9,'Blues')[c(2,4,6,8)],
       main        = "SMR raw",
       par.settings =
         list(axis.line = list(col =  'transparent')))

#dev.off()

         
         
### --- 3. Defining neighbor relation --- ####
temp <- poly2nb(london.gen)

#This create a file called ``LDN.graph'' with the graph for INLA
nb2INLA("data/london_suic/LDN.graph", temp)

### ----- 3.1. Plotting the generated graph --- ####
H <- inla.read.graph(filename="data/london_suic/LDN.graph")
image(inla.graph2matrix(H),xlab="",ylab="")

### ----- 3.2. More plotting --- ####
plot(H)

### ----- 3.3. Plotting the neighbors --- ####
plot_map_neig <- function(neig)
{
  plot(london.gen)
  plot(london.gen[neig, ], border="white", 
       col="red", add=TRUE)
  
  plot(london.gen[temp[[neig]], ], 
       border="white", 
       col="pink", add=TRUE)
  london.gen[temp[[neig]],]$NAME
}

plot_map_neig(30)

### --- 4. Fitting a model with bym effect --- ####
### ----- 4.0. Adding ids for the random effects --- ####
S <- U <- seq(1,32)
data <- cbind(data, S, U)


### ----- 4.1. Formula --- ####
formula <- y ~ 1 + f(S, 
                     model       = "besag", 
                     graph       = H,
                     scale.model = TRUE,
                     hyper       = 
                       list(prec = list(prior="loggamma",param = c(1,0.001)))) +
  f(U, 
    model       = "iid",
    hyper       = 
      list(prec = list(prior="loggamma",param = c(1,0.001))))

### ----- 4.2. Model --- ####
mod.suicides <- inla(formula,
                     family          = "poisson",
                     data            = data,
                     E               = E,
                     control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE),
                     control.predictor = list(compute=TRUE, cdf=c(log(1))))

summary(mod.suicides)


### ----- 4.3. Posterior distribution of the random effects --- ####
london.gen$SPmean <- round(mod.suicides$summary.random$S[["mean"]], 4)
london.gen$SPsd <- round(mod.suicides$summary.random$S[["sd"]],5)

#png("images/random_effect_spatial_bym.png", width = 1500, height = 700, 
#    res = 100)
grid.arrange(
  spplot(london.gen, c("SPmean"),
         main = c("Mean posterior of S"),
         #col.regions = rev(viridis_pal(option = "B")(101)),
         col.regions = colorRampPalette(brewer.pal(9,'Blues'))(101),
         cuts        = 100,
         colorkey=list(space="bottom", space = "bottom"),
         par.settings =
           list(axis.line = list(col =  'transparent',
                                 legend.ticks = 'black'))),
  spplot(london.gen, c("SPsd"),
         main = c("Sd posterior of S"),
         col.regions = colorRampPalette(brewer.pal(9,'Blues'))(101),
         cuts        = 100,
         colorkey=list(space="bottom", space = "bottom"),
         par.settings =
           list(axis.line = list(col =  'transparent',
                                 legend.ticks = 'black'))),
  ncol = 2)
#dev.off()

### --- 4.4. Posterior distribution of suicides mortality --- ####
london.gen$SMR_mean <- mod.suicides$summary.fitted.values$mean # mean
london.gen$SMR_sd <- mod.suicides$summary.fitted.values$sd #s
london.gen$SMR_median <- mod.suicides$summary.fitted.values$`0.5quant` # median
london.gen$SMR_q025 <- mod.suicides$summary.fitted.values$`0.025quant` # quantile
london.gen$SMR_q975 <- mod.suicides$summary.fitted.values$`0.975quant` # quantile
london.gen$SMR_p1 <- 1 - mod.suicides$summary.fitted.values$`1 cdf` # probability to be greater than 1

#png("images/SMR_bym.png", width = 1500, height = 600, 
#    res = 100)
grid.arrange(spplot(london.gen,
                    c("SMR_mean"),
                    col.regions = colorRampPalette(brewer.pal(9,'Blues'))(101),
                    cuts         = 100,
                    main        = "SMR mean ",
                    colorkey=list(space="bottom"),
                    par.settings =
                      list(axis.line = list(col =  'transparent'))),
             spplot(london.gen,
                    c("SMR_sd"),
                    col.regions = colorRampPalette(brewer.pal(9,'Blues'))(101),
                    cuts         = 100,
                    main        = "SMR sd ",
                    colorkey=list(space="bottom"),
                    par.settings =
                      list(axis.line = list(col =  'transparent'))), ncol = 2)
             
#dev.off()


### --- 4.5. Posterior distribution of suicides SMR with cutoff--- ####
## Also, the probability for SMR to be greater than 1.
SMR.cutoff<- c(0.6, 0.9, 1.0, 1.1,  1.8)
SMR_p1.cutoff <- c(0,0.2,0.8,1)

SMR_disc = cut(london.gen$SMR_mean,
               breaks         = SMR.cutoff,
               include.lowest = TRUE)

SMR_p1_disc = cut(london.gen$SMR_p1,
               breaks         = SMR_p1.cutoff,
               include.lowest = TRUE)


london.gen$SMR_disc <- SMR_disc
london.gen$SMR_p1_disc <- SMR_p1_disc

#png("images/SMR_bym_disc.png", width = 1500, height = 600, res = 150)
grid.arrange(spplot(london.gen,
                    c("SMR_disc"),
                    col.regions = brewer.pal(9,'Blues')[c(2,4,6,8)],
                    main        = "SMR ",
                    par.settings =
                      list(axis.line = list(col =  'transparent'))),
             spplot(london.gen,
                    c("SMR_p1_disc"),
                    col.regions = brewer.pal(9,'Blues')[c(3,6,9)],
                    main        = "p(SMR > 1) ",
                    par.settings =
                      list(axis.line = list(col =  'transparent'))), ncol = 2)

#dev.off()

### --- 5. Excercise --- ###
#Now, add the covariates (deprivation - x1 and social fragmentation - x2) and 
#repeat the steps