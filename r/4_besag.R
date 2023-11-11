#' 

### --- 0. Loading the packages --- ####
library(sf)
library(spdep)
library(lattice)
library(latticeExtra)
library(viridis)
library(gridExtra)
library(RColorBrewer)
library(INLA)
library(sf)
library(ggthemes)

#BiocManager::install(c("graph", "Rgraphviz"), dep=TRUE)
library(Rgraphviz)
library(graph)


### --- 1. Loading the data --- ####
#Dataset
data <- readRDS("data/london_suic/london_suicides.RDS")
Nareas <- length(data[,1])

#Spatial polygon data frame
london.gen <- read_sf("data/london_suic", "LDNSuicides")


### --- 2. Checking if the data of the sp and the data.frame match --- ####
# The order of the areas needs to be the same between 
# the data and the spatial polygon object obtained importing 
# the shapefile, so we re-order the data.
data$NAME == london.gen$NAME #Not the same
boroughs <- london.gen
data.boroughs <- attr(boroughs, "data")
order <- match(boroughs$NAME, data$NAME)
data <- data[order,]
data$NAME == london.gen$NAME

### ----- 2.1. Plotting the data --- ####
### ------- 2.1.1. SMR --- ####
london.gen$SMR_raw <- data$y/data$E 
SMR_raw.cutoff<- c(0.6, 0.9, 1.0, 1.1,  1.8)
SMR_raw_disc = cut(london.gen$SMR_raw,
               breaks         = SMR_raw.cutoff,
               include.lowest = TRUE)

london.gen$SMR_raw_disc <- SMR_raw_disc

plot(london.gen["SMR_raw_disc"], max.plot = 1,
     pal =  brewer.pal(9,'Blues')[c(2,4,6,8)],
     key.pos = 1)

### ------- 2.1.2. Covariates --- ####
# Covariates
london.gen$x1 <- data$x1
london.gen$x2 <- data$x2


plot(london.gen[c("x1")], max.plot = 1,
     key.pos = 1)

plot(london.gen[c("x2")], max.plot = 1,
     key.pos = 1)


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
plot_map_neig <- function(neig, london.gen) {
  
  # Plot the base map
  plot(st_geometry(london.gen), col = "white")
  
  # Highlight the selected region in red
  plot(st_geometry(london.gen[neig, ]), col = "red", add = TRUE)
  
  # Highlight neighbors in pink
  neighbors <- st_geometry(london.gen[temp[[neig]], ])
  plot(neighbors, col = "pink", add = TRUE)
  
  # Print information about the selected region and its neighbors
  cat("You have selected", london.gen$NAME[neig], "and its neighbors are:", "\n")
  cat(london.gen$NAME[temp[[neig]]], "\n")
}

plot_map_neig(neig = 30, london.gen)
plot_map_neig(neig = 25, london.gen)


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

#Mean posterior distribution
ggplot(data = london.gen) +
  geom_sf(aes(fill = SPmean), color = "white") +
  theme_map() +
  theme(legend.position="bottom",
        plot.title = element_text(hjust = 0.5,
                                  color = "Gray40",
                                  size = 16,
                                  face = "bold"),
        legend.title = element_blank(),
        plot.subtitle = element_text(color = "blue"),
        plot.caption = element_text(color = "Gray60")) +
  ggtitle("Mean posterior of S") 
  
#Sd posterior distribution
ggplot(data = london.gen) +
  geom_sf(aes(fill = SPsd), color = "white") +
  theme_map() +
  theme(legend.position="bottom",
        plot.title = element_text(hjust = 0.5,
                                  color = "Gray40",
                                  size = 16,
                                  face = "bold"),
        legend.title = element_blank(),
        plot.subtitle = element_text(color = "blue"),
        plot.caption = element_text(color = "Gray60")) +
  ggtitle("Mean posterior of S") 

### --- 4.4. Posterior distribution of suicides mortality --- ####
london.gen$SMR_mean <- mod.suicides$summary.fitted.values$mean # mean
london.gen$SMR_sd <- mod.suicides$summary.fitted.values$sd #s
london.gen$SMR_median <- mod.suicides$summary.fitted.values$`0.5quant` # median
london.gen$SMR_q025 <- mod.suicides$summary.fitted.values$`0.025quant` # quantile
london.gen$SMR_q975 <- mod.suicides$summary.fitted.values$`0.975quant` # quantile
london.gen$SMR_p1 <- 1 - mod.suicides$summary.fitted.values$`1cdf` # probability to be greater than 1


### --- 4.5. Posterior distribution of suicides SMR with cutoff--- ####
## Also, the probability for SMR to be greater than 1.
SMR.cutoff<- c(0.6, 0.9, 1.0, 1.1,  1.8)
SMR_p1.cutoff <- c(0,0.2,0.6, 0.8,1)

SMR_disc = cut(london.gen$SMR_mean,
               breaks         = SMR.cutoff,
               include.lowest = TRUE)

SMR_p1_disc = cut(london.gen$SMR_p1,
               breaks         = SMR_p1.cutoff,
               include.lowest = TRUE)


london.gen$SMR_disc <- SMR_disc
london.gen$SMR_p1_disc <- SMR_p1_disc

plot(london.gen["SMR_disc"],
     pal = brewer.pal(9,'Blues')[c(2,4,6,8)],
     main = "Fitted SMR")

plot(london.gen["SMR_p1_disc"],
     pal = brewer.pal(9,'Blues')[c(2,4,6,8)],
     main = "p(SMR>1)")

           

### --- 5. Excercise --- ###
#Now, add the covariates (deprivation - x1 and social fragmentation - x2) and 
#repeat the steps