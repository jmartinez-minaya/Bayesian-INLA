#' In this script, we see how to create a good mesh
#' First we will learn how to create a good mesh to set the SPDE model on it
#' A good mesh is REALLY important to avoid numerical problems when fitting the models
#' For this practical we dont need to understand what the data is, we are good knowing 
#' that the data is spatially distributed


library(INLA)


### --- 1. Reading the data --- ####
data <- read.table("data/data_spat/spat_data.txt", sep="\t")
str(data)


### --- 2. Creating some meshes --- ####
coords <- as.matrix(data[,c("lon","lat")])

### ----- 2.1. MESH 1 --- ####
### use data coordinates as starting points for mesh
## maximum triangle edge size of 0.3
m1 <- inla.mesh.2d(coords, max.edge = 0.3)
plot(m1)
points(coords,pch=16)


# !!! abrupt transitions between small and big tringles --> maybe numerical problems


### ----- 2.2. MESH 2 --- ####
## create a domain of the study area by hand
range(data$lon)
range(data$lat)
pl.dom <- cbind(c(0,1,1,1,0), c(0,0,1,1,1))

### use data coordinates as starting points for mesh
## maximum triangle edge size of 0.3
m2 <- inla.mesh.2d(loc.dom  = pl.dom, 
                   max.edge = 0.3)
plot(m2)
points(coords, pch = 16)


### ----- 2.3. MESH 3 --- ####
#### may we have an shapefile of the domain
sp <- readRDS("data/data_spat/square_sp.rds")
plot(sp)

### --- segment from spatial polygon --- ###
borinla <- inla.sp2segment(sp)

### --- mesh --- ###
m3 <- inla.mesh.2d(boundary = borinla,  
                   max.edge = c(0.3))
plot(m3)
points(coords,pch=16)
### better shape on tringles 
### !!! triangles are quite big with respect to distance between points 
### --> part of the spatial variability inside the nugget?


### ----- 2.4. MESH 4 --- ####
### lets make smaller triangles!!
m4 <- inla.mesh.2d(boundary = borinla, 
                   max.edge = 0.1)
plot(m4)
points(coords,pch=16)

#### better, important to find good compromise between number of of nodes 
#### in mesh and require precision in the spatial field




### ----- 2.5. MESH 5 --- ####
m5 <- inla.mesh.2d(boundary = borinla, 
                   max.edge = c(0.1,.3))
plot(m5)
points(coords, pch = 16)
### MUCH BETTER 
## !!! maybe range still bigger


### ----- 2.6. MESH 6 --- ####
m6 <- inla.mesh.2d(boundary = borinla, 
                 max.edge = c(0.1, .3),
                 offset   = c(0.2, -0.3))
plot(m6)
points(coords, pch = 16)
#### this one looks quite good. 
#### Maybe some small triangles in the corner


### ----- 2.7. MESH 7 --- ####
m7 <- inla.mesh.2d(boundary = borinla, 
                   max.edge = c(0.1, .3),
                   offset   = c(0.1, -0.3), 
                   cutoff   = c(0.05))
plot(m7)
points(coords,pch=16)
#### this one looks quite good. 
### You could increase the number 
### of nodes/tringles to increase accuracy but in big spatial 
### models this means an increase in computational time



### --- 3. Exercise --- ####
### Create a mesh using real data
### DATA
galicia <- read.table("data/galicia_fasc/data_galicia.txt")

### Cartography of Galicia
galicia_sp <- readRDS("data/galicia_fasc/galicia_sp.rds")

plot(galicia_sp)
