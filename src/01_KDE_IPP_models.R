###
### Creating KDE images and initial IPP Models
###
###  Claire Kelling and Manasvi Khanna
###    Date Created:  11/15/22
###    Last Modified: 11/15/22
###
###

#start clean
rm(list = ls())

### Set working directory
#Claire's System:
#setwd("C:/Users/ckell/Desktop/Google Drive/Research/farmers_protest")
#Manasvi's System:
setwd("/Users/manasvikhanna/Desktop/Farmer's Protest/farmers_protest_git")

### Packages
library(readxl)
library(rgdal)
library(tidyverse)
library(ggmap)
library(spatstat)
library(maptools)
library(dplyr)
library(tidyr)
library(tigris)
library(spdep)
library(ggplot2)
library(broom)
library(sp)
library(raster)
library(tidycensus)
library(maditr)
library(gridExtra)

#loading spatial data, cleaned in 00_processing_data.R (both areal and point-level data)
load(file = "data/working/areal_point_comp.Rdata")

#rename state shape
state_shape <- state_shape3

#for subclassification
copyFarmer <- farmers_protest
coordinates(copyFarmer) <- ~longitude+latitude 
class(copyFarmer)

# classify peaceful vs non peaceful into subsets for modelling comparisons
sub1 <- copyFarmer[copyFarmer$sub_event_type == "Peaceful protest",]
sub2 <- copyFarmer[copyFarmer$sub_event_type != "Peaceful protest",]
nrow(sub2)

# create intial projection of the state shape ds
ind_poly <- as(state_shape, "SpatialPolygons")
ind_poly <- spTransform(ind_poly, CRS("+init=epsg:24370")) #project them
ind_one <- st_transform(st_union(st_as_sf(ind_poly)), crs = CRS("+init=epsg:24370")) #comb
ind_win <- as.owin(as_Spatial(ind_one)) #transform into window

#coordinates(sub1) <- ~longitude+latitude
proj4string(sub1) <- proj4string(state_shape)
proj4string(sub2) <- proj4string(state_shape)

# reproject to match window
sub1 <- spTransform(sub1, CRS("+init=epsg:24370"))
sub2 <- spTransform(sub2, CRS("+init=epsg:24370"))
over_dat1 <- over(sub1, ind_poly)
over_dat2 <- over(sub2, ind_poly)
length(which(is.na(over_dat2))) #0: they are all in the window

# use the ppp (possion point process model) to create poisson point process object 
sub1_ppp <- as.ppp(sub1@coords, W = ind_win)
plot(sub1_ppp)

# use the ppp (poisson point process model) to create poisson point process object 
sub2_ppp <- as.ppp(sub2@coords, W = ind_win)
plot(sub2_ppp)

# Use the density funciton to get density from the ppp model 
kernel_density1 <- density(sub1_ppp)
plot(kernel_density1, main = "KDE: Protest Type: Peaceful")
plot(sub1_ppp, add = T)

kernel_density2 <- density(sub2_ppp)
plot(kernel_density2, main = "KDE: Protest Type: Not Peaceful")
plot(sub2_ppp, add = T)

# ~1 the model will look at the points at a longitude latitude level
ppm(sub1_ppp, ~1)
ppm(sub2_ppp, ~1)

# rescale to control for really high values
sub1rescaled <- rescale(sub1_ppp, 100000)
sub2rescaled <- rescale(sub2_ppp, 100000)

# fit rescaled subsets to point process model with a linear trend in coordinates
fit1 <- ppm(Q = sub1rescaled, trend = ~x + y)
#summary(fit1)
plot(fit1, main = "Protest Type: Not Peaceful")

fit2 <- ppm(Q = sub2rescaled, trend = ~x + y)
#summary(fit2)
plot(fit2)

#need to project state shape
state_shape <- spTransform(state_shape, CRS("+init=epsg:24370")) 

# create a grid (ind_poly instead of state_shape)
lattice <- expand.grid(Long = seq(state_shape@bbox[1,1],
                                  state_shape@bbox[1,2],length.out = 100), 
                       Lat = seq(state_shape@bbox[2,1],
                                 state_shape@bbox[2,2],length.out = 100))
dim(lattice)
plot(lattice)

# set grid to be similar to shape file 
sp_lattice <- lattice
coordinates(sp_lattice) <- ~Long+Lat
proj4string(sp_lattice) <- proj4string(state_shape)
overlap_set <- over(sp_lattice, state_shape)
lattice <- lattice[!(is.na(overlap_set$ST_NM)),]
#lattice <- lattice[!(is.na(overlap_set)),] #when using ind_poly
plot(lattice)

lattice <- cbind(lattice, overlap_set[-which(is.na(overlap_set$ST_NM)),])
#plot of gridded Census data
ggplot() + geom_point(data = lattice, aes(Long,Lat, col = X2020_pop ))
#remove grid points with NA for perc_renter
lattice <- lattice %>% filter(!is.na(X2020_pop))
#plot of gridded Census data without NA values
ggplot() + geom_point(data = lattice, aes(Long,Lat, col = X2020_pop)) + scale_colour_distiller(palette = "Spectral")

lattice$X2020_pop <- lattice$X2020_pop/100000
coordinates(lattice) <- ~Long+Lat
proj4string(lattice) <- proj4string(state_shape) 
#change from SpatialPointsDataFrame to SpatialPixelsDataFrame
class(lattice)
gridded(lattice) = TRUE
class(lattice)
#then change to SpatialGridDataFrame
lattice_grid <- as(lattice, "SpatialGridDataFrame")
class(lattice_grid)
#make into an "im" object
image(lattice_grid["X2020_pop"]) #plot first
pop_im <- as(lattice_grid["X2020_pop"], "im") #convert to im
plot(pop_im)
#need to rescale data, otherwise we get a numerical error
pop_im <- rescale(pop_im, 100000)
plot(pop_im)


# running into an error here, it is a projection issue!
ipp1 <- ppm(Q = sub1rescaled, trend = ~X2020_pop, 
            covariates = list(X2020_pop = pop_im))

summary(ipp1)
plot(ipp1)

ipp2 <- ppm(Q = sub2rescaled, trend = ~X2020_pop, 
            covariates = list(X2020_pop = pop_im))
summary(ipp2)
plot(ipp2)


