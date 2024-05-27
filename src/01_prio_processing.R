###
### Loading and Processing PRIO Data
###
###  Claire Kelling and Manasvi Khanna
###    Date Created:  02/22/23
###    Last Modified: 02/23/23
###
###

#start clean
rm(list = ls())

#Data source:
#download website: https://grid.prio.org/#/download

#Data dictionary/codebook:
#https://grid.prio.org/extensions/PRIO-GRID-Codebook.pdf

### Set working directory
#Claire's System:
#setwd("C:/Users/ckell/Desktop/Google Drive/Research/farmers_protest")
setwd("C:/Users/ckelling/Dropbox (Carleton College)/Carleton College/Research/farmers_protest")
#Manasvi's System:
# setwd("/Users/manasvikhanna/Desktop/Farmer's Protest/farmers_protest")

### Packages
library(readxl)
#library(rgdal)
library(tidyverse)
library(ggmap)
library(spatstat)
library(maptools)
library(readxl)
library(sp)
library(sf)

# Loading India map
I_map <- ggmap(get_stadiamap(c(left = 67, bottom = 7.5, right = 98.2, top = 36.1), zoom = 5))

#load prio shapefile
prio_shp <- st_read("data/original/prio/priogrid_cellshp/priogrid_cell.shp") %>%
  as_Spatial()

#load prio shape
# state_shape3 <- rgdal::readOGR(dsn = "data/shape_files/maps-f0f120311fefd5049b0dab6856c2fa5bf06e9b08/states",
#                                layer= "Admin2")

#subset to only India and plot
# proj4string(prio_shp)
# proj4string(state_shape3) #they match!
# over_dat <- over(prio_shp, state_shape3)
# india_prio <- prio_shp[which(!is.na(over_dat$ST_NM)),]

india_prio <- prio_shp


#load in prio variables
prio_static <- read.csv(file = "data/original/prio/PRIO-GRID Static Variables - 2023-02-23.csv")
#prior_yearly <- read.csv(file = "data/original/prio/PRIO-GRID Yearly Variables for 2014-2014 - 2023-02-23.csv")
prior_yearly <- read.csv(file = "data/original/prio/PRIO-GRID Yearly Variables for 2010-2010 - 2023-02-23.csv")
#prior_yearly2 <- read.csv(file = "data/original/prio/PRIO-GRID Yearly Variables for 2005-2005 - 2023-03-02.csv")
prior_yearly2 <- read.csv(file = "data/original/prio/PRIO-GRID Yearly Variables for 2005-2005 - 2023-03-15.csv")

#yearly dataset: for 2010 and 2005 (for older variables)

#match the prio dataset to the prio shape
india_prio@data <- left_join(india_prio@data, prio_static, by = "gid")
india_prio@data <- left_join(india_prio@data, prior_yearly, by = "gid")
#save gids
#gids <- india_prio@data$gid
india_prio@data <- india_prio@data %>% dplyr::select(-(colnames(prior_yearly2)[-1]))

#join the updated data from 2005
india_prio@data <- left_join(india_prio@data, prior_yearly2, by = "gid")

india_prio <- india_prio[which(india_prio@data$gwno == 750),]

#remove islands
india_prio <- india_prio[-which(is.na(india_prio$cmr_mean)),]


#plot one of the variables
prio_fortify <- broom::tidy(india_prio)
india_prio$id <- row.names(india_prio)
prio_fortify <- left_join(prio_fortify, india_prio@data)


#crop plot (saved as 6x5in)
cairo_pdf(file = "./Output/images/cmr_prio.pdf",
          #width =6, 
          height = 5) 
I_map + geom_polygon(data= prio_fortify, aes(x = long, y = lat,
                                              group = group, fill = cmr_mean),
                     col = "black", size =0.1) + scale_fill_distiller(palette = 
                    "Spectral") + labs(fill= "Child Malnutrition \nRate (%)") +
  theme_void()
dev.off()

cairo_pdf(file = "./Output/images/harvarea_prio.pdf",
          #width =6, 
          height = 5) 
I_map + geom_polygon(data= prio_fortify, aes(x = long, y = lat,
                                             group = group, fill = harvarea),
                     col = "black") + scale_fill_distiller(palette = "Spectral")+
  theme_void() + labs(col = "Harvest Area\n(hect)")
dev.off()


#agriculture plot (saved as 6x5in)
I_map + geom_polygon(data= prio_fortify, aes(x = long, y = lat,
                                             group = group, fill = agri_gc),
                     col = "black", size = 0.1) + scale_fill_distiller(palette = 
                    "Spectral") + labs(fill= "Agricultural Coverage\n(%)")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

#barren plot
I_map + geom_polygon(data= prio_fortify, aes(x = long, y = lat,
                                             group = group, fill = barren_ih),
                     col = "black") + scale_fill_distiller(palette = "Spectral")

#for poster
#population plot
cairo_pdf(file = "./Output/images/pop_prio.pdf",
          width =6, 
          height = 5) 
I_map + geom_polygon(data= prio_fortify, aes(x = long, y = lat,
                                             group = group, fill = pop_gpw_sum/1000000),
                     col = "black") + scale_fill_distiller(palette = "Spectral") +
  labs(fill = "Total \nPopulation\n(Millions)")
dev.off()

#GCP plot
cairo_pdf(file = "./Output/images/gcp_prio.pdf",
          width =6, 
          height = 5) 
I_map + geom_polygon(data= prio_fortify, aes(x = long, y = lat,
                                             group = group, fill = gcp_mer),
                     col = "black") + scale_fill_distiller(palette = "Spectral") +
  labs(fill = "Gross Cell \nProduct \n(USD)")
dev.off()

###
### Overlay the farmers protests on the grid and plot them
###
#load farmers protest data
farmers_protest <- read.csv("data/original/Farmer_s Protest Data.csv")
colnames(farmers_protest)

# transformations
copy_protest <- farmers_protest
coordinates(copy_protest) <- ~longitude+latitude 
class(copy_protest)

#project and reproject protest data
proj4string(copy_protest) <- "+proj=longlat +datum=NAD83 +no_defs"
proj4string(copy_protest)
copy_protest<- spTransform(copy_protest, proj4string(india_prio))


#overlay the farmers protest on the village shape
# check overlap
over_ds <-over(copy_protest,india_prio)

# add spatial variables to protest data
farmers_protest <- bind_cols(farmers_protest, over_ds)

#plot ggplot
ggplot() + geom_point(data=farmers_protest,
                      aes(x = longitude, y = latitude, col = (agri_ih))) +
  scale_color_distiller(palette= "Spectral")

#save the dataset with covariate values
write.csv(farmers_protest, file = "data/working/farmers_prio.csv")




###
### Create integration points
###

#for these integration points, just take the centroids of all of the grids
integ_points <- india_prio@data

#quick plot of points
plot(integ_points$xcoord.x, integ_points$ycoord.x)

#save the integration points
write.csv(integ_points, file = "data/working/prio_integ.csv")


###
###Overlay data with police and union indicators on the PRIO dataset
###     First, run through line 107 above to get PRIO data processed
###
library(sp)
load(file = "data/working/areal_point_comp_polunion.Rdata")

#load farmers protest data
colnames(farmers_protest)

# transformations
copy_protest <- farmers_protest
coordinates(copy_protest) <- ~longitude+latitude 
class(copy_protest)

#project and reproject protest data
proj4string(copy_protest) <- "+proj=longlat +datum=NAD83 +no_defs"
proj4string(copy_protest)
copy_protest<- spTransform(copy_protest, proj4string(india_prio))


#overlay the farmers protest on the village shape
# check overlap
over_ds <-over(copy_protest,india_prio)

# add spatial variables to protest data
farmers_protest <- bind_cols(farmers_protest, over_ds)

#plot ggplot
ggplot() + geom_point(data=farmers_protest,
                      aes(x = longitude, y = latitude, col = (agri_ih))) +
  scale_color_distiller(palette= "Spectral")

#save the dataset with covariate values
write.csv(farmers_protest, file = "data/working/farmers_union_prio.csv")
