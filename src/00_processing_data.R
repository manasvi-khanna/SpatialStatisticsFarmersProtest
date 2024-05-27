###
### Loading and Processing Datasets
###
###  Claire Kelling and Manasvi Khanna
###    Date Created:  08/04/22
###    Last Modified: 12/27/22
###
###

#start clean
rm(list = ls())


### Set working directory
#Claire's System:
setwd("C:/Users/ckell/Desktop/Google Drive/Research/farmers_protest")
setwd("C:/Users/ckelling/Dropbox (Carleton College)/Carleton College/Research/farmers_protest")
#Manasvi's System:
setwd("/Users/manasvikhanna/Desktop/Farmer's Protest/farmers_protest")

### Packages
library(readxl)
library(rgdal)
library(tidyverse)
library(ggmap)
library(spatstat)
library(maptools)
library(readxl)
# library(dplyr)
# library(tidyr)
# library(spatstat)
# library(tigris)
# library(spdep)
# library(ggplot2)
# library(broom)
# library(sp)
# library(raster)
# library(tidycensus)
# library(maditr)
# library(gridExtra)

###
### Load datasets:
###

#load farmers protest data
allProtests <- read.csv("data/original/Farmer_s Protest Data.csv")
colnames(allProtests)

#land use data
land_use <- read_excel("data/original/ESTIMATED AREA BY SIZE CLASSES AND LAND USE by STATE.xlsx")

#district level crop data
crop_data <- read.csv("data/original/20230113_yield_clim_data_cleaned.csv")



#shapefiles
#state_shape1 <- rgdal::readOGR(dsn = "data/shape_files/India_States_ADM1_GADM-shp", layer = "3e563fd0-8ea1-43eb-8db7-3cf3e23174512020330-1-layr7d.ivha")#

#state_shape2 <- rgdal::readOGR(dsn = "data/shape_files/India-State-and-Country-Shapefile-Updated-Jan-2020-master", 
#                               layer = "India_State_Boundary")

#this is the one we use (-Manasvi)
state_shape3 <- rgdal::readOGR(dsn = "data/shape_files/maps-f0f120311fefd5049b0dab6856c2fa5bf06e9b08/States",
                               layer= "Admin2")
plot(state_shape3)

#read district shape file
district_shape <- rgdal::readOGR(dsn = "data/shape_files/district_shapefile", layer = "20190619_merged_shpfl")
plot(district_shape)


# read highway shape file
#highway <- rgdal::readOGR(dsn = "data/original/hotosm_ind_central_roads_lines_shp",
#                          layer= "hotosm_ind_central_roads_lines")
#plot(highway)

#population data
predicted_pop <- read.csv("data/working/modified_india_population_data.csv")


# Loading India map
I_map <- ggmap(get_map(c(left = 67, bottom = 8.063, right = 97.141, top = 36.7), source = "stamen"))

# read area under state data
total_area <- read.csv("data/original/Total Area Under Each State.csv")


###
### Processing data
###

# 1. Protest data
farmers_protest <- filter(allProtests, year>2018)
table(farmers_protest$year)

#making dataset with row for every actor
#*** we still want the actual row to be an event, not an associated actor
act_inv <- separate_rows(farmers_protest,assoc_actor_1,sep = ";")


# 2. Land use data
# remove empty rows and all cities total
land_use <- land_use %>% slice(-c(1,2, 39))
land_use$`State Name` <- tolower(land_use$`State Name`)
# change specific variables in land use db to match state shape db
land_use$`State Name` <- gsub('a & n islands', 'andaman & nicobar island', land_use$`State Name`)
land_use$`State Name` <- gsub('d & n haveli', 'dadara & nagar havelli', land_use$`State Name`)
land_use$`State Name` <- gsub('telengana', 'telangana', land_use$`State Name`)
land_use$`State Name` <- gsub('chattisgarh', 'chhattisgarh', land_use$`State Name`)
land_use$`State Name` <- gsub('delhi', 'nct of delhi', land_use$`State Name`)

#3. Predicted population data
#processing name of State in this dataset
predicted_pop$State<- tolower(predicted_pop$State)
# subset only total popultion 
predicted_pop_total <- subset(predicted_pop, Gender=="Persons", select=State:X2021_pop)
predicted_pop_total <- predicted_pop_total[order(predicted_pop_total$State),]

#modifyig the state name for matching
predicted_pop_total$State <- gsub('[*]', '',as.character(predicted_pop_total$State))

#modifying population values to be correct numbers
predicted_pop_total$X2019_pop <- gsub(',', '',as.character(predicted_pop_total$X2019_pop)) 
predicted_pop_total$X2020_pop <- gsub(',', '',as.character(predicted_pop_total$X2020_pop))
predicted_pop_total$X2021_pop <- gsub(',', '',as.character(predicted_pop_total$X2021_pop))

#making correct data type and unit
predicted_pop_total$X2019_pop <- (as.integer(as.character(predicted_pop_total$X2019_pop)))*1000
predicted_pop_total$X2020_pop <- (as.integer(as.character(predicted_pop_total$X2020_pop)))*1000
predicted_pop_total$X2021_pop <- (as.integer(as.character(predicted_pop_total$X2021_pop)))*1000

# change specific variables in population db to match state shape db
rownames(predicted_pop_total) <- c()
predicted_pop_total$State <- gsub('andaman & nicobar islands', 'andaman & nicobar island'
                                  , predicted_pop_total$State)
predicted_pop_total$State <- gsub('dadra & nagar haveli', 'dadara & nagar havelli'
                                  , predicted_pop_total$State)
predicted_pop_total$State <- gsub("jammu & kashmir.*", 'jammu & kashmir',
                                   predicted_pop_total$State)
predicted_pop_total$State <- gsub('chandigarh ', 'chandigarh',
                                  predicted_pop_total$State)

# 4. Shape file data
# change state names to lower case to match each other
state_shape3@data$ST_NM <- tolower(state_shape3@data$ST_NM)
# change names in state shape 
state_shape3@data$ST_NM <- gsub('arunanchal pradesh', 'arunachal pradesh', state_shape3@data$ST_NM)

# change distrct names to lower case to match each other
district_shape@data$icri_dist <- tolower(district_shape@data$icri_dist)
district_shape@data$icri_st <- tolower(district_shape@data$icri_st)


###
### Combine all demographic data with the shapefile
###

# match land use and state shape 
state_shape3@data <- left_join(state_shape3@data, land_use, by = c("ST_NM" = "State Name"))
state_shape3@data <- left_join(state_shape3@data, predicted_pop_total, by = c("ST_NM" = "State"))
state_shape3@data <- left_join(state_shape3@data, total_area, by = c("ST_NM" = "ST_NM"))

# Calculate Proportion of aggricultural land for each state
state_shape3@data['agri_prop'] <- state_shape3@data$`Net Area Sown`/(state_shape3@data$Total_Area..km.sq.*100)


#match crop data and district shape
#most recent crop data
crop_data <- crop_data %>% filter(year == 2004, crop == "rice")
dim(district_shape@data)
district_shape@data <- left_join(district_shape@data, crop_data, by = c("icri_dist" = "district",
                                                                        "icri_st" = "state"))
district_shape@data$icri_dist[which(duplicated(district_shape@data$icri_dist))]
#View(district_shape@data)
dim(district_shape@data)


#try plotting socioeconomic variables

# state level
state_fortify <- broom::tidy(state_shape3)
state_shape3$id <- row.names(state_shape3)
state_fortify <- left_join(state_fortify, state_shape3@data)


#population plot
I_map + geom_polygon(data= state_fortify, aes(x = long, y = lat,
                                              group = group, fill = X2020_pop),
                     col = "black") + scale_fill_distiller(palette = "Spectral")

I_map + geom_polygon(data= state_fortify, aes(x = long, y = lat,
                                              group = group, fill = agri_prop),
                     col = "black") + scale_fill_distiller(palette = "Spectral")


# district level
district_fortify <- broom::tidy(district_shape)               
district_shape$id <- row.names(district_shape)
district_fortify <- left_join(district_fortify, district_shape@data)

# irrigation plot 
I_map + geom_polygon(data= district_fortify, aes(x = long, y = lat,
                                              group = group, fill = irrigated_area),
                     col = "black") + scale_fill_distiller(palette = "Spectral")

#plot of net area sown
I_map + geom_polygon(data= state_fortify, aes(x = long, y = lat,
                                              group = group, fill = 
                                                `Net Area Sown`),
                     col = "black") + scale_fill_distiller(palette = "Spectral")


###
### Process event data and plot on top of the India map
###

# transformations
copy_protest <- farmers_protest
coordinates(copy_protest) <- ~longitude+latitude 
class(copy_protest)

#project and reproject protest data
proj4string(copy_protest) <- "+proj=longlat +datum=NAD83 +no_defs"
proj4string(copy_protest)
copy_protest<- spTransform(copy_protest, proj4string(state_shape3))

#check bounding boxes
bbox(state_shape3)
bbox(copy_protest) 

# check overlap
over_ds <-over(copy_protest,state_shape3)

# add environmental variables to protest data
farmers_protest <- bind_cols(farmers_protest, over_ds)

#Sl.No is a unique identifier, so we can count by this identifier
count_event <- over_ds %>% group_by(Sl.No) %>% summarize(n())
count_event <- over_ds %>% group_by(ST_NM) %>% summarize(n())
#(two states have no events (sikkim and mizoram);some union terrirories have 0) 

#call this column something informative
colnames(count_event)[2] <- "num_protest"

#let's join it to the shapefile
state_shape3@data <- left_join(state_shape3@data, count_event)

#refortify data for plotting
state_fortify <- broom::tidy(state_shape3)
state_shape3$id <- row.names(state_shape3)
state_fortify <- left_join(state_fortify, state_shape3@data)

#population plot
I_map + geom_polygon(data= state_fortify, aes(x = long, y = lat,
                                              group = group, fill = num_protest),
                     col = "black") + scale_fill_distiller(palette = "Spectral")

#plot as points
I_map + geom_polygon(data= state_fortify, aes(x = long, y = lat,
                                              group = group, fill = num_protest),
                     col = "grey") + scale_fill_distiller(palette = "Spectral") +
  geom_point(data=farmers_protest, aes(x=longitude, y = latitude), size = 0.4)


#save shape file data and point data in Rdata
#save(state_shape3, farmers_protest, file = "data/working/areal_point_comp.Rdata")


###
### Creating integration points
###
#Create a lattice (grid) over the surfance
lattice <- expand.grid(lon = seq(bbox(state_shape3)[1,1],bbox(state_shape3)[1,2],length.out = 60), 
                       lat = seq(bbox(state_shape3)[2,1],bbox(state_shape3)[2,2],length.out = 60))
sp_lattice <- lattice

#subset to points inside window
coordinates(sp_lattice) <- ~lon+lat
proj4string(sp_lattice) <- proj4string(state_shape3)
over_latt <- over(sp_lattice,state_shape3)

lattice <- bind_cols(lattice, over_latt)

lattice <- lattice[-which(is.na(over_latt$ST_NM)),]

#compare covariate plots
#plot existing data with elevation with ggplot
ggplot(lattice, aes(lon,lat,col=X2020_pop)) + geom_point() + scale_colour_distiller(palette = "Spectral")

#rename dataframe for clarity
integ_df <- lattice

#save new integration data
#save(integ_df, file = "data/working/india_integ_points.Rdata")



###
### Label events as police or union involved
###
rm(list = ls())
library(stringr)
library(tidyverse)
#read in protest data
allProtests <- read.csv("data/original/Farmer_s Protest Data.csv")

# #policing indicator
# allProtests$police_ind <- rep(NA, nrow(allProtests))
# 
# #see if police are involved in either actor type
# allProtests$police_ind[which(str_detect(allProtests$actor1, "Police"))] <- 1
# sum(allProtests$police_ind, na.rm=T) #25
# allProtests$police_ind[which(str_detect(allProtests$actor2, "Police"))] <- 1
# sum(allProtests$police_ind, na.rm=T) #532 (most of them are actor2)
# allProtests$police_ind[which(str_detect(allProtests$assoc_actor_1, "Police"))] <- 1
# sum(allProtests$police_ind, na.rm=T) #534
# allProtests$police_ind[which(str_detect(allProtests$assoc_actor_2, "Police"))] <- 1
# sum(allProtests$police_ind, na.rm=T) #535


# #union indicator
# allProtests$union_ind <- rep(NA, nrow(allProtests))
# #see if unions are involved in either actor type
# allProtests$union_ind[which(str_detect(allProtests$actor1, "Union"))] <- 1
# sum(allProtests$union_ind, na.rm=T) #0
# allProtests$union_ind[which(str_detect(allProtests$actor2, "Union"))] <- 1
# sum(allProtests$union_ind, na.rm=T) #0
# allProtests$union_ind[which(str_detect(allProtests$assoc_actor_1, "Union"))] <- 1
# sum(allProtests$union_ind, na.rm=T) #1093 (most of them are assoc actor 2)
# allProtests$union_ind[which(str_detect(allProtests$assoc_actor_2, "Union"))] <- 1
# sum(allProtests$union_ind, na.rm=T) #1097

#need to figure out where Manasvi has coded unions in modified data? 
#^this is done right?
#Actor Cleaning
#read in actor cleaning dataset
act_clean <- read.csv("./data/working/Actor Classification/Actor Cleaning.csv")

#need to process names of actors so they are identical to what we saw in original dataset
clean_actors <- str_remove(act_clean$Associated_Actor1, "\\[.\\]") %>% 
  str_remove(., "\\[..\\]") %>% str_remove(., "\\[...\\]") %>%
  str_remove(., "\"") %>%
  str_remove(., "\\\"") %>% str_trim() %>% as.character()

# which(str_detect(allProtests$assoc_actor_1, clean_actors[1]))
# allProtests$assoc_actor_1[1]
# clean_actors[1]
# str_detect(allProtests$assoc_actor_1[1], clean_actors[1])
# clean_actors2 <- gsub("\\(", "\\\\(", clean_actors); clean_actors <- gsub("\\)", "\\\\)", clean_actors)
# str_detect(allProtests$assoc_actor_1[1], clean_actors[1]) #parentheses causing issues


#first, break up the all protest farmers data actors
farm_act <- separate_rows(allProtests,assoc_actor_1,sep = ";") %>% #select(assoc_actor_1) %>%
  mutate(assoc_actor_1 = trimws(assoc_actor_1)) #get rid of white space
act_clean$clean_actors <- clean_actors 
 
#remove duplicated actors
act_clean <- act_clean[-which(duplicated(act_clean$clean_actors)),]

#then, join the cleaned actor data to the farmers protest data
full_farm <- left_join(farm_act, act_clean, by = c("assoc_actor_1" = "clean_actors"))

#how many of them aren't matched?
full_farm$assoc_actor_1[which(is.na(full_farm$Associated_Actor1))] %>% unique()

#indicator variable to be if event involves union
full_farm$union_ind <- rep(NA, nrow(full_farm))
full_farm$police_ind <- rep(NA, nrow(full_farm))
#Manasvi check: Is the word "union" in all actor groups that should be considered?
#yes
full_farm$union_ind[which(str_detect(full_farm$Group, "Union"))] <- 1
#Manasvi check: Is the word "police" in all actor groups that should be considered?
#yes
full_farm$police_ind[which(str_detect(full_farm$Group, "Police"))] <- 1
full_farm$police_ind[which(str_detect(full_farm$Comments, "POLICE"))] <- 1
sum(full_farm$union_ind, na.rm=T)
sum(full_farm$police_ind, na.rm=T)


#sum the number of times a union is mentioned, make it a max of 1 to be an indicator variable
act_sum <- full_farm %>% group_by(data_id) %>% summarize(union_sum = sum(union_ind, na.rm = T),
                                                      police_sum = sum(police_ind, na.rm = T))

act_sum$union_bin <- ifelse(act_sum$union_sum > 0, 1, 0)
act_sum$police_bin <- ifelse(act_sum$police_sum > 0, 1, 0)
sum(act_sum$union_bin)
sum(act_sum$police_bin)

#join this to the original dataset and compare to what we found before
#allProtests <- left_join(allProtests, act_sum[,c("data_id", "union_bin", "police_bin")])
#sum(allProtests$union_bin)
#sum(allProtests$police_bin)
#much larger than before for unions, not for police (stored in other variable)

###
### check assoc_actor_2 for police and union
###
#clean up based on assoc_actor_2
farm_act <- separate_rows(allProtests,assoc_actor_2,sep = ";") %>% #select(assoc_actor_1) %>%
  mutate(assoc_actor_2 = trimws(assoc_actor_2)) #get rid of white space
#then, join the cleaned actor data to the farmers protest data
full_farm <- left_join(farm_act, act_clean, by = c("assoc_actor_2" = "clean_actors"))

#how many of them aren't matched?
full_farm$assoc_actor_2[which(is.na(full_farm$Associated_Actor1))] %>% unique()

#indicator variable to be if event involves union
full_farm$union_ind <- rep(NA, nrow(full_farm))
full_farm$police_ind <- rep(NA, nrow(full_farm))
#Manasvi check: Is the word "union" in all actor groups that should be considered?
full_farm$union_ind[which(str_detect(full_farm$Group, "Union"))] <- 1
#Manasvi check: Is the word "police" in all actor groups that should be considered?
full_farm$police_ind[which(str_detect(full_farm$Group, "Police"))] <- 1
full_farm$police_ind[which(str_detect(full_farm$Comments, "POLICE"))] <- 1
sum(full_farm$union_ind, na.rm=T)
sum(full_farm$police_ind, na.rm=T)


#sum the number of times a union is mentioned, make it a max of 1 to be an indicator variable
act_sum2 <- full_farm %>% group_by(data_id) %>% summarize(union_sum2 = sum(union_ind, na.rm = T),
                                                         police_sum2 = sum(police_ind, na.rm = T))

#combine with previous act_sum
act_sum <- left_join(act_sum, act_sum2)
act_sum$sum_ind_union <- act_sum$union_sum + act_sum$union_sum2
act_sum$sum_ind_police <- act_sum$police_sum+ act_sum$police_sum2

#make new binary indicator based on both variables
act_sum$union_bin <- ifelse(act_sum$sum_ind_union > 0, 1, 0)
act_sum$police_bin <- ifelse(act_sum$sum_ind_police > 0, 1, 0)
sum(act_sum$union_bin)
sum(act_sum$police_bin)

###
### calculate for Actor 1
###
#clean up based on actor1
farm_act <- separate_rows(allProtests,actor1,sep = ";") %>% #select(assoc_actor_1) %>%
  mutate(actor1 = trimws(actor1)) #get rid of white space
#then, join the cleaned actor data to the farmers protest data
full_farm <- left_join(farm_act, act_clean, by = c("actor1" = "clean_actors"))

#how many of them aren't matched?
full_farm$actor1[which(is.na(full_farm$Associated_Actor1))] %>% unique()

#indicator variable to be if event involves union
full_farm$union_ind <- rep(NA, nrow(full_farm))
full_farm$police_ind <- rep(NA, nrow(full_farm))
#Manasvi check: Is the word "union" in all actor groups that should be considered?
full_farm$union_ind[which(str_detect(full_farm$Group, "Union"))] <- 1
#Manasvi check: Is the word "police" in all actor groups that should be considered?
full_farm$police_ind[which(str_detect(full_farm$Group, "Police"))] <- 1
full_farm$police_ind[which(str_detect(full_farm$Comments, "POLICE"))] <- 1
sum(full_farm$union_ind, na.rm=T)
sum(full_farm$police_ind, na.rm=T)


#sum the number of times a union is mentioned, make it a max of 1 to be an indicator variable
act_sum3 <- full_farm %>% group_by(data_id) %>% summarize(union_sum3 = sum(union_ind, na.rm = T),
                                                          police_sum3 = sum(police_ind, na.rm = T))

#combine with previous act_sum
act_sum <- left_join(act_sum, act_sum3)
act_sum$sum_ind_union <- act_sum$sum_ind_union + act_sum$union_sum3
act_sum$sum_ind_police <- act_sum$sum_ind_police+ act_sum$police_sum3

#make new binary indicator based on both variables
act_sum$union_bin <- ifelse(act_sum$sum_ind_union > 0, 1, 0)
act_sum$police_bin <- ifelse(act_sum$sum_ind_police > 0, 1, 0)
sum(act_sum$union_bin)
sum(act_sum$police_bin)

###
### calculate for Actor 2
###
#clean up based on actor2
farm_act <- separate_rows(allProtests,actor2,sep = ";") %>% #select(assoc_actor_1) %>%
  mutate(actor2 = trimws(actor2)) #get rid of white space
#then, join the cleaned actor data to the farmers protest data
full_farm <- left_join(farm_act, act_clean, by = c("actor2" = "clean_actors"))

#how many of them aren't matched?
full_farm$actor2[which(is.na(full_farm$Associated_Actor1))] %>% unique()

#indicator variable to be if event involves union
full_farm$union_ind <- rep(NA, nrow(full_farm))
full_farm$police_ind <- rep(NA, nrow(full_farm))
#Manasvi check: Is the word "union" in all actor groups that should be considered?
full_farm$union_ind[which(str_detect(full_farm$Group, "Union"))] <- 1
#Manasvi check: Is the word "police" in all actor groups that should be considered?
full_farm$police_ind[which(str_detect(full_farm$Group, "Police"))] <- 1
full_farm$police_ind[which(str_detect(full_farm$Comments, "POLICE"))] <- 1
sum(full_farm$union_ind, na.rm=T)
sum(full_farm$police_ind, na.rm=T)


#sum the number of times a union is mentioned, make it a max of 1 to be an indicator variable
act_sum4 <- full_farm %>% group_by(data_id) %>% summarize(union_sum4 = sum(union_ind, na.rm = T),
                                                          police_sum4 = sum(police_ind, na.rm = T))

#combine with previous act_sum
act_sum <- left_join(act_sum, act_sum4)
act_sum$sum_ind_union <- act_sum$sum_ind_union + act_sum$union_sum4
act_sum$sum_ind_police <- act_sum$sum_ind_police+ act_sum$police_sum4

#make new binary indicator based on both variables
act_sum$union_bin <- ifelse(act_sum$sum_ind_union > 0, 1, 0)
act_sum$police_bin <- ifelse(act_sum$sum_ind_police > 0, 1, 0)
sum(act_sum$union_bin)
sum(act_sum$police_bin)





#join this to the original dataset and compare to what we found before
allProtests <- left_join(allProtests, act_sum[,c("data_id", "union_bin", "police_bin")])
sum(allProtests$union_bin)
sum(allProtests$police_bin)



#load in modified dataset (created earlier in this script), not original dataset
load(file = "data/working/areal_point_comp.Rdata")

#add labels for union and police involvement
farmers_protest <- left_join(farmers_protest, act_sum[,c("data_id", "union_bin", "police_bin")])
sum(farmers_protest$police_bin)
sum(farmers_protest$union_bin)

#save overwritten dataset with labels for police and union involvement
save(state_shape3, farmers_protest, file = "data/working/areal_point_comp_polunion.Rdata")



