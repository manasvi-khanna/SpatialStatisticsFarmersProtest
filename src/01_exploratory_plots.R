###
### Creating exploratory plots 
###
###  Claire Kelling and Manasvi Khanna
###    Date Created:  12/06/22
###    Last Modified: 12/06/22
###
###

#start clean
rm(list = ls())

### Set working directory
#Claire's System:
#setwd("C:/Users/ckell/Desktop/Google Drive/Research/farmers_protest")
setwd("C:/Users/ckelling/Dropbox (Carleton College)/Carleton College/Research/farmers_protest")
#Manasvi's System:
setwd("/Users/manasvikhanna/Desktop/Farmer's Protest/farmers_protest")

### Packages

library(gganimate)
library(transformr)
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
library(wesanderson)
library(gifski)


# loading spatial data, cleaned in 00_processing_data.R (both areal and point-level data)
load(file = "data/working/areal_point_comp.Rdata")
load(file = "data/working/areal_point_comp_polunion.Rdata")


# rename state shape
state_shape <- state_shape3

# creating a map of India as a base 
I_map <- ggmap(get_map(c(left = 68, bottom = 7.5, right = 97.141, top = 35), source = "stamen"))
I_map

# visualizing the farmers protest 
I_map+geom_point(data=farmers_protest, aes(x=longitude, y =latitude), 
                 color = "#0072B2", size = 1)

# colored by a variable 
## add balck lines around the dot 
I_map+geom_point(data=farmers_protest, aes(x=longitude, y =latitude, col = actor2), size = 1
)+labs(col = "Actors") + scale_color_brewer(palette="Spectral")

# plotting a subset of the actor data
farm_act <- separate_rows(farmers_protest,assoc_actor_1,sep = ";") %>% #select(assoc_actor_1) %>%
  mutate(assoc_actor_1 = trimws(assoc_actor_1))

vals <- c("BKU: Bharatiya Kisan Union","AIKS: All India Kisan Sabha","Dalit Caste Group (India)",
          "TRS: Telangana Rashtra Samithi","INC: Indian National Congress","CITU: Centre of Indian Trade Unions") 
farm_act_sub<- farm_act[farm_act$assoc_actor_1 %in% vals,]


I_map+geom_point(data=farm_act_sub, aes(x=longitude, y =latitude, col = assoc_actor_1), size = 1
)+labs(col = "Sample Protesting Groups/Actors") + scale_color_brewer(palette="Spectral")

state_fortify <- broom::tidy(state_shape3)
state_shape3$id <- row.names(state_shape3)
state_fortify <- left_join(state_fortify, state_shape3@data)

# Holdings plot
I_map + geom_polygon(data= state_fortify, aes(x = long, y = lat,
                                              group = group, fill = `Total Holdings Area`)
                     ) + scale_fill_distiller(palette = "Spectral")

# Area Sown plot
I_map + geom_polygon(data= state_fortify, aes(x = long, y = lat,
                                              group = group, fill = `Net Area Sown`)
) + scale_fill_distiller(palette = "Spectral")


# farmers_protest$month_year <- fct_recode(farmers_protest$month_year, c("October 2019", "November 2019"))
# ?fct_recode
# create a month year variable 
farmers_protest$month_year <- substr(farmers_protest$event_date,4,length(farmers_protest$event_date))

# convert to a factor variable 
farmers_protest$month_year <- as.factor(farmers_protest$month_year)
farmers_protest$month_year2 <- factor(farmers_protest$month_year, 
                                      c("January 2019", "February 2019", 
                                        "March 2019", "April 2019", "May 2019",
                                        "June 2019", "July 2019", "August 2019", 
                                        "September 2019", "October 2019", "November 2019",
                                        "December 2019", "January 2020", "February 2020", 
                                        "March 2020", "April 2020", "May 2020", "June 2020",
                                        "July 2020", "August 2020", "September 2020", "October 2020",
                                        "November 2020","December 2020", "January 2021",
                                        "February 2021", "March 2021", "April 2021", "May 2021",
                                        "June 2021", "July 2021", "August 2021", "September 2021",
                                        "October 2021"))

#create dataset with police or unions
union_indicator_plot <- farmers_protest[farmers_protest$union_bin == 1,]
police_indicator_plot <- farmers_protest[farmers_protest$police_bin == 1,]


#making values be standardized by each plot
I_map + geom_density_2d_filled(data=police_indicator_plot,
                               aes(x=longitude, y =latitude),alpha = 0.5) +
  facet_wrap( ~ year)


#making same max every year:
I_map + geom_density_2d_filled(data=police_indicator_plot,
                               aes(x=longitude, y =latitude),alpha = 0.5,
                               contour_var = "ndensity") +
  facet_wrap( ~ year)

#try removing the month/year where there is only one datapoint*****

# Spatial Temporal Map 
# create a temporal plot with month-year breakdown
I_map + geom_density_2d_filled(data=police_indicator_plot,
                               aes(x=longitude, y =latitude),alpha = 0.5,
                               contour_var = "ndensity") + facet_wrap( ~ month_year2)


# animate the temporal plot 
police_intensity <- I_map + geom_density_2d_filled(data=police_indicator_plot,
                               aes(x=longitude, y =latitude),alpha = 0.5,
                               contour_var = "ndensity") 


theme_set(theme_bw())

#try animating on year first, instead of month_year2
anim <- police_intensity + 
  transition_states(year, #month_year2
                    transition_length = 2,
                    state_length = 1)

p + transition_reveal(year)


###
### Claire's animation code
###
library(ggplot2)
library(gganimate)
p <- I_map +
  geom_density_2d_filled(data=police_indicator_plot,
                         aes(x=longitude, y =latitude),alpha = 0.5,
                         contour_var = "ndensity") +
  # gganimate specific
  transition_states(year, 
                    transition_length = 1, 
                    state_length = 40) +
  labs(title = "Year: {closest_state}") + 
  ease_aes("linear")

gganimate::animate(p, renderer = gganimate::gifski_renderer())
gganimate::anim_save(filename = "year.gif", path = )
