###
### Creating a for loop for indicator variables
###
###  Claire Kelling and Manasvi Khanna
###    Date Created:  04/03/22
###    Last Modified: 04/03/22
###
###

#start clean
rm(list = ls())


### Set working directory
#Claire's System:
#setwd("C:/Users/ckell/Desktop/Google Drive/Research/farmers_protest")
#setwd("C:/Users/ckelling/Dropbox (Carleton College)/Carleton College/Research/farmers_protest")
#Manasvi's System:
setwd("/Users/manasvikhanna/Desktop/Farmer's Protest/farmers_protest_git")

### Packages
library(readxl)
library(rgdal)
library(tidyverse)
library(stringr)
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

# read in processed data with police and Trade indiactors
#load(file = "data/working/areal_point_comp_polunion.Rdata")
farmers_protest <- read.csv(file = "data/working/farmers_prio.csv")

###
### Label events as police or Trade involved
###
look_for <- c("Police", "Trade", "Religious","Political","Regional","Central","Coalition")


for(i in 1:length(look_for)){

  k = look_for[i]
  # print(k) for testing
  
  #Create an indicator
  farmers_protest$k_search<- rep(NA, nrow(farmers_protest))
  #see if keyword are involved in either actor type
  farmers_protest$k_search[which(str_detect(farmers_protest$actor1, k))] <- 1
  sum(farmers_protest$k_search, na.rm=T) #0
  farmers_protest$k_search[which(str_detect(farmers_protest$actor2, k))] <- 1
  sum(farmers_protest$k_search, na.rm=T) #0
  farmers_protest$k_search[which(str_detect(farmers_protest$assoc_actor_1, k))] <- 1
  sum(farmers_protest$k_search, na.rm=T) #1093 (most of them are assoc actor 2)
  farmers_protest$k_search[which(str_detect(farmers_protest$assoc_actor_2, k))] <- 1
  sum(farmers_protest$k_search, na.rm=T) #1097
  
  #Actor Cleaning
  #read in actor cleaning dataset
  act_clean <- read.csv("./data/working/Actor Classification/Actor Cleaning.csv")
  
  #need to process names of actors so they are identical to what we saw in original dataset
  clean_actors <- str_remove(act_clean$Associated_Actor1, "\\[.\\]") %>% 
    str_remove(., "\\[..\\]") %>% str_remove(., "\\[...\\]") %>%
    str_remove(., "\"") %>%
    str_remove(., "\\\"") %>% str_trim() %>% as.character()
  # 
  # which(str_detect(farmers_protest$assoc_actor_1, clean_actors[1]))
  # farmers_protest$assoc_actor_1[1]
  # clean_actors[1]
  # str_detect(farmers_protest$assoc_actor_1[1], clean_actors[1])
  # clean_actors2 <- gsub("\\(", "\\\\(", clean_actors); clean_actors <- gsub("\\)", "\\\\)", clean_actors)
  # str_detect(farmers_protest$assoc_actor_1[1], clean_actors[1]) #parentheses causing issues
  
  
  #first, break up the all protest farmers data actors
  farm_act <- separate_rows(farmers_protest,assoc_actor_1,sep = ";") %>% #select(assoc_actor_1) %>%
    mutate(assoc_actor_1 = trimws(assoc_actor_1)) #get rid of white space
  act_clean$clean_actors <- clean_actors 
  
  #remove duplicated actors
  act_clean <- act_clean[-which(duplicated(act_clean$clean_actors)),]
  
  #then, join the cleaned actor data to the farmers protest data
  full_farm <- left_join(farm_act, act_clean, by = c("assoc_actor_1" = "clean_actors"))
  
  #how many of them aren't matched?
  full_farm$assoc_actor_1[which(is.na(full_farm$Associated_Actor1))] %>% unique()
  
  #indicator variable to be if event involves keyword
  full_farm$k_search <- rep(NA, nrow(full_farm))
  
  full_farm$k_search[which(str_detect(full_farm$Group, k))] <- 1
  
  sum(full_farm$k_search, na.rm=T)
  
  
  #sum the number of times a keyword is mentioned, make it a max of 1 to be an indicator variable
  act_sum <- full_farm %>% group_by(data_id) %>% summarize(k_sum = sum(k_search, na.rm = T))
  
  act_sum$k_bin <- ifelse(act_sum$k_sum > 0, 1, 0)
  sum(act_sum$k_bin)
  
  #join this to the original dataset and compare to what we found before
  #farmers_protest <- left_join(farmers_protest, act_sum[,c("data_id", "Trade_bin", "police_bin")])
  #sum(farmers_protest$Trade_bin)
  #sum(farmers_protest$police_bin)
  #much larger than before for Trades, not for police (stored in other variable)
  
  ###
  ### check assoc_actor_2 for police and Trade
  ###
  #clean up based on assoc_actor_2
  farm_act <- separate_rows(farmers_protest,assoc_actor_2,sep = ";") %>% #select(assoc_actor_1) %>%
    mutate(assoc_actor_2 = trimws(assoc_actor_2)) #get rid of white space
  #then, join the cleaned actor data to the farmers protest data
  full_farm <- left_join(farm_act, act_clean, by = c("assoc_actor_2" = "clean_actors"))
  
  #how many of them aren't matched?
  full_farm$assoc_actor_2[which(is.na(full_farm$Associated_Actor1))] %>% unique()
  
  #indicator variable to be if event involves Trade
  full_farm$k_search <- rep(NA, nrow(full_farm))
  full_farm$k_search[which(str_detect(full_farm$Group, k))] <- 1
  sum(full_farm$k_search, na.rm=T)
  
  
  #sum the number of times a keyword is mentioned, make it a max of 1 to be an indicator variable
  act_sum2 <- full_farm %>% group_by(data_id) %>% summarize(k_sum2 = sum(k_search, na.rm = T))
  
  #combine with previous act_sum
  act_sum <- left_join(act_sum, act_sum2)
  act_sum$sum_ind_k <- act_sum$k_sum + act_sum$k_sum2
  
  #make new binary indicator based on both variables
  act_sum$k_bin <- ifelse(act_sum$sum_ind_k > 0, 1, 0)
  sum(act_sum$k_bin)
  
  ###
  ### calculate for Actor 1
  ###
  #clean up based on actor1
  farm_act <- separate_rows(farmers_protest,actor1,sep = ";") %>% #select(assoc_actor_1) %>%
    mutate(actor1 = trimws(actor1)) #get rid of white space
  #then, join the cleaned actor data to the farmers protest data
  full_farm <- left_join(farm_act, act_clean, by = c("actor1" = "clean_actors"))
  
  #how many of them aren't matched?
  full_farm$actor1[which(is.na(full_farm$Associated_Actor1))] %>% unique()
  
  #indicator variable to be if event involves keyword
  full_farm$k_search <- rep(NA, nrow(full_farm))
  full_farm$k_search[which(str_detect(full_farm$Group, k))] <- 1
  
  sum(full_farm$k_search, na.rm=T)
  
  #sum the number of times a keyword is mentioned, make it a max of 1 to be an indicator variable
  act_sum3 <- full_farm %>% group_by(data_id) %>% summarize(k_sum3 = sum(k_search, na.rm = T))
  
  #combine with previous act_sum
  act_sum <- left_join(act_sum, act_sum3)
  act_sum$sum_ind_k <- act_sum$sum_ind_k + act_sum$k_sum3
  
  #make new binary indicator based on both variables
  act_sum$k_bin <- ifelse(act_sum$sum_ind_k > 0, 1, 0)
  sum(act_sum$k_bin)
  
  ###
  ### calculate for Actor 2
  ###
  #clean up based on actor2
  farm_act <- separate_rows(farmers_protest,actor2,sep = ";") %>% #select(assoc_actor_1) %>%
    mutate(actor2 = trimws(actor2)) #get rid of white space
  #then, join the cleaned actor data to the farmers protest data
  full_farm <- left_join(farm_act, act_clean, by = c("actor2" = "clean_actors"))
  
  #how many of them aren't matched?
  full_farm$actor2[which(is.na(full_farm$Associated_Actor1))] %>% unique()
  
  #indicator variable to be if event involves keyword
  full_farm$k_search <- rep(NA, nrow(full_farm))
  full_farm$k_search[which(str_detect(full_farm$Group, k))] <- 1
  
  sum(full_farm$k_search, na.rm=T)
  
  
  #sum the number of times a keyword is mentioned, make it a max of 1 to be an indicator variable
  act_sum4 <- full_farm %>% group_by(data_id) %>% summarize(k_sum4 = sum(k_search, na.rm = T))
  
  #combine with previous act_sum
  act_sum <- left_join(act_sum, act_sum4)
  act_sum$sum_ind_k <- act_sum$sum_ind_k + act_sum$k_sum4
  
  #make new binary indicator based on both variables
  act_sum$k_bin <- ifelse(act_sum$sum_ind_k > 0, 1, 0)
  sum(act_sum$k_bin)
  
  
  # #join this to the original dataset and compare to what we found before
  farmers_protest <- left_join(farmers_protest, act_sum[,c("data_id", "k_bin")])
  sum(farmers_protest$k_bin)
  
  
  #end of for loop: create new vectors of each type, sub out farmers_protest$X for newly created indicator
  assign(paste0(k, "_ind"), farmers_protest$k_bin)
  
  # for confirmed testing
  # output_dat <- cbind(output_dat, farmers_protest$k_bin)
  # colnames(output_dat) <- 1:(i+1)
  #farmers_protest$k_search <- rep(NA,nrow(farmers_protest))
  #farmers_protest$k_bin <- rep(NA,nrow(farmers_protest))
  
  farmers_protest<- farmers_protest %>% select(-c(k_search,k_bin))
  rm(act_sum,act_sum2,act_sum3,act_sum4,act_clean,full_farm,farm_act)
  
}
#after the for loop, put them in the dataframe (example below)
farmers_protest$religious_ind <- Religious_ind
farmers_protest$police_ind <- Police_ind
farmers_protest$political_ind <- Political_ind
farmers_protest$regional_ind <- Regional_ind
farmers_protest$centrall_ind <- Central_ind
farmers_protest$farmer_union_ind <- Coalition_ind
farmers_protest$Trade_union_ind <- Trade_ind







#save overwritten dataset with labels for police and Trade involvement
#save(state_shape3, farmers_protest, file = "data/working/areal_point_comp_inds.Rdata")
write.csv(farmers_protest, file = "data/working/farmers_protests_w_ind.csv")


#to add: police, union (coalition), religious, non-trade, political

