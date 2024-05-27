###
### Load data to create figures
###
### Farmers Protest Data
### 
### Created:       12/07/23
### Last modified: 12/07/23
### 

#clean up
rm(list = ls())

#load data
library(tidyverse)
library(ggplot2)
library(gridExtra)


### Set working directory
#Claire's System:
#setwd("C:/Users/ckell/Desktop/Google Drive/Research/farmers_protest")
setwd("C:/Users/ckelling/Dropbox (Carleton College)/Carleton College/Research/farmers_protest")
#setwd("C:/Users/ckell/Dropbox (Carleton College)/Carleton College/Research/farmers_protest")
#Manasvi's System:
#setwd("/Users/manasvikhanna/Desktop/Farmer's Protest/farmers_protest_git")

#load nimble function
source(file = "src/00_nimble_functions.R")

# creating a map of India as a base 
#I_map <- ggmap(get_stadiamap(c(left = 67, bottom = 7, right = 96, top = 35), zoom = 5)) + theme_void()
load(file = "./data/working/I_map2.Rdata")

#data for plotting
load(file = "data/working/plotting_data.Rdata")

#load and process farmers protest data
farmers_protest <- read.csv(file = "data/working/farmers_protests_w_ind.csv")
farmers_protest <- farmers_protest %>% filter(year...6 > 2019)
dup_points <- farmers_protest[which(duplicated(farmers_protest$xcoord.x)),]
farmers_protest <- farmers_protest[-which(duplicated(farmers_protest$xcoord.x)),]

jitter_lat <- runif(n = nrow(dup_points), 0.001, 0.005) #only goes to fifth decimal point
jitter_lon <- runif(n = nrow(dup_points), 0.001, 0.005) #only goes to fifth decimal point

dup_points$xcoord.y <- dup_points$xcoord.y + jitter_lat
dup_points$xcoord.x <- dup_points$xcoord.x + jitter_lon

farmers_protest <- bind_rows(farmers_protest, dup_points)

#remove observations with missing covariate values, need to investigate more later
farmers_protest <- farmers_protest %>% filter(!is.na(cmr_mean))

#load final data
#1,000,000 samples created on 12/7/23
samp1 <- read.csv(file = "data/working/002_trade_union_samp_1207.csv")
samp2 <- read.csv(file = "data/working/002_union_samp_1207.csv")
samp3 <- read.csv(file = "data/working/002_regional_samp_1207.csv")
samp4 <- read.csv(file = "data/working/002_central_samp_1207.csv")
samp5 <- read.csv(file = "data/working/002_political_samp_1207.csv")
samp6 <- read.csv(file = "data/working/002_religious_samp_1207.csv")
samp7 <- read.csv(file = "data/working/002_police_samp_1207.csv")



sample_processing <- function(samples, burnin, integ_df, 
                              lattice, distance_latt, max_dist,
                              fix_phi){
  #take out first column in read in data
  samples <- samples[,-1]
  
  #store estimates of the parameters after removing burnin
  #burnin <- 500000
  param_est <- colMeans(samples[burnin:nrow(samples),])
  
  #Final variables: 
  # "cmr_mean", "pop_gpw_sum",
  # "harvarea",  "gcp_mer"
  #parametric intensity function (without GP) on integration points
  integ_logintensity <- param_est[115] + param_est[116]*integ_df$cmr_mean+
    param_est[117]*log(integ_df$pop_gpw_sum) + 
    param_est[118]*integ_df$harvarea + 
    param_est[119]*integ_df$gcp_mer##+
  #param_est[120]*integ_df$cmr_mean
  
  gp_knots_est <- param_est[1:114] %>% as.numeric()
  
  #transform the estimated GP over the notes to over the integration points
  #need to estimate sigma from the data
  sig_est <- param_est[120]
  
  #transform the GP over the integration points
  dist_knot_int = as.matrix(fields::rdist(integ_df[,c("xcoord.x", "ycoord.x")], lattice))
  dist_knot_int <- dist_knot_int/max_dist
  gp_int <- (expCov(dist_knot_int, phi=fix_phi, sigma=sig_est)%*%solve(expCov(distance_latt, phi=fix_phi, sigma=sig_est))%*%gp_knots_est)
  
  integ_df$gp_int_est <- gp_int[,1]
  
  integ_df$intens_est <- (integ_logintensity + integ_df$gp_int_est)
  
  return(integ_df)
}

burnin <- 60000

###
### Intensity Plots
###
#plot 1: Trade union

#process data
integ_df1 <- sample_processing(samp1, burnin, integ_df, 
              lattice, distance_latt, max_dist,
              fix_phi)

farmers_protest1 <- farmers_protest %>% 
  filter(Trade_union_ind == 1)
  #filter(farmer_union_ind == 1)
  #filter(regional_ind == 1)
  #filter(centrall_ind == 1)
  #filter(political_ind == 1)
  #filter(religious_ind == 1)

#plot the estimated Gaussian process over the integration points
gp_integ_plot1 <- I_map + geom_point(data =integ_df1, aes(x=xcoord.x, y=ycoord.x, 
                                                        color=gp_int_est), shape = 15,size = 2)+ 
  #scale_colour_gradient(low="white",high="blue") +
  theme(text = element_text(size=18),axis.title.x=element_blank(),
        axis.text.x=element_blank(), axis.ticks.x=element_blank(),axis.title.y=element_blank(),
        axis.text.y=element_blank(), axis.ticks.y=element_blank())+
  scale_color_distiller(palette = "Spectral") +
  labs(col = "Est.\n\u03C9(s)", title = "Trade Union GP")

gp_integ_plot1

intens_integ_plot1 <- I_map + geom_point(data =integ_df1, aes(x=xcoord.x, y=ycoord.x, 
                                                            color=intens_est), shape = 15,size = 2)+ 
  #scale_colour_gradient(low="white",high="blue") +
  geom_point(data = farmers_protest1, aes(x=longitude, y = latitude), size=0.5) + 
  theme(text = element_text(size=18),axis.title.x=element_blank(),
        axis.text.x=element_blank(), axis.ticks.x=element_blank(),axis.title.y=element_blank(),
        axis.text.y=element_blank(), axis.ticks.y=element_blank())+
  scale_color_distiller(palette = "Spectral") +
  labs(col = "Est.\nlog(\u03BB(s))", title = "Trade Union Intensity")

intens_integ_plot1

#plot 2: farmer union
#process data
integ_df2 <- sample_processing(samp2, burnin, integ_df, 
                               lattice, distance_latt, max_dist,
                               fix_phi)

farmers_protest2 <- farmers_protest %>% 
  #filter(Trade_union_ind == 1)
  filter(farmer_union_ind == 1)
#filter(regional_ind == 1)
#filter(centrall_ind == 1)
#filter(political_ind == 1)
#filter(religious_ind == 1)

#plot the estimated Gaussian process over the integration points
gp_integ_plot2 <- I_map + geom_point(data =integ_df2, aes(x=xcoord.x, y=ycoord.x, 
                                                          color=gp_int_est), shape = 15,size = 2)+ 
  #scale_colour_gradient(low="white",high="blue") +
  theme(text = element_text(size=18),axis.title.x=element_blank(),
        axis.text.x=element_blank(), axis.ticks.x=element_blank(),axis.title.y=element_blank(),
        axis.text.y=element_blank(), axis.ticks.y=element_blank())+
  scale_color_distiller(palette = "Spectral") +
  labs(col = "Est.\n\u03C9(s)", title = "Farmer Union GP")

gp_integ_plot2

intens_integ_plot2 <- I_map + geom_point(data =integ_df2, aes(x=xcoord.x, y=ycoord.x, 
                                                              color=intens_est), shape = 15,size = 2)+ 
  #scale_colour_gradient(low="white",high="blue") +
  geom_point(data = farmers_protest2, aes(x=longitude, y = latitude), size=0.5) + 
  theme(text = element_text(size=18),axis.title.x=element_blank(),
        axis.text.x=element_blank(), axis.ticks.x=element_blank(),axis.title.y=element_blank(),
        axis.text.y=element_blank(), axis.ticks.y=element_blank())+
  scale_color_distiller(palette = "Spectral") +
  labs(col = "Est.\nlog(\u03BB(s))", title = "Farmer Union Intensity")

intens_integ_plot2

#plot 3: Regional Organizations
#process data
integ_df3 <- sample_processing(samp3, burnin, integ_df, 
                               lattice, distance_latt, max_dist,
                               fix_phi)

farmers_protest3 <- farmers_protest %>% 
  #filter(Trade_union_ind == 1)
  #filter(farmer_union_ind == 1)
  filter(regional_ind == 1)
#filter(centrall_ind == 1)
#filter(political_ind == 1)
#filter(religious_ind == 1)

#plot the estimated Gaussian process over the integration points
gp_integ_plot3 <- I_map + geom_point(data =integ_df3, aes(x=xcoord.x, y=ycoord.x, 
                                                          color=gp_int_est), shape = 15,size = 2)+ 
  #scale_colour_gradient(low="white",high="blue") +
  theme(text = element_text(size=18),axis.title.x=element_blank(),
        axis.text.x=element_blank(), axis.ticks.x=element_blank(),axis.title.y=element_blank(),
        axis.text.y=element_blank(), axis.ticks.y=element_blank())+
  scale_color_distiller(palette = "Spectral") +
  labs(col = "Est.\n\u03C9(s)", title = "Regional Org GP")

gp_integ_plot3

intens_integ_plot3 <- I_map + geom_point(data =integ_df3, aes(x=xcoord.x, y=ycoord.x, 
                                                              color=intens_est), shape = 15,size = 2)+ 
  #scale_colour_gradient(low="white",high="blue") +
  geom_point(data = farmers_protest3, aes(x=longitude, y = latitude), size=0.5) + 
  theme(text = element_text(size=18),axis.title.x=element_blank(),
        axis.text.x=element_blank(), axis.ticks.x=element_blank(),axis.title.y=element_blank(),
        axis.text.y=element_blank(), axis.ticks.y=element_blank())+
  scale_color_distiller(palette = "Spectral") +
  labs(col = "Est.\nlog(\u03BB(s))", title = "Regional Org Intensity")

intens_integ_plot3

#plot 4: central union
#process data
integ_df4 <- sample_processing(samp4, burnin, integ_df, 
                               lattice, distance_latt, max_dist,
                               fix_phi)

farmers_protest4 <- farmers_protest %>% 
  #filter(Trade_union_ind == 1)
  #filter(farmer_union_ind == 1)
  #filter(regional_ind == 1)
  filter(centrall_ind == 1)
#filter(political_ind == 1)
#filter(religious_ind == 1)

#plot the estimated Gaussian process over the integration points
gp_integ_plot4 <- I_map + geom_point(data =integ_df4, aes(x=xcoord.x, y=ycoord.x, 
                                                          color=gp_int_est), shape = 15,size = 2)+ 
  #scale_colour_gradient(low="white",high="blue") +
  theme(text = element_text(size=18),axis.title.x=element_blank(),
        axis.text.x=element_blank(), axis.ticks.x=element_blank(),axis.title.y=element_blank(),
        axis.text.y=element_blank(), axis.ticks.y=element_blank())+
  scale_color_distiller(palette = "Spectral") +
  labs(col = "Est.\n\u03C9(s)", title = "Central Org GP")

gp_integ_plot4

intens_integ_plot4 <- I_map + geom_point(data =integ_df4, aes(x=xcoord.x, y=ycoord.x, 
                                                              color=intens_est), shape = 15,size = 2)+ 
  #scale_colour_gradient(low="white",high="blue") +
  geom_point(data = farmers_protest4, aes(x=longitude, y = latitude), size=0.5) + 
  theme(text = element_text(size=18),axis.title.x=element_blank(),
        axis.text.x=element_blank(), axis.ticks.x=element_blank(),axis.title.y=element_blank(),
        axis.text.y=element_blank(), axis.ticks.y=element_blank())+
  scale_color_distiller(palette = "Spectral") +
  labs(col = "Est.\nlog(\u03BB(s))", title = "Central Org Intensity")

intens_integ_plot4

#plot 5: political group
#process data
integ_df5 <- sample_processing(samp5, burnin, integ_df, 
                               lattice, distance_latt, max_dist,
                               fix_phi)

farmers_protest5 <- farmers_protest %>% 
  #filter(Trade_union_ind == 1)
  #filter(farmer_union_ind == 1)
  #filter(regional_ind == 1)
  #filter(centrall_ind == 1)
  filter(political_ind == 1)
  #filter(religious_ind == 1)

#plot the estimated Gaussian process over the integration points
gp_integ_plot5 <- I_map + geom_point(data =integ_df5, aes(x=xcoord.x, y=ycoord.x, 
                                                          color=gp_int_est), shape = 15,size = 2)+ 
  #scale_colour_gradient(low="white",high="blue") +
  theme(text = element_text(size=18),axis.title.x=element_blank(),
        axis.text.x=element_blank(), axis.ticks.x=element_blank(),axis.title.y=element_blank(),
        axis.text.y=element_blank(), axis.ticks.y=element_blank())+
  scale_color_distiller(palette = "Spectral") +
  labs(col = "Est.\n\u03C9(s)", title = "Political Org GP")

gp_integ_plot5

intens_integ_plot5 <- I_map + geom_point(data =integ_df5, aes(x=xcoord.x, y=ycoord.x, 
                                                              color=intens_est), shape = 15,size = 2)+ 
  #scale_colour_gradient(low="white",high="blue") +
  geom_point(data = farmers_protest5, aes(x=longitude, y = latitude), size=0.5) + 
  theme(text = element_text(size=18),axis.title.x=element_blank(),
        axis.text.x=element_blank(), axis.ticks.x=element_blank(),axis.title.y=element_blank(),
        axis.text.y=element_blank(), axis.ticks.y=element_blank())+
  scale_color_distiller(palette = "Spectral") +
  labs(col = "Est.\nlog(\u03BB(s))", title = "Political Org Intensity")

intens_integ_plot5

#plot 6: religious group
#process data
integ_df6 <- sample_processing(samp6, burnin, integ_df, 
                               lattice, distance_latt, max_dist,
                               fix_phi)

farmers_protest6 <- farmers_protest %>% 
  #filter(Trade_union_ind == 1)
  #filter(farmer_union_ind == 1)
  #filter(regional_ind == 1)
  #filter(centrall_ind == 1)
  #filter(political_ind == 1)
  filter(religious_ind == 1)

#plot the estimated Gaussian process over the integration points
gp_integ_plot6 <- I_map + geom_point(data =integ_df6, aes(x=xcoord.x, y=ycoord.x, 
                                                          color=gp_int_est), shape = 15,size = 2)+ 
  #scale_colour_gradient(low="white",high="blue") +
  theme(text = element_text(size=18),axis.title.x=element_blank(),
        axis.text.x=element_blank(), axis.ticks.x=element_blank(),axis.title.y=element_blank(),
        axis.text.y=element_blank(), axis.ticks.y=element_blank())+
  scale_color_distiller(palette = "Spectral") +
  labs(col = "Est.\n\u03C9(s)", title = "Religious Org GP")

gp_integ_plot6

intens_integ_plot6 <- I_map + geom_point(data =integ_df6, aes(x=xcoord.x, y=ycoord.x, 
                                                              color=intens_est), shape = 15,size = 2)+ 
  #scale_colour_gradient(low="white",high="blue") +
  geom_point(data = farmers_protest6, aes(x=longitude, y = latitude), size=0.5) + 
  theme(text = element_text(size=18),axis.title.x=element_blank(),
        axis.text.x=element_blank(), axis.ticks.x=element_blank(),axis.title.y=element_blank(),
        axis.text.y=element_blank(), axis.ticks.y=element_blank())+
  scale_color_distiller(palette = "Spectral") +
  labs(col = "Est.\nlog(\u03BB(s))", title = "Religious Org Intensity")

intens_integ_plot6

#plot 7: police
#process data
integ_df7 <- sample_processing(samp7, burnin, integ_df, 
                               lattice, distance_latt, max_dist,
                               fix_phi)

farmers_protest7 <- farmers_protest %>% 
  #filter(Trade_union_ind == 1)
  #filter(farmer_union_ind == 1)
  #filter(regional_ind == 1)
  #filter(centrall_ind == 1)
  #filter(political_ind == 1)
  #filter(religious_ind == 1)
  filter(police_ind == 1)

#plot the estimated Gaussian process over the integration points
gp_integ_plot7 <- I_map + geom_point(data =integ_df7, aes(x=xcoord.x, y=ycoord.x, 
                                                          color=gp_int_est), shape = 15,size = 2)+ 
  #scale_colour_gradient(low="white",high="blue") +
  theme(text = element_text(size=18),axis.title.x=element_blank(),
        axis.text.x=element_blank(), axis.ticks.x=element_blank(),axis.title.y=element_blank(),
        axis.text.y=element_blank(), axis.ticks.y=element_blank())+
  scale_color_distiller(palette = "Spectral") +
  labs(col = "Est.\n\u03C9(s)", title = "Police GP")

gp_integ_plot7

intens_integ_plot7 <- I_map + geom_point(data =integ_df7, aes(x=xcoord.x, y=ycoord.x, 
                                                              color=intens_est), shape = 15,size = 2)+ 
  #scale_colour_gradient(low="white",high="blue") +
  geom_point(data = farmers_protest7, aes(x=longitude, y = latitude), size=0.5) + 
  theme(text = element_text(size=18),axis.title.x=element_blank(),
        axis.text.x=element_blank(), axis.ticks.x=element_blank(),axis.title.y=element_blank(),
        axis.text.y=element_blank(), axis.ticks.y=element_blank())+
  scale_color_distiller(palette = "Spectral") +
  labs(col = "Est.\nlog(\u03BB(s))", title = "Police Intensity")

intens_integ_plot7


#gp plots
library(gridExtra)

cairo_pdf(file = "./Output/images/all_gp.pdf",
    width = 13, 
    height = 6.5) 

grid.arrange(gp_integ_plot1, gp_integ_plot2, gp_integ_plot3,
             gp_integ_plot4, gp_integ_plot5, #gp_integ_plot6,
             gp_integ_plot7,
             nrow =2)

dev.off()

cairo_pdf(file = "./Output/images/all_intensity.pdf",
          width = 13, 
          height = 6.5) 

#intensity plots
grid.arrange(intens_integ_plot1, intens_integ_plot2, intens_integ_plot3,
             intens_integ_plot4, intens_integ_plot5, #intens_integ_plot6,
             intens_integ_plot7,
             nrow =2)

dev.off()


###
### Calculate credible intervals
###

#check of trace plots
plot(samp1$beta2[burnin:nrow(samp1)], type = "l") 
plot(samp2$beta2[burnin:nrow(samp2)], type = "l")  
plot(samp3$beta2[burnin:nrow(samp3)], type = "l") 
plot(samp4$beta2[burnin:nrow(samp4)], type = "l") 
plot(samp5$beta2[burnin:nrow(samp5)], type = "l") 
plot(samp6$beta2[burnin:nrow(samp6)], type = "l")  #doesn't converge
plot(samp7$beta2[burnin:nrow(samp7)], type = "l") 

#CI for all of the samples
cred_int <- apply(samp1[burnin:nrow(samp1),116:121],
                  2,
                  quantile,
                  probs=c(0.5, 0.025,0.975)) %>% t() #%>% round(2)
cred_int

cred_int_fn <- function(new_samps){
  #CI for all of the samples
  cred_int <- apply(new_samps[burnin:nrow(new_samps),116:121],
                    2,
                    quantile,
                    probs=c(0.5, 0.025,0.975)) %>% t()
  
  #format cred_int table
  new_row <- NULL
  for(i in 1: nrow(cred_int)){
    if(i == 4){
      new_row <- c(new_row, 
                   paste(round(cred_int[i,1], 6), " (", 
                         round(cred_int[i,2], 6),
                         ", ", 
                         round(cred_int[i,3], 6), ")", sep = ""))
    }else{
      new_row <- c(new_row, 
                   paste(round(cred_int[i,1], 2), " (", 
                         round(cred_int[i,2], 2),
                         ", ", 
                         round(cred_int[i,3], 2), ")", sep = ""))
    }
  }
  return(new_row)
}


all_ints <- rbind(cred_int_fn(samp1),
              cred_int_fn(samp2),
              cred_int_fn(samp3),
              cred_int_fn(samp4),
              cred_int_fn(samp5),
              cred_int_fn(samp7)) %>% as.data.frame()

all_ints

colnames(all_ints) <- c("beta0", "beta1", "beta2", "beta3",
                        "beta4", "sigma")

library(xtable)
xtable(all_ints[,1:3])
xtable(all_ints[,4:6])
