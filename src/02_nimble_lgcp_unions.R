###
### Preliminary LGCP
###
### Farmers Protest Data
### 
### Created:       10/09/22
### Last modified: 01/05/23
### 

#clean up
rm(list = ls())


#libraries
library(nimble)
library(beepr)
library(spatstat)
library(sp)
library(fields)
library(mvtnorm)
library(batchmeans)
library(raster)
library(tictoc)
library(matlib)
library(tidyverse)
library(mcmcse)
library(corrplot)

### Set working directory
#Claire's System:
#setwd("C:/Users/ckell/Desktop/Google Drive/Research/farmers_protest")
setwd("C:/Users/ckelling/Dropbox (Carleton College)/Carleton College/Research/farmers_protest")
#Manasvi's System:
#setwd("/Users/manasvikhanna/Desktop/Farmer's Protest/farmers_protest_git")

#set the seed
set.seed(3)

#loading spatial data, cleaned in 00_processing_data.R (both areal and point-level data)
load(file = "data/working/areal_point_comp.Rdata")
#farmers_protest <- read.csv(file = "data/working/farmers_prio.csv")
farmers_protest <- read.csv(file = "data/working/farmers_union_prio.csv")

#load integration data
integ_df <- read.csv(file = "data/working/prio_integ.csv")

#load nimble function
source(file = "src/00_nimble_functions.R")

#############################################################################
### Overall data processing
#############################################################################

#need to do a little cleaning of farmers protest data (only this year)
farmers_protest <- farmers_protest %>% filter(year...6 > 2019)

#remove observations with missing covariate values, need to investigate more later
farmers_protest <- farmers_protest %>% filter(!is.na(cmr_mean))

#multicollinearity investigation
res <- cor(farmers_protest[,c("agri_gc", "irrig_sum", "harvarea","barren_gc",
                              "cmr_mean", "pop_gpw_sum", "pop_hyd_sum")], use = "complete.obs")
#make correlation plot
corrplot(res, 
         type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45,
         addCoef.col = "black")

#format spatial data for nimble
n = nrow(farmers_protest)
#Xmat = as.matrix(farmers_protest[,c("agri_ih", "pop_gpw_sum", "cmr_mean")])
Xmat = as.matrix(farmers_protest[,c("agri_gc", "irrig_sum", "harvarea","barren_gc",
                                    "cmr_mean", "union_bin")]) #pop_gpw_sum

#plot the variable of interest
ggplot() + geom_point(data = integ_df, aes(x = xcoord.x, y= ycoord.y, col = harvarea)) +
  scale_color_distiller(palette= "Spectral")

#calculate area of the shapes
area <- area(state_shape3)

#####
##### Integration setup
#####

#remove integration points on the islands
integ_df <- integ_df %>% filter(!is.na(cmr_mean))

#need integration points with covariates
X_integration <- as.matrix(integ_df[,c("agri_gc", "irrig_sum", "harvarea","barren_gc",
                                       "cmr_mean")]) #pop_gpw_sum

#number of points to integrate over
n_int = nrow(X_integration)


#May need to standardize X variables in both integration df and farmers protest data
#X_integration[,1:3] <- X_integration[,1:3]/10000
#Xmat[,1:3] <- Xmat[,1:3]/10000

###
### Initalize Nimble
###
n_total <- nrow(farmers_protest)

#try without r_s
r_s = rep(1,nrow(farmers_protest))
r_s_int = rep(1,nrow(integ_df))

area = sum(area/100000000000) #scaled area


#####
##### Knots for Predictive Processes
#####
#Create a lattice (grid) over the surface, may want to increase this later ***
lattice <- expand.grid(lon = seq(bbox(state_shape3)[1,1],bbox(state_shape3)[1,2],length.out = 20), 
                       lat = seq(bbox(state_shape3)[2,1],bbox(state_shape3)[2,2],length.out = 20))
sp_lattice <- lattice

#subset to points inside window
coordinates(sp_lattice) <- ~lon+lat
proj4string(sp_lattice) <- proj4string(state_shape3)
over_latt <- over(sp_lattice,state_shape3)

lattice <- bind_cols(lattice, over_latt)

lattice <- lattice[-which(is.na(over_latt$ST_NM)),]

#plot(lattice$lon, lattice$lat)

#Create a lattice (grid) over the surface
#for use in the ll function
#subset to only lat/long variables
lattice <- lattice[,c("lon", "lat")]
latt_matrix <- as.matrix(lattice)

n_knot = nrow(latt_matrix)


###
### Creating all distance matrices
###
# For predictive processes
dist_knotuof_nk = as.matrix(rdist(farmers_protest[,c("longitude", "latitude")], latt_matrix))
dist_knot_int = as.matrix(rdist(integ_df[,c("xcoord.x", "ycoord.x")], latt_matrix))

#distance matrix for knots
distance_latt <- as.matrix(rdist(latt_matrix)) 

#distance matrix for integration (large)
distance_integ <- as.matrix(rdist(integ_df[,c("xcoord.x", "ycoord.x")]))

#full distance matrix for the covariance matrix, data
distMatMod = as.matrix(rdist(farmers_protest[,c("longitude", "latitude")]))

#check
max(dist_knotuof_nk, dist_knot_int, distance_integ, distance_latt, distMatMod)

#calculate max distance possible for distance matrix standardization
max_dist <- dist(rbind(c(bbox(state_shape3)[1,1],bbox(state_shape3)[2,1]),
                       c(bbox(state_shape3)[1,2],bbox(state_shape3)[2,2])))
max_dist <- as.numeric(max_dist)

#standardize all distance matrices
dist_knotuof_nk <- dist_knotuof_nk/max_dist
dist_knot_int <- dist_knot_int/max_dist
distance_integ <- distance_integ/max_dist
distance_latt <- distance_latt/max_dist
distMatMod <- distMatMod/max_dist

#check
max(dist_knotuof_nk, dist_knot_int, distance_integ, distance_latt, distMatMod)


###
### Fixing phi
### 
#dist_q1 <- as.numeric(quantile(distMatMod)[2])
#dist_q2 <- as.numeric(quantile(distMatMod)[3])
#dist_q3 <- as.numeric(quantile(distMatMod)[4])
dist_95 <- as.numeric(quantile(distMatMod, probs = c(0.95)))
dist_05 <- as.numeric(quantile(distMatMod, probs = c(0.05)))

#deciding range of uniform prior distribution for phi
upper_unif <- -dist_95/log(0.05)
lower_unif <- -dist_05/log(0.95)

#deciding fixed value for phi
fix_phi <- (upper_unif+lower_unif)/2


#############################################################################
### Basic LGCP Model
#############################################################################

one = 1
#Data processing for pred processes
consts   <- list(n_total = n_total, n_int = n_int, n_knot = n_knot,
                 area = area, X=Xmat, 
                 X_integration = X_integration,
                 dists= distMatMod, dist_knotuof_nk = dist_knotuof_nk, 
                 distance_integ = distance_integ, dist_knot_int = dist_knot_int,
                 mn=rep(0,n_total), mn_int=rep(0,n_int), mn_knot = rep(0,n_knot),
                 distance_latt = distance_latt,
                 r_s = r_s, r_s_int = r_s_int, 
                 phi = fix_phi,
                 one = one)

inits    <- list(beta1=1,#beta2=0.001, 
                 beta2= 0.000001, beta3=0.000001,
                 beta4=1, beta5=1,
                 #beta1=0.000001,beta2=0.000001, 
                 sigma=2,w_s=rnorm(n_total), W_int=rnorm(n_int),W_predproc = rnorm(n_knot),
                 beta0 = 2)

#-1.106516e+172

#Model code
model_string <- nimbleCode({
  # Parameter Model
  #priors for parameters
  beta0 ~  dnorm(0, sd=sqrt(100))
  beta1 ~  dnorm(0, sd=sqrt(100))
  beta2 ~  dnorm(0, sd=sqrt(100))
  beta3 ~  dnorm(0, sd=sqrt(100))
  beta4 ~  dnorm(0, sd=sqrt(100))
  beta5 ~  dnorm(0, sd=sqrt(100))
  #beta2 ~  dnorm(0, sd=sqrt(100))
  
  sigma   ~  dinvgamma(shape = 2, scale = 0.5) #from Liang 2008 and Banerjee 2015
  sigma_union   ~  dinvgamma(shape = 2, scale = 0.5) #from Liang 2008 and Banerjee 2015

  
  ###
  ### Gaussian Process
  ###
  # Process Model, with predictive processes (n_knot < n_total)
  covMat_predproc[1:n_knot,1:n_knot]<- expcov(distance_latt[1:n_knot,1:n_knot],phi=phi, sigma =sigma)
  #evaluate over knots
  W_predproc[1:n_knot] ~ dmnorm(mean = mn_knot[1:n_knot], cov = covMat_predproc[1:n_knot,1:n_knot])
  
  # Process Model, predictive process
  w_s[1:n_total] <- (expcov(dist_knotuof_nk[1:n_total,1:n_knot], phi=phi, sigma=sigma)%*%inverse(expcov(distance_latt[1:n_knot, 1:n_knot], phi=phi, sigma))%*%W_predproc[1:n_knot])[,1]
  #Process model for integration
  W_int[1:n_int] <- (expcov(dist_knot_int[1:n_int,1:n_knot], phi=phi, sigma=sigma)%*%inverse(expcov(distance_latt[1:n_knot, 1:n_knot], phi=phi, sigma=sigma))%*%W_predproc[1:n_knot])[,1]
  
  
  ###
  ### Trying a GP for the indicator variable of union involvement
  ###
  # Process Model, with predictive processes (n_knot < n_total)
  covMat_predproc[1:n_knot,1:n_knot]<- expcov(distance_latt[1:n_knot,1:n_knot],phi=phi, sigma =sigma_union)
  #evaluate over knots
  W_predproc_union[1:n_knot] ~ dmnorm(mean = mn_knot[1:n_knot], cov = covMat_predproc[1:n_knot,1:n_knot])
  
  # Process Model, predictive process
  w_s_union[1:n_total] <- (expcov(dist_knotuof_nk[1:n_total,1:n_knot], phi=phi, sigma=sigma_union)%*%inverse(expcov(distance_latt[1:n_knot, 1:n_knot], phi=phi, sigma=sigma_union))%*%W_predproc_union[1:n_knot])[,1]
  #Process model for integration
  W_int_union[1:n_int] <- (expcov(dist_knot_int[1:n_int,1:n_knot], phi=phi, sigma=sigma_union)%*%inverse(expcov(distance_latt[1:n_knot, 1:n_knot], phi=phi, sigma=sigma_union))%*%W_predproc_union[1:n_knot])[,1]
  
  ###
  ### Spatial portion of intensity: over data points
  ###
  XB_spatial[1:n_total]<- beta0 + beta1*X[,1] + beta2*X[,2] + beta3*X[,3] +
    beta4*X[,4] + beta5*X[,5] + X[,6]*w_s_union[1:n_total]
  
  ###
  ### Integral over region (lambda(D))
  ### 
  #Monte Carlo Method:
  XB_int[1:n_int]<- beta0 + beta1*X_integration[,1] + beta2*X_integration[,2] +
    beta3*X_integration[,3] + beta4*X_integration[,4] + beta5*X_integration[,5] #how to incorporate GP?
  
  
  one ~ d_lgcp(lam0 = 1, W_int = W_int[1:n_int], w_s = w_s[1:n_total],
               XB_int = XB_int[1:n_int], XB_spatial = XB_spatial[1:n_total],
               area =area,
               r_s = r_s[1:n_total], r_s_int = r_s_int[1:n_int]) 
  
})



#Create the model
Rmodel <- nimbleModel(model_string, inits = inits, constants = consts)

Rmodel$initializeInfo()

#configure MCMC
mcmcConf <- configureMCMC(Rmodel, enableWAIC = TRUE, 
                          monitors = c("beta0", "beta1","beta2",
                                       "beta3", "beta4", "beta5",
                                       "sigma", "W_predproc",
                                       "sigma_union", "W_predproc_union"))


length(Rmodel$getNodeNames())

#check to make sure this is finite
Rmodel$getLogProb("one")


#Build MCMC
pt<-proc.time()
Rmcmc <- buildMCMC(mcmcConf)
ptFinal<-proc.time()-pt

comp_model <- compileNimble(Rmodel) 

comp_MCMC <- compileNimble(Rmcmc)

tic()
comp_MCMC$run(niter = 200000)
#comp_MCMC$run(niter = 5000)
toc()

#comp_MCMC$calculateWAIC(nburnin = 1000)

samples_df <- as.matrix(comp_MCMC$mvSamples) %>% as.data.frame()
samples_df_orig <- samples_df

#look at the posterior distribution
plot(samples_df$beta0, type = "l")
plot(samples_df$beta1, type = "l")
plot(samples_df$beta2, type = "l")
plot(samples_df$beta3, type = "l")
plot(samples_df$beta4, type = "l")
plot(samples_df$beta5, type = "l")
plot(samples_df$sigma, type = "l")


#remove burnin and replot
burnin <- 1000
samples_df <- samples_df[burnin:nrow(samples_df),]

#look at the posterior distribution after removing burnin
plot(samples_df$beta0, type = "l")
plot(samples_df$beta1, type = "l")
plot(samples_df$beta2, type = "l")
plot(samples_df$beta3, type = "l")
plot(samples_df$beta4, type = "l")
plot(samples_df$beta5, type = "l")
plot(samples_df$sigma, type = "l")


#34,000 sec for 200,000 iterations (9 hours)

#save occasional output
#write.csv(samples_df_orig, file = "data/working/mcmc_samp_03_16.csv")
#samples_df <- read.csv(file = "data/working/mcmc_samp_03_16.csv")

burnin <- 50000
mean(samples_df$beta1[burnin:nrow(samples_df)])

###
### Rescale by population, incorporate unions and police, 
###     need to review how that was done, if we are confident
###




###
### Assess convergence
###

#effective sample size

#MCMC SE

###
### Plot estimate of intensity function for each of the integration points
###

#store estimates of the parameters after removing burnin
burnin <- 50000
#burnin <- 1
param_est <- colMeans(samples_df[burnin:nrow(samples_df),])


# "agri_gc", "irrig_sum", "harvarea","barren_gc",
# "cmr_mean"
#parametric intensity function (without GP) on integration points
integ_logintensity <- param_est[115] + param_est[116]*integ_df$agri_gc+
  param_est[117]*integ_df$irrig_sum + 
  param_est[118]*integ_df$harvarea + 
  param_est[119]*integ_df$barren_gc+
  param_est[120]*integ_df$cmr_mean

gp_knots_est <- param_est[1:114]

#transform the estimated GP over the notes to over the integration points
#need to estimate sigma from the data
sig_est <- param_est[121]

#transform the GP over the integration points
dist_knot_int = as.matrix(fields::rdist(integ_df[,c("xcoord.x", "ycoord.x")], lattice))
dist_knot_int <- dist_knot_int/max_dist
gp_int <- (expCov(dist_knot_int, phi=fix_phi, sigma=sig_est)%*%solve(expCov(distance_latt, phi=fix_phi, sigma=sig_est))%*%gp_knots_est)

integ_df$gp_int_est <- gp_int[,1]

integ_df$intens_est <- (integ_logintensity + integ_df$gp_int_est)

#plot the estimated Gaussian process over the integration points
gp_integ_plot <- ggplot() + geom_point(data =integ_df, aes(x=xcoord.x, y=ycoord.x, 
                                                           color=gp_int_est), shape = 15,size = 2)+ 
  #scale_colour_gradient(low="white",high="blue") +
  theme(text = element_text(size=18),axis.title.x=element_blank(),
        axis.text.x=element_blank(), axis.ticks.x=element_blank(),axis.title.y=element_blank(),
        axis.text.y=element_blank(), axis.ticks.y=element_blank())+
  scale_color_distiller(palette = "Spectral") +
  labs(col = "Estimated \u03C9(s)")

gp_integ_plot

intens_integ_plot <- ggplot() + geom_point(data =integ_df, aes(x=xcoord.x, y=ycoord.x, 
                                                               color=intens_est), shape = 15,size = 2)+ 
  #scale_colour_gradient(low="white",high="blue") +
  theme(text = element_text(size=18),axis.title.x=element_blank(),
        axis.text.x=element_blank(), axis.ticks.x=element_blank(),axis.title.y=element_blank(),
        axis.text.y=element_blank(), axis.ticks.y=element_blank())+
  scale_color_distiller(palette = "Spectral") +
  labs(col = "Estimated\nlog(\u03BB(s))")

intens_integ_plot


###
### Next steps:
###         - how to incorporate union_bin into the integral? It is a nonspatial variable?
###