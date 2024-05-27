###
### Preliminary NHPP
###
### Farmers Protest Data
### 
### Created:       10/09/22
### Last modified: 10/17/22
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

### Set working directory
#Claire's System:
setwd("C:/Users/ckell/Desktop/Google Drive/Research/farmers_protest")
#Manasvi's System:
setwd("/Users/manasvikhanna/Desktop/Farmer's Protest/farmers_protest_git")

#set the seed
set.seed(3)

#loading spatial data, cleaned in 00_processing_data.R (both areal and point-level data)
load(file = "data/working/areal_point_comp.Rdata")
#load integration data
load(file = "data/working/india_integ_points.Rdata")

#load nimble function
source(file = "src/00_nimble_functions.R")

#############################################################################
### Overall data processing
#############################################################################

#format spatial data for nimble
n = nrow(farmers_protest)
Xmat = as.matrix(farmers_protest[,c("Net Area Sown", "Total Holdings Area", "X2021_pop")])

#calculate area of the shapes
area <- area(state_shape3)

#####
##### Integration setup
#####

#need integration points with covariates
X_integration <- as.matrix(integ_df[,c("Net Area Sown", "Total Holdings Area", "X2021_pop")])

#number of points to integrate over
n_int = nrow(X_integration)


###
### Initalize Nimble
###
n_total <- nrow(farmers_protest)

#try without r_s
r_s = rep(1,nrow(farmers_protest))
r_s_int = rep(1,nrow(integ_df))

area = sum(area/1000000000) #scaled area
#############################################################################
### Basic NHPP Model
#############################################################################

one = 1
#Data processing for pred processes
consts   <- list(n_total = n_total, n_int = n_int, 
                 area = area, X=Xmat, 
                 X_integration = X_integration,
                 r_s = r_s, r_s_int = r_s_int, 
                 one = one)

inits    <- list(beta1=0.00001,beta2=0.00001, 
                 beta0 = 2)


#Model code
model_string <- nimbleCode({
  # Parameter Model
  #priors for parameters
  beta0 ~  dnorm(0, sd=sqrt(100))
  beta1 ~  dnorm(0, sd=sqrt(100))
  beta2 ~  dnorm(0, sd=sqrt(100))

  #spatial portion
  XB_spatial[1:n_total]<- beta0 + beta1*X[,1]+ beta2*X[,2] 
  
  ###
  ### Integral over region (lambda(D))
  ### 
  #Monte Carlo Method:
  XB_int[1:n_int]<- beta0 + beta1*X_integration[,1]+ beta2*X_integration[,2] 
  
  one ~ d_nhpp(lam0 = 1,
               XB_int = XB_int[1:n_int], XB_spatial = XB_spatial[1:n_total],
               area =area,
               r_s = r_s[1:n_total], r_s_int = r_s_int[1:n_int]) 
  
})



#Create the model
Rmodel <- nimbleModel(model_string, inits = inits, constants = consts)

Rmodel$initializeInfo()

#configure MCMC
mcmcConf <- configureMCMC(Rmodel, enableWAIC = TRUE, 
                          monitors = c("beta0", "beta1","beta2"))



length(Rmodel$getNodeNames())

#check to make sure this is finite
Rmodel$getLogProb("one")


#Build MCMC
pt<-proc.time()
Rmcmc <- buildMCMC(mcmcConf)
ptFinal<-proc.time()-pt

comp_model <- compileNimble(Rmodel) # running into an error here 

comp_MCMC <- compileNimble(Rmcmc)

tic()
comp_MCMC$run(niter = 2000)
toc()

#comp_MCMC$calculateWAIC(nburnin = 1000)

samples_df <- as.matrix(comp_MCMC$mvSamples) %>% as.data.frame()

#look at the posterior distribution
plot(samples_df$beta0, type = "l")
plot(samples_df$beta1, type = "l")
plot(samples_df$beta2, type = "l")

#remove burnin and replot
burnin <- 1000
samples_df <- samples_df[burnin:nrow(samples_df),]

#look at the posterior distribution
plot(samples_df$beta0, type = "l")
plot(samples_df$beta1, type = "l")
plot(samples_df$beta2, type = "l")

