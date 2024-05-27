# Exponential Covariance Function
expCov<-function(distMat,phi, sigma){
  sigma^2*exp(-distMat/phi)
}


expcov2 <- nimbleFunction(run = function(dists = double(2), phi = double(0), sigma = double(0)) {
  returnType(double(2))
  sigma2 <- sigma*sigma
  result <- sigma2*exp(-dists/phi)
  return(result)
})

#allow for asymmetrical
expcov <- nimbleFunction(     
  run = function(dists = double(2), phi = double(0), sigma = double(0)) {
    returnType(double(2))
    n <- dim(dists)[1]
    m <- dim(dists)[2]
    result <- matrix(nrow = n, ncol = m, init = FALSE)
    sigma2 <- sigma*sigma
    for(i in 1:n){
      for(j in 1:m){
        result[i, j] <- sigma2*exp(-dists[i,j]/phi)
      }
    }
    
    return(result)
  })


d_nhpp <-nimbleFunction(
  run =function(x=double(0), #x is proxy data value
                lam0 = double(0), 
                XB_int = double(1), XB_spatial = double(1), 
                area = double(0), r_s = double(1), r_s_int = double(1),
                log =integer(0,default =0)) {
    returnType(double(0))
    
    lambda <- XB_spatial  + log(lam0)
    
    log_ll <- sum(lambda) + sum(log(r_s))
    
    lam_D <- lam0*mean(r_s_int*exp(XB_int))*area
    
    logProb <- log_ll - lam_D
    if(log)return(logProb)
    else return(exp(logProb))
    
  })


d_lgcp <-nimbleFunction(
  run =function(x=double(0), #x is proxy data value
                lam0 = double(0), W_int =double(1), w_s = double(1),
                XB_int = double(1), XB_spatial = double(1), 
                area = double(0), r_s = double(1), r_s_int = double(1),
                log =integer(0,default =0)) {
    returnType(double(0))
    
    lambda <- XB_spatial  + w_s + log(lam0)
    
    log_ll <- sum(lambda) + sum(log(r_s))
    
    lam_D <- lam0*mean(r_s_int*exp(XB_int + W_int))*area
    
    logProb <- log_ll - lam_D
    if(log)return(logProb)
    else return(exp(logProb))
    
  })