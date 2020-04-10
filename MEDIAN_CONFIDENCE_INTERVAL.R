# This function generates a confidence interval around the median.
# If your data has a large number of ties (especially around the median)
# the CI could be exceedingly narrow. Your results may vary.
# 2020 Edward G. Brown. distributed under the Creative Commons Attribution 
# Non-Commercial 4.0 International license(CC BY-NC 4.0) as detailed here: 
# https://creativecommons.org/licenses/by-nc/4.0/

GetCI <- function(v, clevel){

  v <- sort(v)
  n <- length(v)
  cl <- 0
  prod1 <- floor((n)*(0.50))
  prod2 <- ceiling((n)*(0.50))
  Lstar <- floor(prod1 - 1)
  Ustar <- floor(prod2 + 1)
  
  while (cl < clevel ) { 

    cl <- sum(dbinom(Lstar:Ustar,length(v),prob=0.5))
  
    Lstar <- Lstar - 1
    Ustar <- Ustar + 1
  
  }

return(list(cl,v[Lstar],v[Ustar],median(v)))
}

# Exmple

v <- c(3.15,2.31,7.35,4.46,9.51,21.42,5.20,15.85,5.68,3.29,8.34,10.35,3.76,6.17,6.99,3.75)

# returns the achieved CL and upper/lower bounds, and the median
cls <- GetCI(v,0.90) 
cls
hist(v, col="lightgreen")
abline(v=cls[2],col="blue", lwd=2)
abline(v=cls[3],col="blue", lwd=2)
abline(v=cls[4],col="red", lwd=2)