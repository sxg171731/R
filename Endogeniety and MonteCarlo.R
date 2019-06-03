##############################################

## Purpose:  Demonstrate endogeneity & time 
##           series using Monte Carlo
##############################################

rm(list=ls(all=TRUE))
simn  <- 1000 #repetitions

##############################################
## OLS without endogeneity
##############################################

## Define the data generating process
b0    <- 3
b1    <- 2
sig   <- 1.2
xmean <- 4
xsd   <- 2

## Simulate
for (ns in 1:5) {
  if (ns == 1) {n <- 10}
  if (ns == 2) {n <- 25}
  if (ns == 3) {n <- 50}
  if (ns == 4) {n <- 100}
  if (ns == 5) {n <- 250}
  coefs           <- rep(0,simn)
  for (isim in 1:simn) {
    # create random data
    x             <- rnorm(n,mean=xmean,sd=xsd)
    e             <- rnorm(n,mean=0,sd=sig)
    y             <- b0 + b1*x + e
    
    # estimate the ols coefficients
    bhat          <- coef(lm(y~x))
    coefs[isim]   <- bhat[2]
  }
  plot(density(coefs),xlim=c(1.5,2.5))
  if(ns!=5)invisible(readline(prompt="Press [enter] to continue"))
}

##############################################
## OLS with an omitted variable (CORRELATED with x)
##############################################

## Define the data generating process
b0    <- 3
b1    <- 2
b2    <- 1
sig   <- 1.2
xmean <- 4
xsd   <- 2
zmean <- 3
zsd   <- 1.5
xzsd  <- 1.25

## Simulate
for (ns in 1:5) {
  if (ns == 1) {n <- 10}
  if (ns == 2) {n <- 25}
  if (ns == 3) {n <- 50}
  if (ns == 4) {n <- 100}
  if (ns == 5) {n <- 250}
  coefs           <- rep(0,simn)
  for (isim in 1:simn) {
    # create random data
    w             <- rnorm(n,mean=0,sd=xzsd) #covariance between x and z
    x             <-  w + rnorm(n,mean=xmean,sd=sqrt(xsd*xsd-xzsd))
    z             <-  w + rnorm(n,mean=zmean,sd=sqrt(zsd*zsd-xzsd))
    e             <- rnorm(n,mean=0,sd=sig)
    y             <- b0 + b1*x + b2*z + e
    
    # estimate the ols coefficients WITHOUT INCLUDING z
    bhat          <- coef(lm(y~x)) # doesn't include z
    coefs[isim]   <- bhat[2]
  }
  plot(density(coefs),xlim=c(1.5,2.5))
  if(ns!=5)invisible(readline(prompt="Press [enter] to continue"))
}

##############################################
## OLS with reverse regression
##############################################

## Define the data generating process
b0    <- 3
b1    <- 2
sig   <- 1.2
xmean <- 4
xsd   <- 2

## Simulate
for (ns in 1:5) {
  if (ns == 1) {n <- 10}
  if (ns == 2) {n <- 25}
  if (ns == 3) {n <- 50}
  if (ns == 4) {n <- 100}
  if (ns == 5) {n <- 250}
  coefs           <- rep(0,simn)
  for (isim in 1:simn) {
    # create random data
    x             <- rnorm(n,mean=xmean,sd=xsd)
    e             <- rnorm(n,mean=0,sd=sig)
    y             <- b0 + b1*x + e
    
    # estimate the ols coefficients BACKWARDS
    ahat          <- coef(lm(x~y))  #estimate the reverse regression
    bhat          <- c(-ahat[1],1)/ahat[2] #flip the model
    coefs[isim]   <- bhat[2]
  }
  plot(density(coefs),xlim=c(1.5,2.5))
  if(ns!=5)invisible(readline(prompt="Press [enter] to continue"))
}



##############################################
## Time series regression data generating process
##############################################

library(lmtest)
# coeftest <- function(x,vcov.=vcov) {
#   estim <- coef(x)
#   sterr <- diag(vcov.(x))
#   tstat <- estim/sqrt(sterr)
#   pval  <- dnorm(-abs(tstat))*2
#   return(cbind(estim,sterr,tstat,pval))
# }

## Define the data generating process
b0    <- 3
b1    <- 2
sig   <- 1.2
rho   <- 0.5
xmean <- 4
xsd   <- 2
rhox  <- 0.5

## Define sample size
bigT  <- 500

# create random data
x             <- rnorm(bigT+100,mean=0,sd=xsd*sqrt(1-rhox*rhox))
e             <- rnorm(bigT+100,mean=0,sd=sig*sqrt(1-rho*rho))
for (t in 2:(bigT+100)) {
  x[t]        <- rhox*x[t-1]  + x[t]
  e[t]        <- rho*e[t-1]   + e[t]
}
x             <- x[101:(bigT+100)]
e             <- e[101:(bigT+100)]
y             <- b0 + b1*x + e

# estimate the ols coefficients
summary(lm(y~x))
model1 <- lm(y~x)
summary(model1)
model2 <- lm((y-3-2*x)~x)
summary(model2)
ctest <- coeftest(model2)
ctest[2,4]

##############################################
## TS Regression working
##############################################

simn  <- 1000 #repetitions

library(sandwich)
library(lmtest)

## Define the data generating process
b0    <- 3
b1    <- 2
sig   <- 1.2
rho   <- 0.5
xmean <- 4
xsd   <- 2
rhox  <- 0.5

## Simulate
outp  <- matrix(0,nrow=5,ncol=5) 
for (ts in 1:5) {
  if (ts == 1) {bigT <- 10}
  if (ts == 2) {bigT <- 25}
  if (ts == 3) {bigT <- 50}
  if (ts == 4) {bigT <- 100}
  if (ts == 5) {bigT <- 250}
  coefs              <- rep(0,simn)
  rejections_vcov    <- 0
  rejections_newey   <- 0
  for (isim in 1:simn) {
    # create random data
    x                <- rnorm(bigT+100,mean=0,sd=xsd*sqrt(1-rhox*rhox))
    e                <- rnorm(bigT+100,mean=0,sd=sig*sqrt(1-rho*rho))
    for (t in 2:(bigT+100)) {
      x[t]           <- rhox*x[t-1]  + x[t]
      e[t]           <- rho*e[t-1]   + e[t]
    }
    x                <- x[101:(bigT+100)]
    e                <- e[101:(bigT+100)]
    y                <- b0 + b1*x + e
    
    # estimate the ols coefficients
    bhat             <- coef(lm(y~x))
    coefs[isim]      <- bhat[2]
    model            <- lm((y-3-2*x)~x)
    ctest            <- coeftest(model)
    if(ctest[2,4] < 0.05) rejections_vcov <- rejections_vcov   + 1
    ctest            <- coeftest(model,vcov=NeweyWest(model,lag=5))
    if(ctest[2,4] < 0.05) rejections_newey <- rejections_newey + 1
  }
  outp[ts,] <- c(bigT,mean(coefs),var(coefs),rejections_vcov/simn,rejections_newey/simn)
  plot(density(coefs),xlim=c(1.5,2.5))
  if(ts!=5)invisible(readline(prompt="Press [enter] to continue"))
} 
outp


##############################################
## Unit root example
##############################################

simn  <- 1000 #repetitions

## Simulate 
for (ts in 1:5) {
  if (ts == 1) {bigT <- 10}
  if (ts == 2) {bigT <- 25}
  if (ts == 3) {bigT <- 50}
  if (ts == 4) {bigT <- 100}
  if (ts == 5) {bigT <- 250}
  rsq              <- rep(0,simn)
  for (isim in 1:simn) {
    # create random data
    x                <- rnorm(bigT+100)
    y                <- rnorm(bigT+100)
    # x                <- cumsum(x)
    # y                <- cumsum(y)
    x                <- x[101:(bigT+100)]
    y                <- y[101:(bigT+100)]
    
    # estimate the R-squared
    rsq[isim]      <- summary(lm(y~x))$r.squared
    
    ## First differences
    # dx               <- diff(x)
    # dy               <- diff(y)
    # rsq[isim]      <- summary(lm(dy~dx))$r.squared
  }
  plot(density(rsq))
  if(ts!=5)invisible(readline(prompt="Press [enter] to continue"))
} 
outp


