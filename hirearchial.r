###############################################################
# Title:        ex6.2.r
# Author:       Jason Parker
# Date:         2017-11-15
# Description:  K-means, hierarchical clustering and PCA
# Sources:      Various, Jay Scott, Walter Johnston
###############################################################

rm(list=ls())
context1 <- read.csv("murder.csv")


Xdata <- context1[2:ncol(context1)]
pca <- prcomp(Xdata)
screeplot(pca,type="lines") 
factors <- pca$x[,1:2]

head(factors)
summary(factors)
cov(factors)

# algebra to standardize the principal components
library(expm)
factors <- factors %*% solve(sqrtm(crossprod(factors))) * sqrt(nrow(factors)) 
crossprod(factors)/nrow(factors)
cov(factors)

ts.plot(factors[,1])
context1$Year[28]
ts.plot(factors[,2])

pca$rotation[,1:2]*100
factors[,2] <- factors[,2]*-1

ts.plot(factors)
