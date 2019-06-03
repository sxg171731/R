###############################################################
# Date:         2017-11-29
# Description:  PCA
###############################################################

rm(list=ls())
library(data.table)

############################
## Dimension reduction
############################
context1 <- read.csv("Boston.csv")
Xdata <- context1[,2:14] # exclude the price
head(Xdata)

pca <- prcomp(Xdata)
screeplot(pca,type="lines") # looks like there are 2 principal components

pca$rotation[,1:2]*100 # because we have 2 prin. comp.s 

# get the principal components
factors <- pca$x[,1:2]
head(factors)
summary(factors)
cov(factors)

# algebra to standardize the principal components
library(expm)
factors <- factors %*% solve(sqrtm(crossprod(factors))) * sqrt(nrow(factors)) 
crossprod(factors)/nrow(factors)
cov(factors) #mean zero, var one

# modeling with the factors
model9 <- lm(log(medv)~factors,data=context1)
summary(model9) # Note the R-squared here is not the best

# clustering with the factors
groups5 <- apply(factors^2,1,which.max)
context1$cluster <- groups5

## Clustering the variables using PCA
pca$rotation[,1:2]*1000
apply(pca$rotation[,1:2]^2,1,which.max)

