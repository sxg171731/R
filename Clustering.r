###############################################################

# Description:  K-means, hierarchical clustering and PCA

###############################################################

rm(list=ls())
library(data.table)
context1 <- read.csv("Boston.csv")
context1 <- context1[,2:15]

############################
## K-means Estimation
############################
?kmeans
seed        <-	2	# NOT a good random seed!
maxClusters	<-	10 #try with 50, then with 15

## Use within-group variation to choose k
wss	<- rep(-1,maxClusters)
for (i in 1:maxClusters) { # i represents the k value
  set.seed(seed)
  model <- kmeans(context1,centers=i,nstart=10)
  wss[i] <- model$tot.withinss
}
plot(1:maxClusters,	wss, type="b", 
     xlab="Number of Clusters",
     ylab="Aggregate Within Group SS")

## Run the model
set.seed(seed)
model1 <- kmeans(context1,centers=3,nstart=10)

model1$centers
groups1 <- model1$cluster
groups1


############################
## Clustering in Practice
############################
context1 <- fread("Boston.csv")
context1$cluster <- groups1

model2 <- lm(log(medv)~log(nox)+rm+ptratio,data=context1[cluster==1])
model3 <- lm(log(medv)~log(nox)+rm+ptratio,data=context1[cluster==2])
model4 <- lm(log(medv)~log(nox)+rm+ptratio,data=context1[cluster==3])
model5 <- lm(log(medv)~log(nox)+rm+ptratio,data=context1)

table(groups1)
summary(model2)
summary(model3)
summary(model4)
summary(model5)


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
cov(factors)

# modeling with the factors
model9 <- lm(log(medv)~factors,data=context1)
summary(model9) # Note the R-squared here is not the best

# clustering with the factors
groups5 <- apply(factors^2,1,which.max)
NROW(apply(factors^2,1,which.max))
context1$cluster <- groups5

summary(subset(context1,cluster==1))
summary(subset(context1,cluster==2))

model6 <- lm(log(medv)~log(nox)+rm+ptratio,data=subset(context1,cluster==1))
model7 <- lm(log(medv)~log(nox)+rm+ptratio,data=subset(context1,cluster==2))
summary(model6)
summary(model7)

apply(pca$rotation[,1:2]^2,1,which.max)

