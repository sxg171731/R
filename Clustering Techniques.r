###############################################################
# Description:  K-means and hierarchical clustering
# Sources:      Various, Jay Scott, Walter Johnston
###############################################################

rm(list=ls())
context1 <- read.csv("Boston.csv")
context1 <- context1[,2:15]

############################
## K-means Estimation
############################
?kmeans
seed        <-	2	# NOT a good random seed!
maxClusters	<-	50 #try with 50, then with 15

## Use within-group variation to choose k
wss	<- rep(-1,maxClusters)
for (i in 1:maxClusters) {
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

############################
## Hierarchical Clustering
############################
?hclust
## Run the models
model2 <- hclust(dist(context1),method="complete")
model3 <- hclust(dist(context1),method="average")
model4 <- hclust(dist(context1),method="centroid")

## Q: How to choose k? Answer is nobody knows
plot(model2)
rect.hclust(model2, k=5, border="red")   # 5 clusters
rect.hclust(model2, k=4, border="blue")  # 4 clusters
rect.hclust(model2, k=3, border="green") # 3 clusters 

plot(model2)
rect.hclust(model3, k=5, border="red")   # 5 clusters
rect.hclust(model3, k=4, border="blue")  # 4 clusters
rect.hclust(model3, k=3, border="green") # 3 clusters 

plot(model2)
rect.hclust(model4, k=5, border="red")   # 5 clusters
rect.hclust(model4, k=4, border="blue")  # 4 clusters
rect.hclust(model4, k=3, border="green") # 3 clusters 

mins <- rep(0,10)
for(i in 1:10)
  mins[i] <- min(table(cutree(model2,k=i)))
cbind(mins)

## Find the clusters
groups2 <- cutree(model2,k=3) # cut tree into 3 clusters
  groups3 <- cutree(model3,k=3) # cut tree into 3 clusters
groups4 <- cutree(model4,k=3) # cut tree into 3 clusters


############################
## Comparisons
############################
context2 <- cbind(groups1,groups2,groups3,groups4)
for(i in 1:506) if(groups1[i]!=1) groups1[i] = 2+(1-(groups1[i]-2))

mean(as.numeric(groups1==groups2))
mean(as.numeric(groups2==groups3))
mean(as.numeric(groups2==groups4))
mean(as.numeric(groups1==groups4))


