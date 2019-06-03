##############################################################

# Description:  Show recursive partitioning for classification
###############################################################

rm(list=ls())

library(data.table)
library(rpart)
library(tree)
library(party)
library(evtree)

# read raw data file (change to work on your system)
titanic	<-	read.csv("titanic.csv")
head(titanic)

# create numeric outcome variable (instead of the character var "survived")
titanic$alive		<-	ifelse(titanic$Survived=="Yes",1,0)
# titanic$alive		<-	as.numeric(titanic$Survived=="Yes")

model1 <- rpart(alive~Class+Age+Sex,method="anova",data=titanic)
model2 <- tree(alive~Class+Age+Sex,data=titanic)
model3 <- ctree(alive~Class+Age+Sex,data=titanic)
model4 <- evtree(alive~Class+Age+Sex,data=titanic)

model1
model2
model3
model4

plot(model1)
text(model1)
plot(model2)
text(model2)

plot(model3)
plot(model4)
