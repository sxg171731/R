###############################################################
# Title:        ps5.r
# Author:       Sai Krishna Gollapally
# Date:         2017-11-21
# Description:  Problem set5
###############################################################


rm(list=ls(all=TRUE))

## Import packages
library(data.table)
library(tseries)
library(plyr)
library(expm)



context1 <- read.csv("WAGE1.csv")
context1 <- context1[,1:21]#cdata we are interested in

seed        <-	2	#Set the seed to be 2

maxClusters	<-	10
wss	<- rep(-1,maxClusters)#storage space for clusters


for (i in 1:maxClusters) { # i represents the k value
  set.seed(seed)
  model <- kmeans(context1,centers=i,nstart=10)#no. of initial random sets=10
  wss[i] <- model$tot.withinss
}

plot(1:maxClusters,	
     wss, 
     type="b", 
     xlab="Number of Clusters",
     ylab="Aggregate Within Group SS")


set.seed(seed)
model1 <- kmeans(context1,centers=3,nstart=10)
model1$centers

##    educ     exper    tenure
#1 5.900926 10.96296 38.777778 
#2 5.206958 13.12548  5.946768  
#3 7.105806 12.72258 20.638710   

groups1 <- model1$cluster
groups1



context1$cluster <- groups1

model2 <- lm(context1$wage~context1$educ+context1$exper+context1$tenure,data=subset(context1,cluster==1))
model3 <- lm(context1$wage~context1$educ+context1$exper+context1$tenure,data=subset(context1,cluster==2))
model4 <- lm(context1$wage~context1$educ+context1$exper+context1$tenure,data=subset(context1,cluster==3))

table(groups1)
##table(groups1)
#groups1
#1   2   3 
#108 263 155 

summary(model2)


## summary(model2)
#
#Call:
#  lm(formula = context1$wage ~ context1$educ + context1$exper + 
#       context1$tenure, data = subset(context1, cluster == 1))
#
#Residuals:
#  Min      1Q  Median      3Q     Max 
#-7.6498 -1.7708 -0.6407  1.2051 14.7201 
#
#Coefficients:
#                   Estimate Std. Error t value Pr(>|t|)    
#  (Intercept)     -2.91354    0.73172  -3.982 7.81e-05 ***
#  context1$educ    0.60268    0.05148  11.708  < 2e-16 ***
#  context1$exper   0.02252    0.01210   1.861   0.0633 .  
#  context1$tenure  0.17002    0.02173   7.825 2.83e-14 ***
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#
#Residual standard error: 3.096 on 522 degrees of freedom
#Multiple R-squared:  0.3072,	Adjusted R-squared:  0.3032 
#F-statistic: 77.15 on 3 and 522 DF,  p-value: < 2.2e-16

summary(model3)

## summary(model3)
#
#Call:
#  lm(formula = context1$wage ~ context1$educ + context1$exper + 
#       context1$tenure, data = subset(context1, cluster == 2))
#
#Residuals:
#  Min      1Q  Median      3Q     Max 
#-7.6498 -1.7708 -0.6407  1.2051 14.7201 
#
#Coefficients:
#                   Estimate Std. Error t value Pr(>|t|)    
#  (Intercept)     -2.91354    0.73172  -3.982 7.81e-05 ***
#  context1$educ    0.60268    0.05148  11.708  < 2e-16 ***
#  context1$exper   0.02252    0.01210   1.861   0.0633 .  
#  context1$tenure  0.17002    0.02173   7.825 2.83e-14 ***
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#
#Residual standard error: 3.096 on 522 degrees of freedom
#Multiple R-squared:  0.3072,	Adjusted R-squared:  0.3032 
#F-statistic: 77.15 on 3 and 522 DF,  p-value: < 2.2e-16



summary(model4)


## summary(model4)
#
#Call:
#  lm(formula = context1$wage ~ context1$educ + context1$exper + 
#       context1$tenure, data = subset(context1, cluster == 3))
#
#Residuals:
#  Min      1Q  Median      3Q     Max 
#-7.6498 -1.7708 -0.6407  1.2051 14.7201 
#
#Coefficients:
#                   Estimate Std. Error t value Pr(>|t|)    
#  (Intercept)     -2.91354    0.73172  -3.982 7.81e-05 ***
#  context1$educ    0.60268    0.05148  11.708  < 2e-16 ***
#  context1$exper   0.02252    0.01210   1.861   0.0633 .  
#  context1$tenure  0.17002    0.02173   7.825 2.83e-14 ***
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#
#Residual standard error: 3.096 on 522 degrees of freedom
#Multiple R-squared:  0.3072,	Adjusted R-squared:  0.3032 
#F-statistic: 77.15 on 3 and 522 DF,  p-value: < 2.2e-16


model1$centers
##Results concentrating on educ, exper, tenure
##   educ     exper    tenure   
# 10.96296 38.777778 11.657407 
# 13.12548  5.946768  1.699620 
# 12.72258 20.638710  6.316129   

################   INTERPRETATIONS   ###########################################################
# a. From the Elbow test, the optimal number of clusters seems to be four.
# 
# b.Group1 has people with high tenure and high experience but lower education. Their wage
# is intermediate.Group 2 has people with low tenure and experience, but education seeems to be
# high  and their wage is lowest among the 3 groups .Group3 is people with intermediate educ, exper an d tenure and
# their wage is highest in the 3 clusters.
# Education has a bigger positive effect in cluster3. Tenure hasa bigger effect 
# in cluster2 and experience has a negative impacton wage in model2. So there are 
# differences in the slopes of the variables across models.

context2 <- read.csv("ffportfolios.csv",header = TRUE)


ht <- lapply(context2[,2:33], kpss.test, null="Level")
pvalues=ldply(ht, function(x){ x$p.value })
pvalues # null hypothesis of stationarity  P-val=0.1>0.05

# series is thus stationary

#           .id  V1
# 1   Portfolio1 0.1
# 2   Portfolio2 0.1
# 3   Portfolio3 0.1
# 4   Portfolio4 0.1
# 5   Portfolio5 0.1
# 6   Portfolio6 0.1
# 7   Portfolio7 0.1
# 8   Portfolio8 0.1
# 9   Portfolio9 0.1
# 10 Portfolio10 0.1
# 11 Portfolio11 0.1
# 12 Portfolio12 0.1
# 13 Portfolio13 0.1
# 14 Portfolio14 0.1
# 15 Portfolio15 0.1
# 16 Portfolio16 0.1
# 17 Portfolio17 0.1
# 18 Portfolio18 0.1
# 19 Portfolio19 0.1
# 20 Portfolio20 0.1
# 21 Portfolio21 0.1
# 22 Portfolio22 0.1
# 23 Portfolio23 0.1
# 24 Portfolio24 0.1
# 25 Portfolio25 0.1
# 26 Portfolio26 0.1
# 27 Portfolio27 0.1
# 28 Portfolio28 0.1
# 29 Portfolio29 0.1
# 30 Portfolio30 0.1
# 31 Portfolio31 0.1
# 32 Portfolio32 0.1

Xdata <- context2[,2:33] # exclude the year
head(Xdata)

# Portfolio1 Portfolio2 Portfolio3 Portfolio4 Portfolio5 Portfolio6 Portfolio7 Portfolio8 Portfolio9
# 1    -0.6303    -1.3831    -0.8584    -0.4752     3.0868    -0.2290    -0.5140     1.8833    -0.9524
# 2     2.1275     9.7474     2.6705     5.1990     4.9748     5.3315     4.3054     5.4376     1.7310
# 3    -1.4491    -2.1223    -0.6562    -2.7478    -1.5514     2.0085    -0.7079    -1.5795    -1.8127
# 4    -2.1709     1.4728     2.6810     1.2080    -0.3104     1.7763     1.0502     2.8078     1.5738
# 5    -5.6849     1.0131    -2.4075    -2.6606    -4.1675    -0.7733    -0.7810     0.5658    -0.2538
# 6    -3.3542    -1.6390    -0.6652     0.7612     2.7973     1.4057     2.2849    -1.3074    -2.2618
# Portfolio10 Portfolio11 Portfolio12 Portfolio13 Portfolio14 Portfolio15 Portfolio16 Portfolio17
# 1     -1.9358      2.2221     -1.5476     -2.3904     -0.1962     13.4953      2.7106      1.8018
# 2      4.0029      5.6458      5.8969      6.9951      5.2414     12.4100     12.6767     16.3717
# 3     -1.8270     -4.2746     -4.9770     -1.2637     -0.8032     -1.1625     -4.6962     -4.5627
# 4      0.9798      0.1074      9.7216      3.8569      2.5706      0.2977      4.8649     -7.1713
# 5     -0.0220     -0.3149      0.0248     -0.6527     -1.4519     -0.9368      1.4635     -1.2876
# 6      1.0976      3.2736      2.9545     -0.3656     -0.2608     -2.8359     -1.4982     -2.3652
# Portfolio18 Portfolio19 Portfolio20 Portfolio21 Portfolio22 Portfolio23 Portfolio24 Portfolio25
# 1     -0.3969     -0.9998      0.1553      1.7109      0.0724     -0.1627      0.1853      0.6341
# 2      5.6487      7.2521      5.8085      2.1484      4.6768      4.9391      5.3859      3.8510
# 3      2.6954     -1.3249     -1.0535     -3.8938      0.7542     -2.4360     -4.6588     -2.2603
# 4      0.2341      2.5584      6.9272     -2.4213      2.7799      2.4508      0.2058      2.3818
# 5     -1.1647     -0.0714     -1.7442     -1.6909      2.6664     -0.5078      1.1336      0.2453
# 6      1.7661      2.4718      2.3818      3.3088      0.6013      2.7780      1.9853      4.6375
# Portfolio26 Portfolio27 Portfolio28 Portfolio29 Portfolio30 Portfolio31 Portfolio32
# 1      0.7000      1.4714      0.5163     -1.2438     -0.1809     -0.1078     -0.6712
# 2      4.9008      3.7275      6.0250      6.7866     10.2469      4.9839      8.1044
# 3     -1.2649     -1.0634     -4.0657     -1.2221      0.1073     -0.6151     -2.4192
# 4     -0.6060      2.9504      2.8116      1.1834      5.9007      8.5468      2.8353
# 5     -2.6463     -0.5497      2.7447     -1.0409     -2.0949      0.9850      1.1559
# 6      3.0363      1.1556      0.1292      3.5370     -1.2592      2.9668     -1.1745

model5<-prcomp(Xdata) #Principal component analyis of Xdata
screeplot(model5,type="lines") 
model5$rotation[,1:3]*100 # because we have 3 prin. comp.s 
model5$rotation
summary(model5)

# Importance of components%s:
#   PC1     PC2     PC3     PC4     PC5     PC6     PC7     PC8     PC9    PC10
# Standard deviation     28.253 8.22247 7.63427 5.71354 4.33045 3.74526 3.55615 3.09698 2.76608 2.63047
# Proportion of Variance  0.733 0.06208 0.05352 0.02998 0.01722 0.01288 0.01161 0.00881 0.00703 0.00635
# Cumulative Proportion   0.733 0.79504 0.84856 0.87853 0.89575 0.90863 0.92025 0.92905 0.93608 0.94243
# PC11    PC12    PC13    PC14    PC15    PC16    PC17    PC18   PC19    PC20
# Standard deviation     2.42692 2.36524 2.29882 2.08474 2.06489 1.93285 1.85176 1.78643 1.7134 1.67454
# Proportion of Variance 0.00541 0.00514 0.00485 0.00399 0.00392 0.00343 0.00315 0.00293 0.0027 0.00257
# Cumulative Proportion  0.94784 0.95298 0.95783 0.96182 0.96574 0.96917 0.97231 0.97524 0.9779 0.98052
# PC21    PC22    PC23    PC24    PC25    PC26    PC27    PC28    PC29    PC30
# Standard deviation     1.60604 1.56737 1.51772 1.46433 1.39495 1.35280 1.31589 1.26471 1.20909 1.08629
# Proportion of Variance 0.00237 0.00226 0.00212 0.00197 0.00179 0.00168 0.00159 0.00147 0.00134 0.00108
# Cumulative Proportion  0.98288 0.98514 0.98725 0.98922 0.99101 0.99269 0.99428 0.99575 0.99709 0.99818
# PC31    PC32
# Standard deviation     1.04701 0.94374
# Proportion of Variance 0.00101 0.00082
# Cumulative Proportion  0.99918 1.00000

# first principal components in context2
context2$factor<-model5$x[,1]

#  standardize the factor variable 
context2$factor<-scale(context2$factor)
colMeans(context2$factor)
sd(context2$factor)
context2_sub<-subset(context2, context2$factor < (-2.58))
context2_sub

summary(model5) 
# 
# Importance of components%s:
#   PC1     PC2     PC3     PC4     PC5     PC6     PC7     PC8     PC9    PC10
# Standard deviation     28.253 8.22247 7.63427 5.71354 4.33045 3.74526 3.55615 3.09698 2.76608 2.63047
# Proportion of Variance  0.733 0.06208 0.05352 0.02998 0.01722 0.01288 0.01161 0.00881 0.00703 0.00635
# Cumulative Proportion   0.733 0.79504 0.84856 0.87853 0.89575 0.90863 0.92025 0.92905 0.93608 0.94243
# PC11    PC12    PC13    PC14    PC15    PC16    PC17    PC18   PC19    PC20
# Standard deviation     2.42692 2.36524 2.29882 2.08474 2.06489 1.93285 1.85176 1.78643 1.7134 1.67454
# Proportion of Variance 0.00541 0.00514 0.00485 0.00399 0.00392 0.00343 0.00315 0.00293 0.0027 0.00257
# Cumulative Proportion  0.94784 0.95298 0.95783 0.96182 0.96574 0.96917 0.97231 0.97524 0.9779 0.98052
# PC21    PC22    PC23    PC24    PC25    PC26    PC27    PC28    PC29    PC30
# Standard deviation     1.60604 1.56737 1.51772 1.46433 1.39495 1.35280 1.31589 1.26471 1.20909 1.08629
# Proportion of Variance 0.00237 0.00226 0.00212 0.00197 0.00179 0.00168 0.00159 0.00147 0.00134 0.00108
# Cumulative Proportion  0.98288 0.98514 0.98725 0.98922 0.99101 0.99269 0.99428 0.99575 0.99709 0.99818
# PC31    PC32
# Standard deviation     1.04701 0.94374
# Proportion of Variance 0.00101 0.00082
# Cumulative Proportion  0.99918 1.00000
# 

###############INTERPRETATIONS########################
#a.principle components are 3.



