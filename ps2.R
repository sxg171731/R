###############################################################
# Title:        ps-2.r
# Author:       Sai Krishna Gollapally
# Date:         2017-09-25
# Description:  Turn-in product for problem set 2
###############################################################


###################  QUESTION 1 ####################################

rm(list=ls(all=TRUE))

## Import packages
library(data.table)

## Data import and validation
context1    <- fread('attend.csv')
summary(context1)


#---------------------------------------------------------------------------------------------------
#  storage   display    value
#variable name   type    format     label      variable label
#---------------------------------------------------------------------------------------------------
#  attend          byte    %8.0g                 classes attended out of 32
#termGPA         float   %9.0g                 GPA for term
#priGPA          float   %9.0g                 cumulative GPA prior to term
#ACT             byte    %8.0g                 ACT score
#final           byte    %8.0g                 final exam score
#frosh           byte    %8.0g                 =1 if freshman
#soph            byte    %8.0g                 =1 if sophomore
#hw              byte    %8.0g                 number of homeworks turned in out of 8
#---------------------------------------------------------------------------------------------------



## creating attendrt and hwrt  variables
attendrt <- (context1$attend/32)
hwrt <- (context1$hw/8)


## Run models
model1 <- lm(termGPA~priGPA+ACT+attendrt+hwrt,data=context1)

## summarize model
summary(model1)

# Residuals:
#   Min       1Q   Median       3Q      Max 
# -1.87210 -0.28100  0.00001  0.30164  1.49711 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -1.286983   0.164169  -7.839 1.77e-14 ***
#   priGPA       0.548962   0.042418  12.942  < 2e-16 ***
#   ACT          0.036099   0.006051   5.966 3.92e-09 ***
#   attendrt     1.052246   0.155436   6.770 2.81e-11 ***
#   hwrt         0.913031   0.116932   7.808 2.22e-14 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.4788 on 675 degrees of freedom
# Multiple R-squared:   0.58,	Adjusted R-squared:  0.5775 
# F-statistic:   233 on 4 and 675 DF,  p-value: < 2.2e-16

## ------------ QUESTION -1 Interpretations-----------#######
#
#  a  --> 1.052246
#         For every increase of 1 unit of   attendance rate is associated
#         with a 1.052246 increase in Term GPA.
#  b  --> 0.913031
#         For every increase of 1 unit of homework rate is 
#         associated with a 0.913031 increase in Term GPA.
#
#  c  --> 2.9096574 GPA
#          The Equation is  TermGPA=-1.286983+0.548962*(2.2)+0.036099*(32)+1.052246*(0.875)
#         +0.913031*(1)+0.4788  on calculating including the standard error we get the value
#         As 2.9096574
#
#  d  --> 2.93089505 GPA
#         The Equation is  TermGPA = -1.286983+0.548962*(3.9)+0.036099*(20)+1.052246*(0.875)
#         +0.913031*(8)+0.4788  on calculating including the standard error we get
#         the value as 2.93089505
#
# e   --> Based on the linear model summary PriorGPA seems to be more important as it has a larger
#         coefficient and it contributes more to the GPA than attendance.
#
#  f   --> 2.7711395 GPA
#         The Equation is like TermGPA = -1.286983+0.548962*(3.0)+0.036099*(25)+1.052246*(1)
#         +0.913031*(0.5)+0.4788  on calculating including the standard error we get
#         the value as 2.7711395
#
#  g   -->  2.701538 GPA
#       The Equation is like TermGPA = -1.286983+0.548962*(3.9)+0.036099*(20)+1.052246*(0.875)
#         +0.913031*(8)+0.4788  on calculating including the standard error we get
#         the value as 2.701538
#   
#   h-> Attendance rate is more important as it has high coefficient.
#
#
#   i -> It is easier to compare attendrt and hwrt because we have taken rate for both of them
#         and their values fall in the range of  0 to 10 but it is not the same with priorGPA and ACT.
#
#
#
#


#############  QUESTION 2 ###################################

## Data import and validation

context2    <- fread('CEOSAL2.csv')
summary(context2)

## Generating varaibles
lnsalary <- log(context2$salary)
lnmktval <- log(context2$mktval)
lnsales <- log(context2$sales)

model2 <- lm(lnsalary~lnmktval+profits+ceoten,data=context2)
summary(model2)



##### --- Model2 summary -----------------###############

#Residuals:
#  Min       1Q   Median       3Q      Max 
#-2.63382 -0.34660  0.00627  0.35059  1.96220 

#Coefficients:
#               Estimate Std. Error t value Pr(>|t|)    
#  (Intercept) 4.7095052  0.3954502  11.909  < 2e-16 ***
#  lnmktval    0.2386220  0.0559166   4.267 3.25e-05 ***
#  profits     0.0000793  0.0001566   0.506   0.6132    
#  ceoten      0.0114646  0.0055816   2.054   0.0415 *  
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#Residual standard error: 0.5289 on 173 degrees of freedom
#Multiple R-squared:  0.2514,	Adjusted R-squared:  0.2384 
#F-statistic: 19.36 on 3 and 173 DF,  p-value: 7.141e-11



model3 <- lm(lnsalary~lnmktval+profits+ceoten+lnsales,data=context2)
summary (model3)

######## Model 3 summary ##################
#Residuals:
#  Min       1Q   Median       3Q      Max 
#-2.48792 -0.29369  0.00827  0.29951  1.85524 

#Coefficients:
#               Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 4.558e+00  3.803e-01  11.986  < 2e-16 ***
#  lnmktval    1.018e-01  6.303e-02   1.614   0.1083    
#  profits     2.905e-05  1.503e-04   0.193   0.8470    
#  ceoten      1.168e-02  5.342e-03   2.187   0.0301 *  
#  lnsales     1.622e-01  3.948e-02   4.109 6.14e-05 ***
  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#Residual standard error: 0.5062 on 172 degrees of freedom
#Multiple R-squared:  0.3183,	Adjusted R-squared:  0.3024 
#F-statistic: 20.08 on 4 and 172 DF,  p-value: 1.387e-13

  
  
###### ---------- QUESTION 2 - INTERPRETATIONS -----------
#
#   j -> we did not use natural log of profits because there are some obseravations 
#         whose value is negative for Profit variable and natural logarithm of a negative 
#         value is not defined.

#   k ->  0.2386220
#         For every one percent Increase in market value is associated with a 
#         0.2386220% Increase in the salary of CEO.
#
#   l -> 1.018e-01
#         For every one percent Increase in market value is associated with a 
#         0.1018% Increase in the salary of CEO.
#
#
#   m -> In the model4 sales is omitted and in model5 sales variable is included.In model5 
#         the significance of mktvl is drastically reduced  after sales variable is included.
#         It is a indication of Omitted Bias. 
#
#   n-> since the P value of profits is greater than standard 0.05 it is not significant.
#
#   0 --> 1.622e-01
#         For every one percent increase in sales firm sales is associated 
#         with 0.1622% percent increase in the salary of CEO.

 ################# QUESTION 3 ########







 context3 <- fread('hprice1.csv') 
 summary(context3)
 
 
#Generating Variables
 lnlotsize=log(context3$lotsize)
 lnsqrft=log(context3$sqrft)
 lnprice=log(context3$price)

 #Run Models
 model4=lm(price~bdrms+lnlotsize+lnsqrft+colonial,data=context3)
 summary(model4)
 
 # Residuals:
 #   Min       1Q   Median       3Q      Max 
 # -109.603  -38.258   -4.325   22.984  220.766 
 # 
 # Coefficients:
 #   Estimate Std. Error t value Pr(>|t|)    
 # (Intercept) -2030.455    210.967  -9.625 3.68e-15 ***
 #   bdrms          18.572      9.308   1.995   0.0493 *  
 #   lnlotsize      61.446     12.372   4.966 3.60e-06 ***
 #   lnsqrft       225.508     30.072   7.499 6.41e-11 ***
 #   colonial        4.134     14.509   0.285   0.7764    
 # ---
 #   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
 # 
 # Residual standard error: 59.66 on 83 degrees of freedom
 # Multiple R-squared:  0.6781,	Adjusted R-squared:  0.6626 
 # F-statistic: 43.71 on 4 and 83 DF,  p-value: < 2.2e-16
 

 model5=lm(lnprice~bdrms+lnlotsize+lnsqrft+colonial,data=context3)
 summary(model5)
 
 # Residuals:
 #   Min       1Q   Median       3Q      Max 
 # -0.69479 -0.09750 -0.01619  0.09151  0.70228 
 # 
 # Coefficients:
 #   Estimate Std. Error t value Pr(>|t|)    
 # (Intercept) -1.34959    0.65104  -2.073   0.0413 *  
 #   bdrms        0.02683    0.02872   0.934   0.3530    
 # lnlotsize    0.16782    0.03818   4.395 3.25e-05 ***
 #   lnsqrft      0.70719    0.09280   7.620 3.69e-11 ***
 #   colonial     0.05380    0.04477   1.202   0.2330    
 # ---
 #   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
 # 
 # Residual standard error: 0.1841 on 83 degrees of freedom
 # Multiple R-squared:  0.6491,	Adjusted R-squared:  0.6322 
 # F-statistic: 38.38 on 4 and 83 DF,  p-value: < 2.2e-16
 
 #----------------- QUESTION-3 INTERPRETAIONS -----------------
 
  # p --> 61.446
  #       For every increase in 1% of lot size of the house is associated
  #       with a 614.46 dollars increase in price of the house
  #
  # q --> 0.16782
 #        For every increase in 1% of lot size of the house is associated 
  #       with a 0.167% increase in the price of the house.
  #
  # r --> 4.134
  #       The type of the house i,e if the house is colonial is associated
  #        with a 4134 dollars increase   in the price of the house
  # 
  # s --> Model 4 seems to be best fit among both because
  #     i) R squared value of model4 is high among both .
  #     ii) Model 4 has more number of predictor variables with p Value less than 0.05.
  #
  #
  # t--> on increase of 1 room the house of price will increase by 18.572k$ and increase of 10%
  #       sqrft associated 22.55K$ on the whole the price of the house is increased by 41.02K
  #       which is more than our required 30K so IT IS RECOMMENDED.
  #
 
 
####----------------------QUESTION-4---------------------############
 #importing data
 context4 <- fread('JTRAIN2.csv')
summary(context4)

# Run the model
model6=lm(re78~re75+train+educ+black,data=context4)
summary(model6)

# Residuals:
#   Min     1Q Median     3Q    Max 
# -9.120 -4.377 -1.756  3.353 54.058 
# 
# Coefficients:
#               Estimate Std. Error t value Pr(>|t|)   
#  (Intercept)  1.97686    1.89028   1.046   0.2962   
#   re75         0.14697    0.09811   1.498   0.1349   
#   train        1.68422    0.62700   2.686   0.0075 **
#   educ         0.41026    0.17267   2.376   0.0179 * 
#   black       -2.11277    0.82941  -2.547   0.0112 * 
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 6.496 on 440 degrees of freedom
# Multiple R-squared:  0.04917,	Adjusted R-squared:  0.04053 
# F-statistic: 5.688 on 4 and 440 DF,  p-value: 0.00018

#--------------- QUESTION-4 INTERPRETATIONS-----------
# u--> 0.14697 
#     For every increase of 1000 dollars in realearnings in 1975 is 
#     associated with increase in 146.97 dollars in realearnings in 1978.
#
# v --> 1.68422
#     If the person is assigned to training  is associated
#     with a 1684.22 dollars increase in real earnings in the year 1978.
#     Yes it is significant.
#
# w--> -2.11277
#     If the person is black is associated with a decrease
#      of 2112.77 dollars in real earnings in the year 1978.
