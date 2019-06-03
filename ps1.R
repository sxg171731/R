###############################################################
# Title:        ps-1.r
# Author:       Sai Krishna Gollapally
# Date:         2017-09-11
# Description:  Turn-in product for problem set 1
###############################################################


rm(list=ls(all=TRUE))


## Import packages
library(data.table)

## Data import and validation
context1    <- fread('Wage1.csv')
summary(context1)
#---------------------------------------------------------------------------------------------------
 # storage   display    value
#variable name   type    format     label      variable label
#---------------------------------------------------------------------------------------------------
#  wage            float   %8.2g                 average hourly earnings
#educ            byte    %8.0g                 years of education
#exper           byte    %8.0g                 years potential experience
#tenure          byte    %8.0g                 years with current employer
#nonwhite        byte    %8.0g                 =1 if nonwhite
#female          byte    %8.0g                 =1 if female
#married         byte    %8.0g                 =1 if married
#numdep          byte    %8.0g                 number of dependents
#smsa            byte    %8.0g                 =1 if live in SMSA
#northcen        byte    %8.0g                 =1 if live in north central U.S
#south           byte    %8.0g                 =1 if live in southern region
#west            byte    %8.0g                 =1 if live in western region
#construc        byte    %8.0g                 =1 if work in construc. indus.
#ndurman         byte    %8.0g                 =1 if in nondur. manuf. indus.
#trcommpu        byte    %8.0g                 =1 if in trans, commun, pub ut
#trade           byte    %8.0g                 =1 if in wholesale or retail
#services        byte    %8.0g                 =1 if in services indus.
#profserv        byte    %8.0g                 =1 if in prof. serv. indus.
#profocc         byte    %8.0g                 =1 if in profess. occupation
#clerocc         byte    %8.0g                 =1 if in clerical occupation
#servocc         byte    %8.0g                 =1 if in service occupation
#---------------------------------------------------------------------------------------------------


## Generate new variables
lwage <- log(context1$wage)

## Run models
model1 <- lm(wage~educ, data=context1)
model2 <- lm(wage~educ+exper+tenure, data=context1)
model3 <- lm(lwage~educ+exper+tenure, data=context1)

## Summarize
summary(model1)

# Residuals:
#  Min      1Q  Median      3Q     Max 
# -5.3707 -2.1578 -0.9854  1.1864 16.3975 

# Coefficients:
# Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -0.93389    0.68769  -1.358    0.175    
# educ         0.54470    0.05346  10.189   <2e-16 ***
## ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# Residual standard error: 3.392 on 524 degrees of freedom
# Multiple R-squared:  0.1654,	Adjusted R-squared:  0.1638 
# F-statistic: 103.8 on 1 and 524 DF,  p-value: < 2.2e-16

summary(model2)

#Residuals:
#  Min      1Q  Median      3Q     Max 
# -7.6498 -1.7708 -0.6407  1.2051 14.7201 

# Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -2.91354    0.73172  -3.982 7.81e-05 ***
# educ         0.60268    0.05148  11.708  < 2e-16 ***
# exper        0.02252    0.01210   1.861   0.0633 .  
# tenure       0.17002    0.02173   7.825 2.83e-14 ***
#  ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#Residual standard error: 3.096 on 522 degrees of freedom
#Multiple R-squared:  0.3072,	Adjusted R-squared:  0.3032 
#F-statistic: 77.15 on 3 and 522 DF,  p-value: < 2.2e-16

summary(model3)


#Residuals:
#  Min       1Q   Median       3Q      Max 
#-2.05911 -0.29563 -0.03302  0.28590  1.42657 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept) 0.282635   0.104331   2.709  0.00697 ** 
# educ        0.092256   0.007340  12.569  < 2e-16 ***
# exper       0.004137   0.001726   2.397  0.01687 *  
# tenure      0.022112   0.003098   7.138 3.19e-12 ***
# ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# Residual standard error: 0.4415 on 522 degrees of freedom
# Multiple R-squared:  0.3165,	Adjusted R-squared:  0.3125 
# F-statistic: 80.56 on 3 and 522 DF,  p-value: < 2.2e-16



## Interpretations
#----------------------------------------------------
#  a  --> 0.54470
#       Every increase in 2 years of education 
#       is with a increase in 1.1$ average hourly wage of workers at employee level
#       
#  b   --> 0.60268
#        Every increase in 2 years of education
#       is with a increase in 1.21$ average hourly wage of workers at employee level
#
#  c  -->  0.02252
#       Every Increase in 1 year of  potential experience is associated with a
#       increase in 0.225$ average hourly wage of workers at employee level
#
#  d  --> 0.17002
#       Every Increase in 1 year tenure with current employer is associated with
#       increae in 0.17 average hourly wage
#
#  e   --> 2.91354
#
#  f   --> 0.092256
#       Every increase in 1 year of education  is with a 9.22%
#       increase in  average hourly wage of workers at employee level
#
#  g  --> 0.004137
#       Every Increase in 1 year of  potential experience is associated with a
#       0.41% increase in  average hourly wage of workers at employee level
#   
#  h    --> 0.022112
#       Every Increase in 1 year tenure with current employer is associated with
#       2.21% increae in  average hourly wage
#
#
###########################################################################################