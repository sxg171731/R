###############################################################
# Title:        ps-3.r
# Author:       Sai Krishna Gollapally
# Date:         2017-10-31
# Description:  Turn-in product for problem set 3
###############################################################

rm(list=ls(all=TRUE))

## Import packages
library(data.table)
context    <- fread('hprice1.csv')

model1<- lm(price~bdrms+lotsize+sqrft , data=context)
summary(model1)

# Residuals:
#   Min       1Q   Median       3Q      Max 
# -120.026  -38.530   -6.555   32.323  209.376 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -2.177e+01  2.948e+01  -0.739  0.46221    
# bdrms        1.385e+01  9.010e+00   1.537  0.12795    
# lotsize      2.068e-03  6.421e-04   3.220  0.00182 ** 
#   sqrft        1.228e-01  1.324e-02   9.275 1.66e-14 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 59.83 on 84 degrees of freedom
# Multiple R-squared:  0.6724,	Adjusted R-squared:  0.6607 
# F-statistic: 57.46 on 3 and 84 DF,  p-value: < 2.2e-16

# plot(context$bdrms,model1$residuals)
# plot(context$lotsize,model1$residuals)
# plot(context$sqrft,model1$residuals)


library(sandwich) # For White correction
library(lmtest) # More advanced hypothesis testing tools

vcov(model1) # Standard error matrix for coefficients
vcovHC(model1) # White-corrected standard error matrix
coeftest(model1,vcov.=vcov)
# t test of coefficients:
#   
#   Estimate  Std. Error t value  Pr(>|t|)    
# (Intercept) -2.1770e+01  2.9475e+01 -0.7386  0.462208    
# bdrms        1.3853e+01  9.0101e+00  1.5374  0.127945    
# lotsize      2.0677e-03  6.4213e-04  3.2201  0.001823 ** 
#   sqrft        1.2278e-01  1.3237e-02  9.2751 1.658e-14 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
coeftest(model1,vcov.=vcovHC) 
# t test of coefficients:
#   
#   Estimate  Std. Error t value Pr(>|t|)   
# (Intercept) -21.7703086  41.0326944 -0.5306 0.597124   
# bdrms        13.8525219  11.5617901  1.1981 0.234236   
# lotsize       0.0020677   0.0071485  0.2893 0.773101   
# sqrft         0.1227782   0.0407325  3.0143 0.003406 **
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
lnlotsize<-log(context$lotsize)
lnsqrft<-log(context$sqrft)
model2<- lm(log(price)~bdrms+log(lotsize)+log(sqrft) , data=context)
summary(model2)

# Residuals:
#   Min       1Q   Median       3Q      Max 
# -0.68422 -0.09178 -0.01584  0.11213  0.66899 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  -1.29704    0.65128  -1.992   0.0497 *  
#   bdrms         0.03696    0.02753   1.342   0.1831    
# log(lotsize)  0.16797    0.03828   4.388 3.31e-05 ***
#   log(sqrft)    0.70023    0.09287   7.540 5.01e-11 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.1846 on 84 degrees of freedom
# Multiple R-squared:  0.643,	Adjusted R-squared:  0.6302 
# F-statistic: 50.42 on 3 and 84 DF,  p-value: < 2.2e-16


# plot(context$bdrms,model2$residuals)
# plot(context$lnlotsize,model2$residuals)
# plot(context$lnsqrft,model2$residuals)

vcov(model2) # Standard error matrix for coefficients
vcovHC(model2) # White-corrected standard error matrix
coeftest(model2,vcov.=vcov)

# t test of coefficients:
#   
#   Estimate Std. Error t value  Pr(>|t|)    
# (Intercept)  -1.297042   0.651284 -1.9915   0.04967 *  
#   bdrms         0.036958   0.027531  1.3424   0.18308    
# log(lotsize)  0.167967   0.038281  4.3877 3.307e-05 ***
#   log(sqrft)    0.700232   0.092865  7.5403 5.006e-11 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

coeftest(model2,vcov.=vcovHC)

# t test of coefficients:
#   
#   Estimate Std. Error t value  Pr(>|t|)    
# (Intercept)  -1.297042   0.850457 -1.5251  0.130988    
# bdrms         0.036958   0.035576  1.0389  0.301845    
# log(lotsize)  0.167967   0.053275  3.1528  0.002243 ** 
#   log(sqrft)    0.700232   0.121392  5.7683 1.298e-07 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#---------------INTERPRETATIONS--------------------
#
# a. lot size and squarefeet are significant variable in the OLS test,
# 
# b.Only sqrft is significant after white corrected significant test.
# 
# c.log(lotsize) and log(sqrft) are significant using OLS test for model2
# 
# d.Even after white correction log(lotsize) and log(sqrft) are significant.
#
# e.Taking the logs has removed the heteroskedacity.There is no much change in the
#   significance even after white corrected test.


rm(list=ls(all=TRUE))
context2<-fread('beveridge.csv')
model3<-lm(urate~vrate,data=context2)
summary(model3)
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  17.1194     0.5920   28.92   <2e-16 ***
#   vrate        -3.7414     0.2068  -18.09   <2e-16 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
coeftest(model3, vcov. = vcov)
# t test of coefficients:
#   
#   Estimate Std. Error t value  Pr(>|t|)    
# (Intercept) 17.11942    0.59200  28.918 < 2.2e-16 ***
#   vrate       -3.74145    0.20681 -18.091 < 2.2e-16 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

coeftest(model3,vcov=NeweyWest(model3,lag=5))
# t test of coefficients:
#   
#   Estimate Std. Error t value  Pr(>|t|)    
# (Intercept) 17.11942    1.36561  12.536 < 2.2e-16 ***
#   vrate       -3.74145    0.39575  -9.454 < 2.2e-16 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

library(tseries)
kpss.test(log(context2$urate),null="Level")
kpss.test(log(context2$urate),null="Trend")
kpss.test(log(context2$vrate),null="Level")
kpss.test(log(context2$vrate),null="Trend")

ts.plot(context2$urate)
ts.plot(context2$vrate)

kpss.test(diff(log(context2$urate)),null="Level")#good
kpss.test(diff(log(context2$urate)),null="Trend") 
kpss.test(diff(log(context2$vrate)),null="Level") #good
kpss.test(diff(log(context2$vrate)),null="Trend")

ts.plot(diff(context2$urate))
ts.plot(diff(context2$vrate))


kpss.test(diff(diff(log(context2$urate))),null="Level")#good
kpss.test(diff(diff(log(context2$urate))),null="Trend") #GOOD
kpss.test(diff(diff(log(context2$vrate))),null="Level") #good
kpss.test(diff(diff(log(context2$vrate))),null="Trend")#GOOD

ts.plot(diff(diff(context2$urate)))
ts.plot(diff(diff(context2$vrate)))


model4<-lm(diff(log(urate))~diff(log(vrate)),data=context2)
summary(model4)
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)  
# (Intercept)       0.006208   0.002667   2.328   0.0215 *
#   diff(log(vrate)) -0.019990   0.042921  -0.466   0.6422  
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
coeftest(model4, vcov. = vcov)

# t test of coefficients:
#   
#   Estimate Std. Error t value Pr(>|t|)  
# (Intercept)       0.0062082  0.0026671  2.3277  0.02145 *
#   diff(log(vrate)) -0.0199899  0.0429211 -0.4657  0.64217  
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

coeftest(model4,vcov=NeweyWest(model4,lag=5))
# t test of coefficients:
  #   Estimate Std. Error t value Pr(>|t|)
# (Intercept)       0.0062082  0.0041604  1.4922   0.1380
# diff(log(vrate)) -0.0199899  0.0321314 -0.6221   0.5349

##----------------INTERPRETATIONS-----------------------#########

# f. Coeiffcients on the vacany rate are significant in both OLS and NeweyWest
# g.Based on the Kpss finding we have to do logarithmic and first order differential 
#   transformation on the Vacany rate before modeling.
# h.Based on the Kpss finding we have to do logarithmic and first order differential 
#   transformation on the Vacany rate before modeling.
# i.In the model 3 both OLS and Neweywest shows vacancy rate as significant, whereas 
#   in the model4  Both OLS and Neweywest shows vacancy rate as not significant.
# j.Model3 has high R value than Model4. Model3 better describes the data.
#   



library(plm)
rm(list=ls(all=TRUE))
context3<- fread("JTRAIN.csv")
context3 <- plm.data(context3, index=c("fcode","year"))

context3$d88 <- ifelse(context3$year == 1988, 1,0)
context3$d89 <- ifelse(context3$year == 1989, 1,0)
table(context3$grant)


for(i in 1:nrow(context3)){
  if(context3$year[i] == 1987)
    context3$grantlast[i] <- 0
  else if(context3$grant[i-1] == 1)
    context3$grantlast[i] <- 1
  else
    context3$grantlast[i] <- 0
}

model5 <- plm(log(scrap) ~ d88 + d89 + grant + grantlast, model ="pooling", data=context3)
model6 <- plm(log(scrap) ~ d88 + d89 + grant + grantlast, model ="within", data=context3)


coeftest(model5)
# t test of coefficients:
#              Estimate Std. Error t value Pr(>|t|)   
# (Intercept)  0.597434   0.203063  2.9421 0.003754 **
# d88         -0.239370   0.310864 -0.7700 0.442447   
# d89         -0.496524   0.337928 -1.4693 0.143748   
# grant        0.200020   0.338285  0.5913 0.555186   
# grantlast    0.048936   0.436066  0.1122 0.910792   
# ---
# Signif. codes:  0 '*' 0.001 '*' 0.01 '' 0.05 '.' 0.1 ' ' 1

coeftest(model6) 
# t test of coefficients:
#            Estimate Std. Error t value Pr(>|t|)  
# d88       -0.080216   0.109475 -0.7327  0.46537  
# d89       -0.247203   0.133218 -1.8556  0.06634 .
# grant     -0.252315   0.150629 -1.6751  0.09692 .
# grantlast -0.421590   0.210200 -2.0057  0.04749 *
#   ---
# Signif. codes:  0 '*' 0.001 '*' 0.01 '' 0.05 '.' 0.1 ' ' 1

summary(model6, vcov=vcovHC(model6, method = "arellano")) 

# Oneway (individual) effect Within Model
# 
# Note: Coefficient variance-covariance matrix supplied: vcovHC(model6, method = "arellano")
# 
# Call:
#   plm(formula = log(scrap) ~ d88 + d89 + grant + grantlast, data = context3, 
#       model = "within")
# 
# Balanced Panel: n=54, T=3, N=162
# 
# Residuals :
#   Min.   1st Qu.    Median   3rd Qu.      Max. 
# -2.286936 -0.112387 -0.017841  0.144272  1.426674 
# 
# Coefficients :
#            Estimate Std. Error t-value Pr(>|t|)  
# d88       -0.080216   0.095719 -0.8380  0.40393  
# d89       -0.247203   0.192514 -1.2841  0.20197  
# grant     -0.252315   0.140329 -1.7980  0.07507 .
# grantlast -0.421590   0.276335 -1.5256  0.13013  
# ---
# Signif. codes:  0 '*' 0.001 '*' 0.01 '' 0.05 '.' 0.1 ' ' 1
# 
# Total Sum of Squares:    32.25
# Residual Sum of Squares: 25.766
# R-Squared:      0.20105
# Adj. R-Squared: -0.23684
# F-statistic: 7.38691 on 4 and 53 DF, p-value: 8.3412e-05

##------------ INTERPRETATIONS------------------------##
# K. Giving Grant to increase productivity is associated with a 20% increase in the 
#   scrap rate of items produced.
# l.Having Grant to increase productivity in the last year is associated with a 
#   4.82% increase in the scrape rate of items produced in the current year.
# m. Signs of Both B1 and B2 are positive . That indicates having grant in the 
#   current year and last year is associated with a increase in the scrap rate 
#   percentage.
# n.Giving Grant to increase productivity is associated with a 25.23% decrease in the 
# scrap rate of items produced.
# o.Giving Grant to increase productivity in the last year is associated with 
#   a 42.15% decrease in the scrap rate of items produced.
# p.Signs of Both B1 and B2 are Negative . That indicates having grant in the 
#   current year and last year is associated with a increase in the scrap rate
#   percentage.
# q. significance results change from OLS to HAC.In OLS grantlast is the only significant
#   variable with p value less than 0.05 whereas in HAC there is no significant 
#   variables whose p value is less than 0.05.