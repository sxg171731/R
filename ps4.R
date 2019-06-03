###############################################################
# Title:        ps-4.r
# Author:       Sai Krishna Gollapally
# Date:         2017-11-21
# Description:  Turn-in product for problem set 4
###############################################################

rm(list=ls(all=TRUE))
library(data.table)
library(sandwich)
library(lmtest) 
library(glmx)
library(partykit)
library(grid)
library(evtree)
library(leaps)
 
# creating new variables
context1 <- fread("htv.csv")
abilsq <- context1$abil^2
educsq <- context1$educ^2
expersq<- context1$exper^2
abileduc<-context1$abil*context1$educ
abilexper <-context1$abil*context1$exper
educexper <- context1$educ*context1$exper
lwage <- log(context1$wage)

model1 <- lm(log(wage)~abil+educ+exper,data=context1)
summary(model1)

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 0.612140   0.176546   3.467 0.000544 ***
#   abil        0.056510   0.008627   6.551 8.41e-11 ***
#   educ        0.101724   0.009831  10.347  < 2e-16 ***
#   exper       0.034811   0.006682   5.209 2.22e-07 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.5303 on 1226 degrees of freedom
# Multiple R-squared:  0.2043,	Adjusted R-squared:  0.2023 
# F-statistic: 104.9 on 3 and 1226 DF,  p-value: < 2.2e-16

c(AIC(model1),BIC(model1))

#[1] 1935.995 1961.569


model2<- lm(log(wage)~abil+educ+exper+abilsq+educsq+expersq+abileduc+abilexper+educexper,data=context1)
summary(model2)
step(model2,direction="backward",k=2)

# Call:
#   lm(formula = log(wage) ~ abil + educ + exper + abilsq + educsq + 
#        expersq + abileduc + abilexper + educexper, data = context1)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -2.20501 -0.29599  0.02712  0.31079  1.76016 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)   
# (Intercept)  2.5454981  1.8954648   1.343  0.17954   
# abil        -0.0039040  0.1051421  -0.037  0.97039   
# educ        -0.0296430  0.1841934  -0.161  0.87217   
# exper       -0.1539877  0.1481153  -1.040  0.29871   
# abilsq      -0.0106648  0.0034642  -3.079  0.00213 **
#   educsq      -0.0003806  0.0047865  -0.080  0.93663   
# expersq      0.0016526  0.0030570   0.541  0.58888   
# abileduc     0.0104544  0.0063383   1.649  0.09933 . 
# abilexper   -0.0047758  0.0039160  -1.220  0.22287   
# educexper    0.0123099  0.0068245   1.804  0.07151 . 
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.5269 on 1220 degrees of freedom
# Multiple R-squared:  0.2182,	Adjusted R-squared:  0.2124 
# F-statistic: 37.84 on 9 and 1220 DF,  p-value: < 2.2e-16


c(AIC(model2),BIC(model2))
# 1926.256 1982.518

model2<- lm(log(wage)~exper+abileduc+educexper,data=context1)
summary (model2)

# Call:
#   lm(formula = log(wage) ~ exper + abileduc + educexper, data = context1)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -2.26548 -0.30090  0.03545  0.30963  1.81246 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  1.9766863  0.0744288  26.558  < 2e-16 ***
#   exper       -0.0838091  0.0104772  -7.999 2.88e-15 ***
#   abileduc     0.0045335  0.0006595   6.874 9.88e-12 ***
#   educexper    0.0090154  0.0009101   9.906  < 2e-16 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.5282 on 1226 degrees of freedom
# Multiple R-squared:  0.2104,	Adjusted R-squared:  0.2085 
# F-statistic: 108.9 on 3 and 1226 DF,  p-value: < 2.2e-16

BIC(model2)
# [1] 1952.031

######### INTERPRETATIONS ############
# a.  Model1 has predictor variables as abil,exp and educ where as model2 has exper
# abileduc which is an interaction variable of abil and education ,educ exper interaction
# variable of education and experience .On comparision model 2 has slightly high R-squared 
# value compared to model 1 and BIC value of model2 is also less than model1.This indicates
# model2 is better compared to model1. 
#
#
# b.The presence of a interaction variable indicates that the effect of one predictor 
#   variable on the response variable is different at different values of the other predictor 
#   variable. since (educ*exper) is a interaction vairable it captures the effect of educatio
#   for different years of experience and vice versa on the wage.
#

rm(list=ls(all=TRUE))
context2 <-fread("loanapp.csv")
summary(context2)


model3 <- glm(approve~white, family=binomial(link="logit"),data=context2)

summary(model3)

# Call:
#   glm(formula = approve ~ white, family = binomial(link = "logit"), 
#       data = context2)
# 
# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -2.1864   0.4384   0.4384   0.4384   0.8314  
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)   0.8847     0.1253   7.061 1.65e-12 ***
#   white         1.4094     0.1512   9.325  < 2e-16 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 1480.7  on 1988  degrees of freedom
# Residual deviance: 1401.8  on 1987  degrees of freedom
# AIC: 1405.8
# 
# Number of Fisher Scoring iterations: 5

## white heteroskedasticity errors
coeftest(model3)
# z test of coefficients:
#   
#   Estimate Std. Error z value  Pr(>|z|)    
# (Intercept)  0.88469    0.12529  7.0609 1.654e-12 ***
#   white        1.40942    0.15115  9.3246 < 2.2e-16 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

coeftest(model3,vcov.=vcovHC)
# z test of coefficients:
#   
#   Estimate Std. Error z value  Pr(>|z|)    
# (Intercept)  0.88469    0.12570   7.038  1.95e-12 ***
#   white        1.40942    0.15152   9.302 < 2.2e-16 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

model4 <- glm(approve~white+hrat+obrat+loanprc+unem+male+married+dep+sch+cosign+chist+pubrec+mortlat1+mortlat2+vr, family=binomial(link="logit"),data=context2)

summary(model4)

# glm(formula = approve ~ white + hrat + obrat + loanprc + unem + 
#       male + married + dep + sch + cosign + chist + pubrec + mortlat1 + 
#       mortlat2 + vr, family = binomial(link = "logit"), data = context2)
# 
# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -2.9549   0.2545   0.3458   0.4768   2.0827  
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)  3.80171    0.59467   6.393 1.63e-10 ***
#   white        0.93776    0.17290   5.424 5.84e-08 ***
#   hrat         0.01326    0.01288   1.030  0.30313    
# obrat       -0.05303    0.01128  -4.702 2.58e-06 ***
#   loanprc     -1.90495    0.46041  -4.138 3.51e-05 ***
#   unem        -0.06658    0.03281  -2.029  0.04242 *  
#   male        -0.06639    0.20642  -0.322  0.74776    
# married      0.50328    0.17799   2.828  0.00469 ** 
#   dep         -0.09073    0.07333  -1.237  0.21598    
# sch          0.04123    0.17840   0.231  0.81723    
# cosign       0.13206    0.44608   0.296  0.76720    
# chist        1.06658    0.17121   6.230 4.67e-10 ***
#   pubrec      -1.34067    0.21736  -6.168 6.92e-10 ***
#   mortlat1    -0.30988    0.46351  -0.669  0.50378    
# mortlat2    -0.89468    0.56857  -1.574  0.11559    
# vr          -0.34983    0.15372  -2.276  0.02286 *  
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 1476  on 1970  degrees of freedom
# Residual deviance: 1201  on 1955  degrees of freedom
# (18 observations deleted due to missingness)
# AIC: 1233
# 
# Number of Fisher Scoring iterations: 5

coeftest(model4)
coeftest(model4,vocc.=vocvHC)
# z test of coefficients:
#   
#   Estimate Std. Error z value  Pr(>|z|)    
# (Intercept)  3.801710   0.594675  6.3929 1.627e-10 ***
#   white        0.937764   0.172901  5.4237 5.838e-08 ***
#   hrat         0.013263   0.012880  1.0298  0.303125    
# obrat       -0.053034   0.011280 -4.7016 2.581e-06 ***
#   loanprc     -1.904951   0.460407 -4.1375 3.511e-05 ***
#   unem        -0.066579   0.032808 -2.0294  0.042421 *  
#   male        -0.066385   0.206423 -0.3216  0.747758    
# married      0.503282   0.177993  2.8275  0.004691 ** 
#   dep         -0.090734   0.073332 -1.2373  0.215977    
# sch          0.041229   0.178399  0.2311  0.817234    
# cosign       0.132059   0.446080  0.2960  0.767197    
# chist        1.066577   0.171208  6.2297 4.673e-10 ***
#   pubrec      -1.340665   0.217362 -6.1679 6.921e-10 ***
#   mortlat1    -0.309882   0.463510 -0.6686  0.503780    
# mortlat2    -0.894675   0.568570 -1.5736  0.115591    
# vr          -0.349828   0.153721 -2.2757  0.022862 *  
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1


whitobrat <- context2$white*context2$obrat

model5 <- glm(approve~white+hrat+obrat+loanprc+unem+male+married+dep+sch+cosign+chist+pubrec+mortlat1+mortlat2+vr+whitobrat,family=binomial(link="logit"),data=context2)
 summary(model5)
 
 # Call:
 #   glm(formula = approve ~ white + hrat + obrat + loanprc + unem + 
 #         male + married + dep + sch + cosign + chist + pubrec + mortlat1 + 
 #         mortlat2 + vr + whitobrat, family = binomial(link = "logit"), 
 #       data = context2)
 # 
 # Deviance Residuals: 
 #   Min       1Q   Median       3Q      Max  
 # -2.9379   0.2580   0.3472   0.4722   2.1809  
 # 
 # Coefficients:
 #   Estimate Std. Error z value Pr(>|z|)    
 # (Intercept)  4.30653    0.83493   5.158  2.5e-07 ***
 #   white        0.29688    0.75565   0.393 0.694407    
 # hrat         0.01341    0.01295   1.035 0.300625    
 # obrat       -0.06660    0.01935  -3.442 0.000578 ***
 #   loanprc     -1.90970    0.45916  -4.159  3.2e-05 ***
 #   unem        -0.06755    0.03278  -2.061 0.039315 *  
 #   male        -0.07190    0.20704  -0.347 0.728372    
 # married      0.50354    0.17808   2.828 0.004691 ** 
 #   dep         -0.09577    0.07355  -1.302 0.192849    
 # sch          0.03489    0.17894   0.195 0.845390    
 # cosign       0.15257    0.45004   0.339 0.734606    
 # chist        1.06139    0.17160   6.185  6.2e-10 ***
 #   pubrec      -1.34427    0.21793  -6.168  6.9e-10 ***
 #   mortlat1    -0.33331    0.46282  -0.720 0.471415    
 # mortlat2    -0.92086    0.56942  -1.617 0.105838    
 # vr          -0.35086    0.15391  -2.280 0.022631 *  
 #   whitobrat    0.01815    0.02079   0.873 0.382587    
 # ---
 #   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
 # 
 # (Dispersion parameter for binomial family taken to be 1)
 # 
 # Null deviance: 1476.0  on 1970  degrees of freedom
 # Residual deviance: 1200.2  on 1954  degrees of freedom
 # (18 observations deleted due to missingness)
 # AIC: 1234.2
 # 
 # Number of Fisher Scoring iterations: 5
 coeftest(model5)
 coeftest(model5,vcov.=vcovHC)
 # z test of coefficients:
 #   
 #   Estimate Std. Error z value  Pr(>|z|)    
 # (Intercept)  4.306527   0.901355  4.7778 1.772e-06 ***
 #   white        0.296882   0.858772  0.3457 0.7295644    
 # hrat         0.013405   0.014158  0.9468 0.3437269    
 # obrat       -0.066604   0.020728 -3.2133 0.0013121 ** 
 #   loanprc     -1.909701   0.533546 -3.5793 0.0003446 ***
 #   unem        -0.067549   0.035988 -1.8770 0.0605206 .  
 # male        -0.071904   0.210842 -0.3410 0.7330802    
 # married      0.503536   0.187212  2.6897 0.0071526 ** 
 #   dep         -0.095772   0.076030 -1.2597 0.2077873    
 # sch          0.034893   0.180807  0.1930 0.8469704    
 # cosign       0.152567   0.413003  0.3694 0.7118226    
 # chist        1.061385   0.174154  6.0945 1.098e-09 ***
 #   pubrec      -1.344267   0.235075 -5.7185 1.075e-08 ***
 #   mortlat1    -0.333314   0.540401 -0.6168 0.5373739    
 # mortlat2    -0.920857   0.610013 -1.5096 0.1311534    
 # vr          -0.350862   0.157302 -2.2305 0.0257141 *  
 #   whitobrat    0.018149   0.023603  0.7689 0.4419416    
 # ---
 #   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
 
 ########## INTERPRETATIONS #############
#  2a.Coefficient of a logistic regression indicates the log odds .In model 3 
#  B1 indicates the log odds of a white person getting a loan is 1.404
#  
#  2b.After adding 14 variables the B1 is still significant but the log odds have
#  been decreased from 1.404 to 0.93776 compared to model3 to model4.
#  
#  2c.The coefficient of white beta1 has lost its significance and the value has decreased 
#     from 0.83776 to 0.29688
#
#  2d. white*obrat is an interaction variable which accounts for the variablity of loanApproval
#     on the basis of whether the person is white and he has any other obligations.Earlier 
#    model white variable had a coefficient of 0.93776 but it has been decreased to 0.296882
#    in the model5.A person has to be white and should have less obligations to increase his odds
#    of getting a loan.
 
 rm(list=ls(all=TRUE))
 
 context3 <- fread('smoke.csv')
 agesq <- (context3$age)^2
 
 model6 <- glm(cigs~educ+age+agesq+log(income)+restaurn, family=poisson(link="log"),data=context3)

  summary(model6) 
  # 
  # Call:
  #   glm(formula = cigs ~ educ + age + agesq + log(income) + restaurn, 
  #       family = poisson, data = context3)
  # 
  # Deviance Residuals: 
  #   Min      1Q  Median      3Q     Max  
  # -6.338  -4.229  -3.280   2.223  13.942  
  # 
  # Coefficients:
  #   Estimate Std. Error z value Pr(>|z|)    
  # (Intercept) -8.953e-02  1.881e-01  -0.476    0.634    
  # educ        -5.952e-02  4.257e-03 -13.981  < 2e-16 ***
  #   age          1.140e-01  4.968e-03  22.943  < 2e-16 ***
  #   agesq       -1.368e-03  5.696e-05 -24.016  < 2e-16 ***
  #   log(income)  1.047e-01  2.026e-02   5.168 2.36e-07 ***
  #   restaurn    -3.613e-01  3.074e-02 -11.754  < 2e-16 ***
  #   ---
  #   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
  # 
  # (Dispersion parameter for poisson family taken to be 1)
  # 
  # Null deviance: 15821  on 806  degrees of freedom
  # Residual deviance: 14755  on 801  degrees of freedom
  # AIC: 16238
  # 
  # Number of Fisher Scoring iterations: 6
  coeftest(model6,vcov.=vcovHC)
  # z test of coefficients:
  #   
  #   Estimate Std. Error z value  Pr(>|z|)    
  # (Intercept) -0.0895322  0.7819305 -0.1145  0.908840    
  # educ        -0.0595212  0.0194111 -3.0663  0.002167 ** 
  #   age          0.1139858  0.0215662  5.2854 1.254e-07 ***
  #   agesq       -0.0013679  0.0002485 -5.5045 3.701e-08 ***
  #   log(income)  0.1047168  0.0840807  1.2454  0.212973    
  # restaurn    -0.3613089  0.1386491 -2.6059  0.009163 ** 
  #   ---
  #   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
  
########### INTERPRETATIONS#########
# 3.a For every Increase of 1 year of education is associated with a decrease of 0.0595 log odds
#   of smoking cigarattes remaing all other variables being constant.
# 
# 3.b d(ln[cigs])/d(age) = 1*0.1139 + 2*(age)*(-0.0013)
#        at 20:  0.0619
#       at 60: -0.0421
  
  rm(list=ls(all=TRUE))
  context4 <- fread('hdisease.csv')
  context4$exangbin		<-	ifelse(context4$exang=="yes",1,0)
  
  
  model7 <- evtree(hdisease~age+cp+trestbps+thalach+exangbin,data=context4)
  
  model7
    # Model formula:
  #   hdisease ~ age + cp + trestbps + thalach + exangbin
  # 
  # Fitted party:
  #   [1] root
  # |   [2] cp < 4
  # |   |   [3] age < 57: 0.192 (n = 255, err = 75.6)
  # |   |   [4] age >= 57: 0.802 (n = 106, err = 112.8)
  # |   [5] cp >= 4
  # |   |   [6] age < 54: 1.078 (n = 180, err = 172.9)
  # |   |   [7] age >= 54: 1.700 (n = 253, err = 343.2)
  # 
  # Number of inner nodes:    3
  # Number of terminal nodes: 4
  model8 <- ctree(hdisease~age+cp+trestbps+thalach+exangbin,data=context4)
  model8
 # Model formula:
  #   hdisease ~ age + cp + trestbps + thalach + exangbin
  # 
  # Fitted party:
  #   [1] root
  # |   [2] cp <= 3
  # |   |   [3] thalach <= 112: 1.241 (n = 29, err = 45.3)
  # |   |   [4] thalach > 112
  # |   |   |   [5] exangbin <= 0
  # |   |   |   |   [6] age <= 56: 0.110 (n = 219, err = 27.4)
  # |   |   |   |   [7] age > 56: 0.625 (n = 72, err = 56.9)
  # |   |   |   [8] exangbin > 0: 0.707 (n = 41, err = 40.5)
  # |   [9] cp > 3
  # |   |   [10] age <= 53
  # |   |   |   [11] thalach <= 128: 1.333 (n = 78, err = 79.3)
  # |   |   |   [12] thalach > 128: 0.882 (n = 102, err = 84.6)
  # |   |   [13] age > 53
  # |   |   |   [14] exangbin <= 0: 1.409 (n = 93, err = 138.5)
  # |   |   |   [15] exangbin > 0
  # |   |   |   |   [16] age <= 65: 1.768 (n = 138, err = 162.6)
  # |   |   |   |   [17] age > 65: 2.500 (n = 22, err = 19.5)
  # 
  # Number of inner nodes:    8
  # Number of terminal nodes: 9
  plot(model7)
  plot(model8)
  
  
  context5 <- fread('hdisease-new.csv')
  context5$exangbin		<-	ifelse(context5$exang=="yes",1,0) ## or context5$exangbin <- as.factor(context5$exang)
  summary(context5)
  context5$hdisease_pred <- predict(model8,context5,type="response")
##### INTERPRETATIONS############
  # 4a. on comparing model 7 and model 8 plots model8 is overfitting the data and model7
  # is underfitting the data.
  # 4b.dset is a categorical variable which denotes cities, it denotes the location of people falling into
  #    different age groups having different blood pressure, cholesterol, ECG  and heart rate levels, in order
  #    to predict whether a person is going to have heart disease depends upon metrics like  blood pressure, cholesterol
  #     and heart rate level and not specific to location.
  