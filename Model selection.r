###############################################################
# Title:        ex2.5.r
# Author:       Jason Parker
# Date:         2017-10-11
# Description:  Use the wage1 data to demonstrate model selection
###############################################################


rm(list=ls(all=TRUE))

## Import packages
library(data.table)
library(sandwich)
library(lmtest)
library(ggplot2)

## Data import and validaticon
context1    <- fread('WAGE1.csv')
# variable name   type    format     label      variable label
# wage            float   %8.2g                 average hourly earnings
# educ            byte    %8.0g                 years of education
# exper           byte    %8.0g                 years potential experience
# tenure          byte    %8.0g                 years with current employer

context1$educsq    <- context1$educ^2
context1$expersq   <- context1$exper^2
context1$tenuresq  <- context1$tenure^2
context1$educcub   <- context1$educ^3
context1$expercub  <- context1$exper^3
context1$tenurecub <- context1$tenure^3

model1 <- lm(log(wage)~educ+exper+tenure,data=context1)
summary(model1) 
BIC(model1)

model2 <- lm(log(wage)~educ+exper+tenure+educsq,data=context1)
model3 <- lm(log(wage)~educ+exper+tenure+expersq,data=context1)
model4 <- lm(log(wage)~educ+exper+tenure+tenuresq,data=context1)
c(BIC(model2),BIC(model3),BIC(model4))

model5 <- lm(log(wage)~educ+exper+tenure+educsq+expersq+tenuresq,data=context1)
BIC(model5)

model6 <- lm(log(wage)~educ+exper+tenure+educsq+expersq+tenuresq+educcub,data=context1)
model7 <- lm(log(wage)~educ+exper+tenure+educsq+expersq+tenuresq+expercub,data=context1)
model8 <- lm(log(wage)~educ+exper+tenure+educsq+expersq+tenuresq+tenurecub,data=context1)
c(BIC(model6),BIC(model7),BIC(model8))

coeftest(model5,.vcov=vcovHC)
BIC(model5)

model11 <- lm(log(wage)~educsq+exper+expersq+tenure,data=context1)
BIC(model11)
coeftest(model11,.vcov=vcovHC)

# mean(context1$educ)
# lm(log(wage)~educ+educsq, data = context1)
# -0.067794+2*0.006439*max(context1$educ)#mean(context1$educ)
# summary(context1$educ)

plot1 <- ggplot(context1, aes(x=educ,y=log(wage))) + geom_point()
plot1 <- plot1 + geom_line(aes(y=predict(lm(log(wage)~educ,data=context1)),color="red"),size=2)
plot1 <- plot1 + geom_line(aes(y=predict(lm(log(wage)~educ+educsq,data=context1)),color="blue"),size=2)
# plot1 <- plot1 + geom_line(aes(y=predict(lm(log(wage)~educ+educsq+educcub,data=context1)),color="green"),size=2)
plot1
plot2 <- ggplot(context1, aes(x=exper,y=log(wage))) + geom_point()
plot2 <- plot2 + geom_line(aes(y=predict(lm(log(wage)~exper,data=context1)),color="red"),size=2)
plot2 <- plot2 + geom_line(aes(y=predict(lm(log(wage)~exper+expersq,data=context1)),color="blue"),size=2)
# plot2 <- plot2 + geom_line(aes(y=predict(lm(log(wage)~exper+expersq+expercub,data=context1)),color="green"),size=2)
plot2 
plot3 <- ggplot(context1, aes(x=tenure,y=log(wage))) + geom_point()
plot3 <- plot3 + geom_line(aes(y=predict(lm(log(wage)~tenure,data=context1)),color="red"),size=2)
plot3 <- plot3 + geom_line(aes(y=predict(lm(log(wage)~tenure+tenuresq,data=context1)),color="blue"),size=2)
# plot3 <- plot3 + geom_line(aes(y=predict(lm(log(wage)~tenure+tenuresq+tenurecub,data=context1)),color="green"),size=2)
plot3 
