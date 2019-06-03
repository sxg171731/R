########################################################################################################

## Purpose:  Demonstrate linear regression using R and the BWGHT.csv data set
########################################################################################################


## Drop everything
rm(list=ls(all=TRUE))

## Use the data.table library for fast importing and manipulation
#install.packages('data.table')
library(data.table)

## Read the BWGHT.csv example into the variable context
context <- fread("BWGHT.csv")
# context <- read.csv("BWGHT.csv")


## Data description from BWGHT_labels.txt
# variable name   type    format     label      variable label
# ---------------------------------------------------------------------------------------------------
# faminc          float   %9.0g                 1988 family income, $1000s
# cigtax          float   %9.0g                 cig. tax in home state, 1988
# cigprice        float   %9.0g                 cig. price in home state, 1988
# bwght           int     %8.0g                 birth weight, ounces
# fatheduc        byte    %8.0g                 father's yrs of educ
# motheduc        byte    %8.0g                 mother's yrs of educ
# parity          byte    %8.0g                 birth order of child
# male            byte    %8.0g                 =1 if male child
# white           byte    %8.0g                 =1 if white
# cigs            byte    %8.0g                 cigs smked per day while preg
# ---------------------------------------------------------------------------------------------------

## Get summary statistics for all variables
summary(context)

## Plot
plot(context$cigs,context$bwght)

## Ordinary least-squares
mdlols       <- lm(bwght~cigs, data=context)
summary(mdlols)

# bwght = 119.8 + -0.514 cigs

# Every 1 cigarette smoked by the mother per day during pregancy is associated with a 0.5 decrease in oz of 
#   birthweight.
# Every pack per day (20 cigs) is associated with a 10 oz decrease in child size at birth.
# Every pack per day is associated with a 0.64 lb decrease in birthweight.
#install.packages('ggplot2')
library(ggplot2)
ggplot(context, aes(x=cigs,y=bwght))
ggplot(context, aes(x=cigs,y=bwght)) + geom_point()
ggplot(context, aes(x=cigs,y=bwght)) + geom_point() + geom_line(aes(y=predict(mdlols),color="red"),size=2)
ggplot(context, aes(x=cigs,y=bwght)) + geom_point() + geom_line(aes(y=predict(mdlols),color="red"),size=2) + theme(legend.position="none") 
ggplot(context, aes(x=cigs,y=bwght)) + geom_point() + geom_line(aes(y=predict(mdlols),color="red"),size=2) + theme(legend.position="none")  + scale_x_continuous(name="Cigarettes smoked during pregnancy per day") + scale_y_continuous(name="Birthweight (oz)")

## Multiple least-squares using lm
mdl       <- lm(bwght~cigs+faminc, data=context)
summary(mdl)


# Every 1 cigarette smoked by the mother per day during pregancy is associated with a 0.46 decrease in oz of 
#   birthweight controlling for family income and child sex.


mdl       <- lm(bwght~cigs+faminc+male+motheduc+fatheduc, data=context)
summary(mdl) # This model drops data (fatheduc is missing for some families)
mdl       <- lm(bwght~cigs+faminc+male+motheduc+fatheduc+white, data=context)
summary(mdl) 


## BWGHT log transform                                                                
lbwght    <- log(context$bwght)
hist(context$bwght)
hist(lbwght)
hist(context$faminc)
mdl       <- lm(lbwght~cigs+faminc+male+motheduc+fatheduc+white, data=context)
summary(mdl)
coef(mdl)*100  #When we interpret coefficients from log models, we have to multiply by 100
# Every 1 cigarette smoked by the mother per day during pregancy is associated with a 0.515% decrease in
#   birthweight controlling for family income and child sex.
mean(context$bwght)
118.6996*-0.515/100

## Quadratic regression
cig2      <- context$cigs^2
mdlc2     <- lm(bwght~cigs+cig2, data=context)
summary(mdlc2)
ggplot(context, aes(x=cigs,y=bwght)) + geom_point() + geom_line(aes(y=predict(mdlc2),color="red"),size=2) + theme(legend.position="none")  + scale_x_continuous(name="Cigarettes smoked during pregnancy per day") + scale_y_continuous(name="Birthweight (oz)")



## Subsetting context
smokectxt <- context[cigs > 0]
summary(smokectxt)
diag(sqrt(var(smokectxt)))
mdl       <- lm(log(bwght)~cigs+male+white, data=smokectxt)
summary(mdl)

## Binary variable
context$smokes    <- as.numeric(context$cigs>0)
mdl       <- lm(log(bwght)~smokes+log(faminc)+male+white, data=context)
summary(mdl)

mean(context$bwght)*-0.0704015


rm(smokes)
rm(lbwght)

p <- ggplot(context,aes(x=smokes,y=bwght))
p + geom_boxplot(aes(group=context$smokes))

nonctxt <- context[cigs==0]
summary(nonctxt)
nonctxt <- nonctxt[!is.na(motheduc)]
diag(sqrt(var(nonctxt)))
nonctxt2 <- nonctxt[!is.na(fatheduc)]
sqrt(var(nonctxt2$fatheduc))

smokectxt <- context[cigs > 0]
summary(smokectxt)
diag(sqrt(var(smokectxt)))
smokectxt2 <- smokectxt[!is.na(fatheduc)]
sqrt(var(smokectxt2$fatheduc))

mdl       <- lm((log(bwght)+0.071733977*smokes)~log(faminc)+male+white, data=context)
mdl2      <- lm(log(bwght)~cigs+log(faminc)+male+white, data=context)
AIC(mdl)
AIC(mdl2)

##############################################
## Measurement error in bwght
##############################################

for (ns in 1:5) {
  if (ns == 1) {n <- 100}
  if (ns == 2) {n <- 250}
  if (ns == 3) {n <- 500}
  if (ns == 4) {n <- 1000}
  if (ns == 5) {n <- 2500}
  coefs           <- rep(0,simn)
  for (isim in 1:simn) {
    # create random data
    smokes        <- as.numeric(runif(n)>0.9)
    cigs          <- smokes*runif(n,min=0,max=9)^2
    bwght         <- cigs*-0.38+rnorm(n,mean=120,sd=20)
    cigs_star     <- cigs*runif(n,min=0.5,max=1)
    
    # estimate the ols coefficients using the measured (with error) x and y
    bhat          <- coef(lm(bwght~cigs_star))
    coefs[isim]   <- bhat[2]
  }
  plot(density(coefs),xlim=c(-1,0))
  if(ns!=5)invisible(readline(prompt="Press [enter] to continue"))
}
plot(density(runif(100000,min=0,max=9)^2))