rm(list=ls()) # Rimuovi tutto (clear-clc in matlab)
# Upload library

library(foreign)
library(lmtest)
library(sandwich)
library(car)
setwd("/Users/federicaromei/Dropbox/ase/New_Practice_2013_R/Practice_05_Wald_and_Logarithm/Dataset")
source("rob.R")

## 1

hprice=read.dta("hprice.dta") #import the dataset
hprice<-na.omit(hprice)    # if you use this command, you can remove the na.rm=T from next commands)
attach(hprice) #use variables names from the dataset

R_u=summary(lm(price~ assess + sqrft +bdrms + colonial))$r.squared

R_r=summary(lm(price~colonial))$r.squared

a= (R_u - R_r)/3
b= (1 - R_u)/(length(price) - 4 -1)
Ftest=2*a/b
Ftest

## Assume heteroschedasticity
m1=lm(price~ assess + sqrft +bdrms + colonial)
#m2=lm(price~  colonial)
#waldtest(m1, m2,vcov = vcovHC,test = "Chisq")



wtest(m1, testcoef=c("assess", "sqrft","bdrms"),vcov=vcov)
#HETERO
linearHypothesis(m1,hm,rhs,vcov=vcovHC)

# Second Part
CEOSAL1=read.dta("CEOSAL1.dta") #import the dataset
CEOSAL1<-na.omit(CEOSAL1)    # if you use this command, you can remove the na.rm=T from next commands)
attach(CEOSAL1) #use variables names from the dataset

lsale=log(sales)
lsalary=log(salary)
rob(lm(lsalary~ sales ))
rob(lm(salary~ lsales ))
rob(lm(lsalary~ lsales ))

