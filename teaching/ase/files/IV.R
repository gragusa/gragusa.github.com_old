rm(list=ls())
library(sem) #this library is needed to perform the TSLS
library(foreign)
getwd()
setwd("/Users/federicaromei/Dropbox/ase/New_Practice_2013_R/Practice_08_IV/Dataset")
source("rob.R")

## 1
birthw<-read.dta("Birthweight.dta")
attach(birthw)

#(a) / 

#(b) /

#(c)
lbw<-log(bw)
ly<-log(y)
summary_rob(lm(lbw~cig+male+order+ly))
summary(tsls(lbw~cig+male+order+ly, ~cigprice+male+order+ly,data=birthw))

#(d)
summary_rob(lm(cig~cigprice+male+order+ly))

## 2
fertil<-read.dta("Fertil2_.dta")
attach(fertil)

#a
age2=age^2
summary_rob(lm(children~educ+age+age2))

#b
summary_rob(lm(educ~frsthalf+age+age2))

#c
summary_rob(tsls(children~educ+age+age2,~frsthalf+age+age2))

#d
summary_rob(lm(children~educ+age+age2+electric+tv+bicycle))
summary(tsls(children~educ+age+age2++electric+tv+bicycle,~frsthalf+age+age2+electric+tv+bicycle))
summary_rob(lm(educ~frsthalf+age+age2+electric+tv+bicycle))
## 3

fertility<-read.dta("Fertility_.dta")
attach(fertility)

#a
summary_rob(lm(weeks~morekids))

#b /

#c
summary_rob(lm(morekids~samesex))

#d /

#e
summary(tsls(weeks~morekids, ~samesex))
summary(tsls(weeks~morekids+age+black+hispan+othrace, ~samesex+age+black+hispan+othrace))