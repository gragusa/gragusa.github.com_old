rm(list=ls())
library(sem) #this library is needed to perform the TSLS
library(foreign)
library(aod)
library(ggplot2)

getwd()
setwd("/Users/federicaromei/Dropbox/ase/New_Practice_2013_R/Practice _09._Probit_Logit/Dataset")
source("rob.R")

## 1
op<-read.dta("Openness.dta")
attach(op)

#(a) / 
summary_rob(lm(inf~open + oil))
#(b) / 
lland<-log(land)
summary_rob(lm(open~oil + lland))
summary(tsls(inf~open+oil,~oil+lland,data=op))
## 2
ba<-read.dta("Smoking.dta")
attach(ba)

#a 
summary(lm(smoker~1))
summary_rob(lm(smoker~smkban))

# c
age2<-age^2
summary_rob(lm(smoker~smkban + age + age2+hsdrop+hsgrad+colsome+colgrad+female+black+hispanic))
# c
myprob<- glm(smoker~smkban,family=binomial(link="probit"),data=ba)
summary(myprob)

myprob<- glm(smoker~smkban + age + age2+hsdrop+hsgrad+colsome+colgrad+black+hispanic,family=binomial(link="probit"),data=ba)
summary(myprob)
# f
mylog<- glm(smoker~smkban,family="binomial",data=ba)
summary(mylog)

mylog<- glm(smoker~smkban + age + age2+hsdrop+hsgrad+colsome+colgrad+black+hispanic,family="binomial",data=ba)
summary(mylog)
