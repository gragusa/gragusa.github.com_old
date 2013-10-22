rm(list=ls())
getwd() #show what is the current Working Directory  
setwd("~/Applied Statistics and Econometrics/Dataset") #set a new one
library(foreign) #library needed to import dta data


## 1

smoke=read.dta("smoke.dta") #import the dataset
smoke<-na.omit(smoke)    # if you use this command, you can remove the na.rm=T from next commands)
attach(smoke) #use variables names from the dataset

#(a)
summary(lm(cigs~ white)) 
mean(cigs[white==0], na.rm=T)
mean(cigs[white==1], na.rm=T)


#2(b)
summary(lm(cigs ~ income))$coef
pack=cigs/20
incpound=income*0.63
summary(lm(pack ~ incpound))$coefficients

#(c)
cigsan=cigs*365
summary(lm(cigsan ~ cigpric+white+income+age))$coefficients
summary(lm(cigsan ~ cigpric+white+income+age))

#(d) 
summary(lm(cigsan ~ cigpric+white+income+age+educ))$coefficients
summary(lm(cigsan ~ cigpric+white+income+age+educ))

## 2
rm(list=ls())
affair=read.dta("affairs.dta") #import the dataset
attach(affair) #use variables names from the dataset

summary(lm(affairs ~ gender+age+yearsmarried+children+religiousness+education+rating))$coefficients
