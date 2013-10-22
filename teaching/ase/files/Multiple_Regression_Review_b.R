rm(list=ls())
getwd() #show what is the current Working Directory  
setwd("/Users/federicaromei/Dropbox/ase/New_Practice_2013_R/Practice_04_Multiple Regression Review/Dataset") #set a new one
library(foreign) #library needed to import dta data
source("rob.R")

## 1

smoke=read.dta("smoke.dta") #import the dataset
smoke<-na.omit(smoke)    # if you use this command, you can remove the na.rm=T from next commands)
attach(smoke) #use variables names from the dataset

#(a)
#summary(lm(cigs~ white)) 
rob(lm(cigs~ white),0.01)
mean(cigs[white==0], na.rm=T)
mean(cigs[white==1], na.rm=T)


#2(b)
#summary(lm(cigs ~ income))$coef
rob(lm(cigs ~ income))
pack=cigs/20
incpound=income*0.63
rob(lm(pack ~ incpound))

#(c)
cigsan=cigs*365
rob(lm(cigsan ~ cigpric+white+income+age))
rob(lm(cigsan ~ cigpric+white+income+age))

#(d) 
rob(lm(cigsan ~ cigpric+white+income+age+educ))
rob(lm(cigsan ~ cigpric+white+income+age+educ))

## 2
rm(list=ls())
affair=read.dta("affairs.dta") #import the dataset
attach(affair) #use variables names from the dataset

summary(lm(affairs ~ gender+age+yearsmarried+children+religiousness+education+rating))$coefficients
