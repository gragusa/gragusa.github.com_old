rm(list=ls())
getwd() #show what is the current Working Directory  
setwd("/Users/federicaromei/Dropbox/ase/New_Practice_2013_R/Practice_06_ Logarithms_and_Interactions/Dataset") #set a new one
library(foreign) #library needed to import dta data
source("rob.R")

## 1

ceosal=read.dta("ceosal1.dta")
attach(ceosal)

#(a)
logsalary=log(salary)
summary_rob(lm(logsalary~sales+roe+finance+consprod+utility))
#(b)
logroe=log(roe)
summary_rob(lm(logsalary~sales+logroe+finance+consprod+utility))

## 2

rm(list=ls())
equipment=read.dta("equipment.dta")
attach(equipment)
source("rob.R")

#(a)
logvalue=log(valueadded)
logcapital=log(capital)
loglabor=log(labor)
rob(lm(logvalue~logcapital+loglabor)) 

#(b)
xnew=loglabor-logcapital
ynew=logvalue-logcapital
summary_rob(lm(ynew~logcapital+xnew))


## 3

rm(list=ls())
wage1=read.dta("wage1.dta")
attach(wage1)

#(a)
logwage<-log(wage)
summary_rob(lm(logwage~educ+female+educ:female)) #the interaction is computed directly in the regression
#or create first a new variable to insert in the regression
eduxfem=educ*female
summary(lm(logwage~educ+female+eduxfem))$coef

#(b) 
res<-lm(logwage~educ+female+eduxfem)
res1<-summary(lm(logwage~educ+female+eduxfem))$coef
cbind(res1,confint(res))  #show all results together