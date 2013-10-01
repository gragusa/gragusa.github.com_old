rm(list=ls())
getwd() #show what is the current Working Directory  
setwd("/Users/federicaromei/Dropbox/ase/dataset/dta") #set a new one 
library(foreign) #library needed to import .dta data

## 1

sleep75=read.dta("sleep75.dta") #import the dataset
attach(sleep75) #use variables names from the dataset

# (a)
mean(sleep[clerical==1])
mean(sleep[clerical==0])
sd(sleep[clerical==1])/sqrt(sum(clerical==1)) # "sum(clerical==1)" counts the number of observations for clerical
sd(sleep[clerical==0])/sqrt(sum(clerical==0))

diff=mean(sleep[clerical==1])-mean(sleep[clerical==0]) # differece between the two averages 
sdcl=sd(sleep[clerical==1])/sqrt(sum(clerical==1))     # standard error for clericals
sdnocl=sd(sleep[clerical==0])/sqrt(sum(clerical==0))   # standard error for non-clericals
test=diff/sqrt(sdcl^2+sdnocl^2)                        # t-statistic
test
#or, simply ...
t.test (sleep[clerical==1], sleep[clerical==0])$statistic
t.test (sleep[clerical==1], sleep[clerical==0])

#(b) this is similar to the previous point:
mean(sleep[male==1])
sd(sleep[male==1])/sqrt(sum(male==1))
mean(sleep[male==0])
sd(sleep[male==0])/sqrt(sum(male==0))

diff=mean(sleep[male==1])-mean(sleep[male==0])
sdmale=sd(sleep[male==1])/sqrt(sum(male==1))
sdfem=sd(sleep[male==0])/sqrt(sum(male==0))
test=diff/sqrt(sdmale^2+sdfem^2)

# or simply...
t.test (sleep[male==1], sleep[male==0])$statistic
t.test (sleep[male==1], sleep[male==0])

## 2
ci<-function(x) #this function  provides coefficients, t-statistics, p values and confidence intervalsfor a given regression
{               # x<-lm(y~x1+x2...)
  x1<-summary(x)$coef
  cbind(x1,confint(x))
}

summary(lm(sleep~totwrk))      # complete outcome for the regression
ci(lm(sleep~totwrk))            # results with confidence intervals

## 3

rm(list=ls())
databwght=read.dta("BirthWeight.dta") 
attach(databwght) 

#(a)
ci<-function(x) #this function  provides coefficients, t-statistics, p values and confidence intervalsfor a given regression
{               # x<-lm(y~x1+x2...)
  x1<-summary(x)$coef
  cbind(x1,confint(x))
}
ci(lm(bw~cigs))
summary(lm(bw~cigs))$r.squared # only the r.squared is displayed 

#(b)
packs=cigs/20
summary(lm(bw~packs))
summary(lm(bw~packs))$r.squared

#(c)
wgrams=0.03*bw
summary(lm(wgrams~packs))$coef
summary(lm(wgrams~packs))$r.squared