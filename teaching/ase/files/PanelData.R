rm(list=ls())
library(foreign)
getwd()
setwd("~/Applied Statistics and Econometrics/Dataset")
source("rob.R")
## 1

guns<-read.dta("guns.dta")
attach(guns)

#(a)
lviolent<-log(violent)
summary_rob(lm(lviolent~shall))
summary_rob(lm(lviolent~shall+prisoners+density+income+population+afam+cauc+male))

#(b) /

#(c)
summary_rob(lm(lviolent~shall+prisoners+density+income+population+afam+cauc+male+factor(state)))

#(d)
summary_rob(lm(lviolent~shall+prisoners+density+income+population+afam+cauc+male+factor(state)+factor(year)))

# (e)
lrobbery<-log(robbery)
lmurder<- log(murder)
summary_rob(lm(robbery~shall+prisoners+density+income+population+afam+cauc+male+factor(state)+factor(year)))
summary_rob(lm(lmurder~shall+prisoners+density+income+population+afam+cauc+male+factor(state)+factor(year)))

# (f) /
# (g) /

## 2

wage2<-read.dta("wage2.dta")
attach(wage2)

educ2<-educ^2
summary_rob(lm(lwage~IQ+educ2+black+tenure+feduc+tenure:exper)) # (my regression, run your own)
