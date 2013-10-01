# COPY and PASTE
ci<-function(x) #this function  provides coefficients, t-statistics, p values and confidence intervalsfor a given regression
{               # x<-lm(y~x1+x2...)
  x1<-summary(x)$coef
  cbind(x1,confint(x))
}
