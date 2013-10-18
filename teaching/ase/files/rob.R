require(sandwich)
require(lmtest)



summary_rob <- function(object, alpha = 0.05) {
  if(class(object)!="lm")
    stop("'summary_rob' only works on object of class 'lm'")
  b <- coeftest(object, vcov = vcovHC) 
  sobj <- summary(object)
  sobj$coefficients <- b
  f <- waldtest(object, vcov=vcovHC, test = 'Chisq')
  sobj$fstatistic <- c(value=f$Chisq[2], numdf = abs(f$Df[2]), dendf = Inf)
  sobj
}



rob <- function(object, alpha = 0.05) {
  b <- coeftest(object, vcov = vcovHC)
  k <- nrow(b)
  cii <- matrix(0, k, 2)
  for (i  in  1:k) {
    cii[i,]<-  c(b[i,1] + qnorm(alpha/2)*b[i,2] , b[i,1] - qnorm(alpha/2)*b[i,2])
  }
  colnames(cii) <- c(paste("",alpha/2*100 ,"%"),
                     paste("",100 - alpha/2*100 ,"%"))
  cbind(b,cii)
}


confint.rob <- function (object, parm, level = 0.95) {
  cf <- coef(object)
  pnames <- names(cf)
  if (missing(parm)) 
    parm <- pnames
  else if (is.numeric(parm)) 
    parm <- pnames[parm]
  a <- (1 - level)/2
  a <- c(a, 1 - a)
  fac <- qnorm(a)
  pct <- paste(format(100 * a, trim = TRUE, scientific = FALSE, digits = 3), "%")
  ci <- array(NA, dim = c(length(parm), 2L), dimnames = list(parm, 
                                                             pct))
  ses <- sqrt(diag(vcovHC(object)))[parm]
  ci[] <- cf[parm] + ses %o% fac
  ci
}
