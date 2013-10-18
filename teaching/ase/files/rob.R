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
  class(sobj) <- "summary_rob"
  sobj
}


print.summary_rob <-
  function (x, digits = max(3L, getOption("digits") - 3L), symbolic.cor = x$symbolic.cor, 
    signif.stars = FALSE, ...) 
{
    cat("\nCall:\n", paste(deparse(x$call), sep = "\n", collapse = "\n"), 
        "\n\n", sep = "")
    resid <- x$residuals
    df <- x$df
    rdf <- df[2L]
  
    if (length(x$aliased) == 0L) {
        cat("\nNo Coefficients\n")
    }
    else {
        if (nsingular <- df[3L] - df[1L]) 
            cat("\nCoefficients: (", nsingular, " not defined because of singularities)\n", 
                sep = "")
        else cat("\nCoefficients:\n")
        coefs <- x$coefficients
        if (!is.null(aliased <- x$aliased) && any(aliased)) {
            cn <- names(aliased)
            coefs <- matrix(NA, length(aliased), 4, dimnames = list(cn, 
                colnames(coefs)))
            coefs[!aliased, ] <- x$coefficients
        }
        printCoefmat(coefs, digits = digits, signif.stars = signif.stars, 
            na.print = "NA", ...)
    }
    cat("\nResidual standard error:", format(signif(x$sigma, 
        digits)), "on", rdf, "degrees of freedom")
    cat("\n")
    if (nzchar(mess <- naprint(x$na.action))) 
        cat("  (", mess, ")\n", sep = "")
    if (!is.null(x$fstatistic)) {
        cat("Multiple R-squared: ", formatC(x$r.squared, digits = digits))
        cat(",\tAdjusted R-squared: ", formatC(x$adj.r.squared, 
            digits = digits), "\nF-statistic:", formatC(x$fstatistic[1L], 
            digits = digits), "on", x$fstatistic[2L], "and", 
            x$fstatistic[3L], "DF,  p-value:", format.pval(pf(x$fstatistic[1L], 
                x$fstatistic[2L], x$fstatistic[3L], lower.tail = FALSE), 
                digits = digits))
        cat("\n")
    }
    correl <- x$correlation
    if (!is.null(correl)) {
        p <- NCOL(correl)
        if (p > 1L) {
            cat("\nCorrelation of Coefficients:\n")
            if (is.logical(symbolic.cor) && symbolic.cor) {
                print(symnum(correl, abbr.colnames = NULL))
            }
            else {
                correl <- format(round(correl, 2), nsmall = 2, 
                  digits = digits)
                correl[!lower.tri(correl)] <- ""
                print(correl[-1, -p, drop = FALSE], quote = FALSE)
            }
        }
    }
    
    cat("---\nHeteroskadasticity robust standard errors\n")
    cat("\n")
    invisible(x)
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
