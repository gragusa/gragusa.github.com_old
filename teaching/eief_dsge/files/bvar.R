setbvar <- function(Y, p, h, constant, is.direct = FALSE)
{
  if(missing(h))
    h <- 1
  if(missing(p) | p<=0)
    stop("lag length should be > 0")  
  if(h<=0)
    stop("The forecast horizon 'h' must be positive")
  
  Yraw  <- as.matrix(Y) 
  Traw  <- nrow(Y)
  m     <- ncol(Y)
  if(is.direct) {
    Y1 = Yraw[(h+1-1):Traw,]
    Y2 = Yraw[1:(Traw-h+1),]
    Traw = Traw - h + 1
  } else {
    Y1 <- Y2 <- Yraw
  }
  
  ## Construct Y,X,Z
  out <- bvar_data_work(Y1, Y2, Yraw, m, p, Traw, constant)
  out$Y1 <- Y1
  out$Traw <- Traw
  out$h <- h
  ##------------------------------OLS------------------------------------
  ## First get Ordinary Least Squares (OLS) estimators
  ## ols <- bvar.ols(out$Y,out$X)    
  ols <- NA
  out <- list(ols = ols, data = out, constant = constant)  
  class(out) <- "bvar2"
  out
}

bvar_data_work <- function(Y1, Y2, Yraw, m, p, Traw, constant) {
    Ylag <- vlag(Y2,p)
    if(constant) {
      X1 <- cbind(matrix(1, Traw-p,1), Ylag[(p+1):Traw,])
    } else {
      X1 = Ylag[(p+1):Traw,]
    }
    ## Create the block diagonal matrix Z
    Z <- kronecker(diag(1,m), X1)
    ## Form Y matrix accordingly
    ## Delete first "LAGS" rows to match the dimensions of X matrix
    Y1 <- Y1[(p+1):Traw,]
    ## This is the final Y matrix used for the VAR
    ## Traw - was the dimesnion of the initial data.
    ## T    - is the number of actual 
    ## time series observations of Y and X
    T <- Traw - p
    Y <- Y1
    X <- X1
    ## Return objects of interest
    out <- list(Y = Y, X = X, Z = Z, Yraw=Yraw, p = p, m=m, T=T, k=ncol(X), constant = constant)
    return(out)
  }

vlag <-
  function(X, p) {
    ## This function create
    ## X = [x1 x2 ... xT]
    ## where
    ## x1 = (1 y_{t-1}' y_{t-2}' ... y_{t-p}')
    T <- nrow(X)
    m <- ncol(X)
    XX <- matrix(0, T, m*p)
    for(ii in 1:p) {
      XX[(p+1):T,(m*(ii-1)+1):(m*ii)] <- X[(p+1-ii):(T-ii),]
    }
    XX
  }





BVARBGR <- function(Y, p, lambda, coefsum = FALSE, constant = TRUE, direct = FALSE, h = 1, 
                    predictive, ngibbs = 1000, repfor = 10, h.max = 8) {
    
  out <- setbvar(Y = Y, p = p, h = h, is.direct = direct, constant=constant)
  
  T <- out$data$T
  k <- out$data$k
  m <- out$data$m
  p <- out$data$p
  
  ## Setup containers from predictive
  if(predictive) {
    if(direct){
      Y_predictive <- matrix(0, repfor*ngibbs, m)
    } else {
      Y_predictive <- array(0, c(repfor*ngibbs, m, h.max))
    }    
  } else {
    Y_predictive = NA
  }
    
  ## FIXME: fix the constant...at the moment is always included  
  prior <- priorBGR(out$data, lambda=lambda, constant=constant, delta = 1, coefsum = coefsum)
  
  ## Rearrange data to have consant at the end
  out$data$X <- cbind(out$data$X[,-1], 1)
  ## Posterior Mean Coeffient
  XX <- crossprod(X <- out$data$X)
  XY <- crossprod(out$data$X, out$data$Y)
  Omega <- prior$Omega
  iOmega <- solve(Omega)
  
  Ys <- rbind(out$data$Y, prior$Yd)
  Xs <- rbind(X, prior$Xd)
  
  
  ## Posterior Mean of coefficient
  B.posterior.mean <- solve(crossprod(Xs), crossprod(Xs, Ys))
  b.posterior.mean <- c(B.posterior.mean)
  
  ## Fitted mean value  
  fitted <- out$data$X%*%B.posterior.mean
  residuals <- out$data$Y-fitted  
  YY <- out$data$Y  
  
  
  ## Generate forecast as mean forecast  
  if(direct) {
    X_fore <- c(YY[T,], X[T,1:(m*(p-1))], 1)
    fcast  <- X_fore%*%B.posterior.mean
  } else {
  ## Generate out-of-sample point forecast
  ## 12 period
  fcast <- matrix(0, 12, m)
  X_fores <- matrix(0, 12, k)
  X_fore <- c(YY[T,], X[T,1:(m*(p-1))], 1)
  for(hh in 1:12) {    
    X_fores[hh,] <- X_fore
    fcast[hh, ] <- Y_hat <- X_fore%*%B.posterior.mean
     X_fore <- c(Y_hat, X_fore[1:(m*(p-1))], 1)                      
  }
  colnames(fcast) <- colnames(Y)
  rownames(fcast) <- paste('h=', 1:12, sep='')
  }
  
  ## Draw predictive distribution
  if(predictive) {          
    X_fore <- c(YY[T,], X[T,1:(m*(p-1))], 1)
    
    Td <- m*p+m+1
    alpha <- T+Td+k-2
    ## Initialize Matrix SIGMA
    SSE    <- crossprod(Ys-Xs%*%B.posterior.mean)
    SIGMA  <- SSE/(T+Td-k+2)
    cSIGMA <- chol(SIGMA)
    ## Obtain (X'X)^(-1)
    iXX <- solve(XssX <- crossprod(Xs))
    ## Obtain chol( (X'X)^(-1) )
    ciXX <- chol(iXX)
    
    ## We can use this this since we will need chol(kron(SIGMA, X'X^(-1))) which
    ## is equal to kron(chol(SIGMA), chol(X'X)^(-1))
    
    ## Note to myself if R=chol(x), then R'R = x Thus n draws from N(0, V), where
    ## V is (s x s) can be obtained chol(V)%*%matrix(rnorm(n*s), nrow = n) or 
    ## crossprod(chol(V), matrix(rnorm(n*s), nrow = n))
    
    ## Also, draw from a inverse wishart iW(V, nu) can be done by drawing from a
    ## [W(S^(-1),nu)]^(-1)] In code, tcrossprod(crossprod(chol(solve(S)),
    ## matrix(rnorm(nu*s), nrow = s)))
    
    
    ## draw_predictive = cSIGMA, ciXX, SSE, m, p, k, direct
    
    ##out <- draw_predictive(cSIGMA, ciXX, SSE, repfor, ngibbs, h.max, X_fore, alpha, b.posterior.mean, direct)
    for(sgibbs in 1:ngibbs) {
      ## Draw B
      V_post <- kronecker(cSIGMA, ciXX)
      b.draw <- b.posterior.mean+crossprod(V_post, rnorm(k*m))
      B.draw <- matrix(b.draw, nrow = k, ncol = m)
      ## Draw SIGMA
      SIGMA  <-  solve(tcrossprod( crossprod(chol(solve(SSE)), 
                                             matrix(rnorm(alpha*m), nrow = m))))
      cSIGMA <- chol(SIGMA)        
      mu <- X_fore%*%B.draw
      if(direct) {
        for(rgibbs in 1:repfor) {
          y_temp[rgibbs,] <- mu + rnorm(m)%*%cSIGMA
        }
        Y_predictive[(sgibbs*repfor+1):(1:sgibbs*repfor)] <- y_temp
      } else {
        ## The iterated returns an array Y_predictive of dimension
        ## c((ngibbs*repfor), m, h)
        ## where h is 8 by default, but can be changed setting h.max
        for(rgibbs in 1:repfor) {
          Y_hat <- mu + rnorm(m)%*%cSIGMA ## This is a draw from Y_{T+1}
          Y_predictive[(rgibbs+repfor*(sgibbs-1)),,1] <- Y_hat
          for(hh in 1:(h.max-1)) {
            if(hh<p){
              X_new_temp <- c(Y_hat, X_fore[1:(m*(p-hh))], 1)
              Y_temp <- X_new_temp%*%B.draw + rnorm(m)%*%cSIGMA              
              Y_hat <- c(Y_temp, Y_hat)
            } else {
              X_new_temp <- c(Y_hat[1:(m*p)],1)
              Y_temp <- X_new_temp%*%B.draw + rnorm(m)%*%cSIGMA
              Y_hat <- c(Y_temp, Y_hat)
            }
            Y_predictive[(rgibbs+repfor*(sgibbs-1)),,(hh+1)] <- Y_temp
          }          
        }
      }
    }
  }  
  out <- list(fcast = fcast, fitted = fitted, residuals = residuals, 
              predictive = Y_predictive)
  
  class(out) <- c('bvar', 'bvarBGR')
  
  return(out)
}


priorBGR <- function(vardata, lambda, constant, delta, coefsum) {  
  ##--------------------------------PRIORS------------------------------------ 
  ## Construct prior parameters using dummy observations as in BGR Equation (5)
  ## pag. 75
  
  m <- vardata$m
  n <- m
  T <- vardata$T
  p <- vardata$p
  k <- vardata$k
  Y <- vardata$Y
  Yraw <- vardata$Yraw  
  constant <- vardata$constant
  
  if(missing(delta))
    delta <- 1
  
  if(length(delta)!=1) {
    if(length(delta!=m))
      stop('delta of wriong dimension')
  } else {
    delta <- rep(delta, m)
  }
  
  ## Get residual variances of univariate p-lag autoregressions. Here we just
  ## run the AR(p) model on each equation, ignoring the constant and exogenous
  ## variables (if they have been specified for the original VAR model)
  
  s.i <- apply(Yraw,2,function(uu) ar(uu, aic=FALSE, method='ols', order.max = p)$var.pred)
  
  ## Construct Xd
  Jp <- diag(1:p)
  dsl <- diag(sqrt(s.i))/lambda
  dsl2 <- diag(sqrt(s.i)*delta)/lambda
  epsilon <- 0.001  
  
  Xd.1 <- cbind(kronecker(Jp,dsl), matrix(0, m*p, 1))  
  Xd.2 <- cbind(matrix(0, n, n*p), matrix(0, n, 1))
  Xd.3 <- cbind(matrix(0, 1, n*p), epsilon)
  
  
  
  Xd <- rbind(Xd.1, Xd.2, Xd.3)  
  Yd <- rbind(dsl2, matrix(0, n*(p-1), n),diag(sqrt(s.i)), matrix(0, 1, n))
  
  if(coefsum) {
    Yd.c <- diag(delta*colMeans(vardata$Yraw))/(10*lambda)
    Xd.c <- rbind(kronecker(array(1, p), Yd.c), array(0, m))    
    Xd <- rbind(Xd, t(Xd.c))
    Yd <- rbind(Yd, Yd.c)
  }
  
  Omega0 <- solve(crossprod(Xd))
  B0 <- solve(crossprod(Xd), crossprod(Xd,Yd))
  Psi0 <- crossprod(Yd-Xd%*%B0)
  ## Rearrange for constant on top  
  return(list(B = B0, Psi = Psi0, Omega = Omega0, Xd=Xd, Yd=Yd))
}


  

prior.normwish <- function(vardata, lambda, delta, Sigma) {
  m <- vardata$m
  T <- vardata$T
  p <- vardata$p
  k <- vardata$k
  Y <- vardata$Y
  Yraw <- vardata$Yraw  
  constant <- vardata$constant
  
  if(length(delta)!=1) {
    if(length(delta!=m))
      stop('delta of wriong dimension')
  } else {
    delta <- rep(delta, m)
  }
  ## prior mean of ALPHA (parameter matrix) 
  A.prior <- rbind(matrix(0, 1, m), diag(delta), matrix(0, (p-1)*m,m)) 
  ## <---- prior mean of alpha (parameter vector)
  a.prior <- c(A.prior)
  
  ##------------------------------PRIORS------------------------------------
  ## Get residual variances of univariate p-lag autoregressions. Here
  ## we just run the AR(p) model on each equation, ignoring the constant
  ## and exogenous variables (if they have been specified for the original
  ## VAR model)
  s.i <- apply(Yraw,2,function(uu)
    crossprod(na.omit(ar(uu, aic=FALSE, method='ols',
                         order.max = p, demean = TRUE)$resid))/(T-p))
  
  ## Create an array of dimensions K x M, which will contain the K diagonal
  ## elements of the covariance matrix, in each of the M equations.  
  V.i <- matrix(0, k, m)  
  
  ind <- matrix(0, m,p)
  
  for(i in 1:m) {
    ind[i,] <- seq(constant+i, k, m)
  }  
  
  for(i in 1:m) {     ## for each i-th equation
    for(j in 1:k) {   ## for each j-th RHS variable
      if(constant) {
        if(j == 1) {   ## if there is constant, use this code
          V.i[j,i]  <- epsilon*s.i[i] ## variance on constant
        }  else {
          if(j %in% ind[i,]) {
            V.i[j,i] <- lambda^2/(ceiling((j-1)/m)^2) ## variance on own lags           
          } else {
            for(kj in 1:m) {
              if(j%in%ind[kj,]) {
                ll <- kj                   
              }
            }
            V.i[j,i] <- (lambda2*s.i[i])/((ceiling((j-1)/m)^2)*s.i[ll])           
          }
        }
      }
      else {
        if(j%in%ind[i,]) {
          V.i[j,i]  <- lambda1/(ceiling(j/M)^2)
        } else {
          for(kj in 1:m) {
            if(j%in%ind[kj,]) {
              ll <- kj
            }               
          }
          V.i[j,i] <- (lambda2*s.i[i])/((ceiling(j/m)^2)*s.i[ll])
        }
      }
    }
  }
  
  V.prior  <- diag(c(V.i))
  
  ## NOTE: No prior for SIGMA. SIGMA is simply a diagonal matrix with each
  ## diagonal element equal to s.i[i]. See Kadiyala and Karlsson (1997)
  ## SIGMA  <- diag(s.i)
  
  list(a.prior = a.prior, A.prior = A.prior, V.prior = V.prior, s.i=s.i)
  
}
