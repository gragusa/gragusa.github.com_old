function [likelihood] = roblinear(theta,y, X, nu) 
% Calculate the likelihood of the robust linear model
% Calculate log-kernel
[T k] = size(X);
beta   = theta(1:k)';
sigma2 = exp(theta(k+1));
r = y-X*beta;
f = nu*sigma2;
tmp = log(1+r.^2/f);
likelihood = -0.5*(T+1)*log(sigma2) -((nu+1)/2)*sum(tmp);     


    


