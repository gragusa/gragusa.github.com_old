function [chain] = metropolis(sim, y, X, nu)
% Metropolis algorithm
[T k] = size(X);
chain = zeros(sim, k+1);
% Set theta1 ==> OLS
beta_ols     = X\y; % equivalent to (X'X)^-1 X'y
s_ols        = ((y-X*beta_ols)'*(y-X*beta_ols)/(T-k))^(1/2);
theta        = [beta_ols', log(s_ols^2)];
% Obtain Sigma
f = @(theta)-roblinear(theta, y, X, nu);
[theta_s,FVAL,EXITFLAG,OUTPUT,GRAD,HESSIAN] = fminunc(f, theta);
Sigma        = inv(HESSIAN);
gamma_s      = roblinear(theta_s, y, X, nu);
for s=1:sim
    % Draw candidate    
    theta_star = mvnrnd(theta_s', Sigma);
    % Construct gamma(theta*)
    gamma_star = roblinear(theta_star, y, X, nu);
    % Construct r
    r = min(exp(gamma_star-gamma_s),1);
    % Draw U
    U = rand(1);
    if(U<=r) 
        theta_s = theta_star;
    end
    chain(s,:) = theta_s;
end