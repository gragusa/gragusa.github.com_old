% Function to calculate the density of the inverse gamma distribution
invgampdf = @(x,a,b) b^a/gamma(a).*(1./x).^(a+1).*exp(-b./x);


% Importance sampling for robust linear model
S = 150000; % Number of draws

% Generate "fake" linear model data
rng(1245); % set sedd to replicate
T = 200;
k = 1;
nu = 6;
y = trnd(nu, T,1);
X = [ones(T,1), randn(T, k)];

% OLS estimator
beta_ols = X\y; % equivalent to (X'X)^-1 X'y
s_ols    = ((y-X*beta_ols)'*(y-X*beta_ols)/(T-k))^(1/2);
XX = X'*X;
A  = s_ols^2*XX^(-1);


% draw beta
beta_s   = repmat(beta_ols', S,1)+mvtrnd(A, nu-1, S); 
% draw gamma If X~Gama(a, b^(-1)), 1/X \sim Inv-Gamma(a, b)
sigma2_s = 1./gamrnd((T-k)/2, (s_ols^2*(T-k)/2)^(-1), S, 1);

gamm = zeros(S,1); % Calculate log-kernel
for s=1:S
    % Calculate 1+(Y-X*beta)/nu*sigma^2
    r = y-X*beta_s(s,:)';
    f = nu*sigma2_s(s,:);
    tmp = log(1+r.^2/f);
    gamm(s,1) = exp(-0.5*(T+1)*log(sigma2_s(s)) -((nu+1)/2)*sum(tmp));     
end

g = zeros(S,1);
for s=1:S
    g(s,1)=mvtpdf(beta_s(s,:)'-beta_ols, A, nu-1)*invgampdf(sigma2_s(s,1),...  
                (T-k)/2, s_ols^2*(T-k)/2);
end


% Contruct weights

w = (gamm./g);
w = w/sum(w);

% calculate posterior mean
pmean = zeros((k+1),1);
parea = zeros((k+1),1);
pares = zeros((k+1),1);
beta_olss = mvnrnd(beta_ols, A, S);
for u=1:(k+1)
    pmean(u,1) = sum(w(:,1).*beta_s(:,u));
    parea(u,1) = sum(w(:,1).*(beta_s(:,u)>0));
    pares(u,1) = mean(beta_olss(:,u)>0);
end

disp('----------------------------------------')
disp(' Importance Sampling')
disp('----------------------------------------')
disp('Number of draws:')
disp(S);
disp('Degress of freedom:')
disp(nu);
disp('----------------------------------------')
disp('Posterior mean:')
disp(pmean)
disp('OLS:')
disp(beta_ols)
disp('----------------------------------------')
disp('Posterior: Pr(beta>0)')
disp(parea)
disp('OLS Asymptotics: Pr(beta>0)')
disp(pares)






        



