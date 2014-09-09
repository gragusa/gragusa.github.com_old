function [beta_est t_first]=beta_estimation(N,M,A,gamma,beta)
% Initialize the matrix
beta_est=NaN(2,M);
t_first=NaN(M,1);

for m=1:M;
    %% GENERATE THE ERRORS 
    % It is important that the errors of x and y are correlated
    u=normrnd(0,1,N,1);% We set the error as normal mean 0 and variance 1
    eps=normrnd(0,1,N,1);% We set the error as normal mean 0 and variance 1
    %% Generate the Instruments
    z=normrnd(3,1,N,1); % Instrument
    
    %% Correlation among the errors
    errors=[u eps]*chol(A); % We correlate the errors
    
    %% Vector of Instruments
    z_in=[ones(N,1) z]; % We build the matrix of instruments
    
    %% Gen the x
    x_g=z_in*gamma'+errors(:,2);% We build x
    
    
    
    x=[ones(N,1) x_g]; % We build the matrix of variables
   
    %% Gen the y
    y=x*beta'+errors(:,1); % We build y
    
    %% From here we are HUMAN!
    %% Estimation of gamma_hat and his Variance
    gamma_hat = (z_in'*z_in)\z_in'*x_g;
    
    er_hat=  x_g - z_in*gamma_hat;
    
    ZE=z_in.*(er_hat *ones(1,size(z_in,2)));
    
    Sigma_het (:,:)=(z_in'*z_in)\(ZE'*ZE)/(z_in'*z_in);
    
    %% Do the F test 
    t_first(m,1)= (gamma_hat(2)/sqrt(Sigma_het(2,2))).^2;
    
    %% Estimate Beta_iv
    beta_iv=(z_in'*x)\z_in'* y; % We find the betas
    
    
    
    
    beta_est(:,m)= beta_iv; % we store
    
    
    
end