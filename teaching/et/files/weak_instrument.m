clear all
clc;
%% Exogenous parameters
gamma=[1 .9];% Coefficients of IV

beta=[1 3];% Coefficients of OLS
M=1000;% # of Montecarlo simulations
N=500; % # of observations
A=[1 0.3;0.3 1]; % Correlations among u and epsilon
    
[beta_est t_first]=beta_estimation(N,M,A,gamma,beta);

figure(1)
subplot(2,1,1)
histfit(beta_est(2,:)); 	
title( '\beta_1' ); 
ylabel( 'frequency' ); 
subplot(2,1,2)
hist(t_first); 	
title( 't on \gamma' ); 
ylabel( 'frequency' ); 
    