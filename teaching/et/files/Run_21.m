clear
clc
load('Guns.mat')
N=51;
T=23;
%% First Question
y= log(violent);
x=[ones(size(violent,1),1) shall];
[beta_ols,t]=OLS_het(y,x);
display('First Question')
display('beta')
display(beta_ols)
display('t')
display(t)




%% Second Question
x=[ones(size(violent,1),1) shall male cauc afam income population density prisoners];


[beta_ols,t]=OLS_het(y,x);

display('Second Question')
display('beta')
display(beta_ols)
display('t')
display(t)
%% Third Question

D_n=kron(eye(N),ones(T,1));
X=[x(:,2:end) D_n];

[beta_ols,t]=OLS_het(y,X);

display('Third Question')
display('beta')
display(beta_ols)
display('t')
display(t)

%%  Beta_fe

M_D=eye(N*T)-D_n*inv(D_n'*D_n)*D_n';
x_new= M_D*x(:,2:end);
y_new=M_D*y;
[beta_fe,t]=OLS_het(y_new,x_new);

display('Fourth Question')
display('beta_fe')
display(beta_fe)
display('t')
display(t)

