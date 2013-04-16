S = 50000; % Number of draws

% Generate "fake" linear model data
rng(1245); % set sedd to replicate
T = 200;
k = 2;
nu = 1;
y = trnd(nu, T,1);
X = [ones(T,1), randn(T, (k-1))];

% Call metropolis algorithm
chain = metropolis(S, y, X, nu);

csvwrite("../data_roblinea.csv", [y,X])
