%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Example 1
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Importance Sampling
% Target density: t(5)
% Importance distribution: t(2)

% Draws from importance
Z = trnd(2, 1000,1);

% Construct weights
W = tpdf(Z,5)./tpdf(Z, 2);

% Calculate means
sum(W.*Z)/sum(W)

% Calculate Pr( t(5) < 2 )
[sum(W.*(Z<2))/sum(W), tcdf(2, 5)]



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Example 2
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Importance Sampling
% Target density: t(5)
% Importance distribution: N(0,1)

% Draws from importance
Z = randn(1000,1);

% Construct weights
W = normpdf(Z)./tpdf(Z, 2);

% Calculate means
sum(W.*Z)/sum(W)

% Calculate Pr( t(5) < 2 )
[sum(W.*(Z<2))/sum(W), tcdf(2, 5)]




