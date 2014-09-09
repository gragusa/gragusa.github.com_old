clear; 
clc;
%% First Practice

N=1000; % Number of sample draw
M=3000; % Number of time we do the loop
mu=0; % Mean of the Normal
sigma=1; % Variance of the Normal
    



    Matrix1=NaN(M,1); 
    Matrix2=NaN(M,1);
    Matrix3=NaN(M,1);
    Matrix4=NaN(M,1);
    Matrix5=NaN(M,1);
%% We write the random draws
%s= randraw('cauchy', [], [1 1e5]);
for m=1:M
y1=normrnd(mu,sigma,N,1); % Draw from the Normal
y2 = randraw('chisq', [2], [1 N]);% Draw from a chi-square parameter 2
y3=randraw('cauchy', [], [1 N]); % Draw from a Cauchy
y4 = randraw('pareto', [1, 1.5], [1 N]); % Draw from a Pareto parameters 1 and 1.5
y5 = randraw('binom', [10 0.5], [1 N]); % Draw from a Binomial parameters 10 and 0,5
    


% We store the sample average
    Matrix1(m,1)=mean(y1); 
    Matrix2(m,1)=mean(y2);
    Matrix3(m,1)=mean(y3);
    Matrix4(m,1)=mean(y4);
    Matrix5(m,1)=mean(y5);
end

%% Plot
% we plot the histogram of the sample average;

 figure(1)
subplot(3,2,1)
histfit(Matrix1); 	
xlabel( 'x' ); 
ylabel( 'frequency' ); 
title('Normal')
subplot(3,2,2)
histfit(Matrix2); 	
xlabel( 'x' ); 
ylabel( 'frequency' ); 
title('chi')
subplot(3,2,3)
histfit(Matrix3); 	
xlabel( 'x' ); 
ylabel( 'frequency' ); 
title('cauchy')
subplot(3,2,4)
histfit(Matrix4); 	
xlabel( 'x' ); 
ylabel( 'frequency' ); 
title('pareto')
subplot(3,2,5)
histfit(Matrix5); 	
xlabel( 'x' ); 
ylabel( 'frequency' ); 
title('binomial')
