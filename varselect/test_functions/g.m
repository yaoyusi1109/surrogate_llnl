
n = 30;
d = 3;
a = [0, 0, 99];
x = lhsdesign(n, d);
y = gfunc(x, a);

% Fan Zhang method --------------------------------------------------------

% Run Indicator-based Bayesian variable selection algorithm
% X and y are required
results = Indicator_based_bayesian_vs_gp(x, y, ...
    'lambda', 1, ... % default 1
    'tau', 1, ... % default 1
    'sigma', 1, ... % default 1
    'q', 0.5, ... % default 0.5
    'iterations', 200, ... % default 2000
    'burnin', 100, ... % default 1000
    'scaling', 3, ...
    'verbose', 1);

% Display results
disp('Probabilities of active variables:');
disp(results.active_prob);

disp('Selected variables:');
disp(results.active_vars);

% Blind Kriging method ----------------------------------------------------

opts = BlindKriging.getDefaultOptions();
theta0 = zeros(1,d);
opts.hpBounds = [repmat(-2, 1, d); repmat(log10(4), 1, d)];
opts.hpOptimizer = SQPLabOptimizer(inDim, 1);
opts.regressionMetric = 'cvpe';
opts.retuneParameters = true;
opts.regressionMaxOrder = 2;
opts.regressionMaxLevelInteractions = 2;
blindKrige = BlindKriging(opts, theta0, 'regpoly0', @corrgauss);
blindKrige = blindKrige.fit(x, y);
	
[dummy regrFunc terms] = blindKrige.regressionFunction( struct('latex', false, 'includeCoefficients', false) );
terms

%bk_predy = blindKrige.predict(xp);
