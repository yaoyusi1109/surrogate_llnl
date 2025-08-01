n = 10;
d = 2;
x = lhsdesign(n, d);
y = gfunc(x);

xp = lhsdesign(500, d);
yp = gfunc(xp);

% traditional kriging
k = oodacefit(x, y);
[mu, s2] = k.predict(xp);

% blind kriging
opts.type = 'BlindKriging';
opts.retuneParameters = true;
k = oodacefit(x, y, opts);
[mu, s2] = k.predict(xp);

inDim = d;
metricName = 'cvpe';
% blind kriging with summary output?
opts = BlindKriging.getDefaultOptions();
theta0 = zeros(1,inDim);
opts.hpBounds = [repmat(-2, 1, inDim); repmat(log10(4), 1, inDim)];
opts.hpOptimizer = SQPLabOptimizer(inDim, 1);

opts.regressionMetric = metricName;
opts.retuneParameters = true;
opts.regressionMaxOrder = 2;
opts.regressionMaxLevelInteractions = 2;
blindKrige = BlindKriging(opts, theta0, 'regpoly0', @corrgauss);
[blindKrige ordinaryKrige] = blindKrige.fit(x, y);
	
[dummy regrFunc terms] = blindKrige.regressionFunction( struct('latex', false, 'includeCoefficients', false) ); % latex output of regression function

ok_predy = ordinaryKrige.predict(xp);
bk_predy = blindKrige.predict(xp);


scatter(yp, ok_predy)
scatter(yp, bk_predy)