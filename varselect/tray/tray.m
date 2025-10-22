
addpath('~/Competitors/variable_selection')
addpath('~/MATLAB_pkgs/ooDACE-1.4/ooDACE')

d = 5;
n = 100;
np = 500;
reps = 50;

for seed = 2:reps
  rng(seed)
  x = lhsdesign(n, d);
  y = trayfunc(x);
  xp = lhsdesign(np, d);
  yp = trayfunc(xp);

  % Zhang method 
  results = Indicator_based_bayesian_vs_gp(x, y, ...
    'lambda', 1, ... % default 1
    'tau', 1, ... % default 1
    'sigma', 1, ... % default 1
    'q', 0.5, ... % default 0.5
    'iterations', 200, ... % default 2000
    'burnin', 100, ... % default 1000
    'scaling', 3, ...
    'verbose', 1);
  zhang_probs = readtable('results/zhang_probs.csv');
  zhang_probs.x1(seed) = num2cell(results.active_prob(1));
  zhang_probs.x2(seed) = num2cell(results.active_prob(2));
  zhang_probs.x3(seed) = num2cell(results.active_prob(3));
  zhang_probs.x4(seed) = num2cell(results.active_prob(4));
  zhang_probs.x5(seed) = num2cell(results.active_prob(5));
  writetable(zhang_probs, 'results/zhang_probs.csv');

  % GP with only the selected variables
  xtrim = x(:, results.active_vars);
  xptrim = xp(:, results.active_vars);
  fit = oodacefit(xtrim, y);
  [mu s2] = fit.predict(xptrim);
  r = readtable('results/pred_zhang.csv');
  r.RMSE(seed) = num2cell(sqrt(mean((yp - mu).^2)));
  s = sqrt(s2);
  z = (yp - mu)./s;
  crps = mean(s.*(-1/sqrt(pi) + 2.*normpdf(z) + z.*(2.*normcdf(z)-1)));
  r.CRPS(seed) = num2cell(crps);
  writetable(r, 'results/pred_zhang.csv');

  % Blind Kriging method (code from demo.m in ooDACE toolbox)
  opts.type = 'BlindKriging';
  opts.retuneParameters = true;
  opts.regressionMaxOrder = 2;
  opts.regressionMaxLevelInteractions = 2;
  k = oodacefit(x, y, opts );
  [dummy, regrFunc, terms] = k.regressionFunction(struct('includeCoefficients', false));
  [mu s2] = k.predict(xp);

  r = readtable('results/pred_bk.csv');
  r.RMSE(seed) = num2cell(sqrt(mean((yp - mu).^2)));
  s = sqrt(s2);
  z = (yp - mu)./s;
  crps = mean(s.*(-1/sqrt(pi) + 2.*normpdf(z) + z.*(2.*normcdf(z)-1)));
  r.CRPS(seed) = num2cell(crps);
  writetable(r, 'results/pred_bk.csv');

  bk_results = readtable('results/bk_in_out.csv');
  for j = 1:d
    bk_results{seed, j+1} = num2cell(contains(regrFunc, 'x' + string(j)));
  end
  writetable(bk_results, 'results/bk_in_out.csv');

end

