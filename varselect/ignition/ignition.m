
addpath('~/Competitors/variable_selection')
addpath('~/MATLAB_pkgs/ooDACE-1.4/ooDACE')

d = 13;
n = 200;
np = 1000;
reps = 50;

for seed = 27:reps
  rng(seed)
  x = lhsdesign(n, d);
  y = ignitionfunc(x);
  xp = lhsdesign(np, d);
  yp = ignitionfunc(xp);

  % Zhang method 
  %results = Indicator_based_bayesian_vs_gp(x, y, ...
  %    'lambda', 1, ... % default 1
  %    'tau', 1, ... % default 1
  %    'sigma', 1, ... % default 1
  %    'q', 0.5, ... % default 0.5
  %    'iterations', 500, ... % default 2000
  %    'burnin', 250, ... % default 1000
  %    'scaling', 3, ...
  %    'verbose', 1);
  %zhang_results = readtable('results/zhang_probs.csv');
  %zhang_results{seed, 2:(d+1)} = num2cell(results.active_prob);
  %writetable(zhang_results, 'results/zhang_probs.csv');

  % GP with only the selected variables
  %xtrim = x(:, results.active_vars);
  %xptrim = xp(:, results.active_vars);
  %fit = oodacefit(xtrim, y);
  %[mu s2] = fit.predict(xptrim);
  %rmse = sqrt(mean((yp - mu).^2));
  %s = sqrt(s2);
  %z = (yp - mu)./s;
  %crps = mean(s.*(-1/sqrt(pi) + 2.*normpdf(z) + z.*(2.*normcdf(z)-1)));
  %r = readtable('results/pred_zhang.csv');
  %r.RMSE{seed} = rmse;
  %r.CRPS{seed} = crps;
  %writetable(r, 'results/pred_zhang.csv');

  % Blind Kriging method (code from demo.m in ooDACE toolbox)
  opts.type = 'BlindKriging';
  opts.retuneParameters = true;
  opts.regressionMaxOrder = 2;
  opts.regressionMaxLevelInteractions = 2;
  k = oodacefit(x, y, opts );
  [dummy, regrFunc, terms] = k.regressionFunction(struct('includeCoefficients', false));
  bk_results = readtable('results/bk_in_out.csv');
  for j = 1:d
    bk_results(seed, j+1) = num2cell(contains(regrFunc, 'x' + string(j)));
  end
  writetable(bk_results, 'results/bk_in_out.csv');
  
  [mu s2] = k.predict(xp);
  rmse = sqrt(mean((yp - mu).^2));
  s = sqrt(s2);
  z = (yp - mu)./s;
  crps = mean(s.*(-1/sqrt(pi) + 2.*normpdf(z) + z.*(2.*normcdf(z)-1)));
  r = readtable('results/pred_bk.csv');
  r.RMSE(seed) = rmse;
  r.CRPS(seed) = crps;
  writetable(r, 'results/pred_bk.csv');


end

