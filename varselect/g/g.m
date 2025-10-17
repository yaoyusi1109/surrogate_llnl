
addpath('~/Competitors/variable_selection')
addpath('~/MATLAB_pkgs/ooDACE-1.4/ooDACE')

d = 4;
n = 50;
np = 500;
a = [0, 0, 99, 99];
reps = 50;

sz = [reps d + 1];
varTypes = repmat({'int8'}, 1, d+1);
varNames = ['seed','x'+string(1:d)];
zhang_results = table('Size',sz,'VariableTypes',varTypes,'VariableNames',varNames);
bk_results = table('Size',sz,'VariableTypes',varTypes,'VariableNames',varNames);

for seed = 27:reps
  rng(seed)
  x = lhsdesign(n, d);
  y = gfunc(x, a);
  xp = lhsdesign(np, d);
  yp = gfunc(xp, a);

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
  zhang_results{seed, :} = [seed, results.active_prob];
  writetable(zhang_results, 'results/zhang_probs.csv');

  % GP with only the selected variables
  xtrim = x(:, results.active_vars);
  xptrim = xp(:, results.active_vars);
  fit = oodacefit(xtrim, y);
  [mu s2] = fit.predict(xptrim);
  r = readtable('results/pred_zhang.csv');
  r.RMSE(r.seed == seed) = sqrt(mean((yp - mu).^2));
  s = sqrt(s2);
  z = (yp - mu)./s;
  crps = mean(s.*(-1/sqrt(pi) + 2.*normpdf(z) + z.*(2.*normcdf(z)-1)));
  r.CRPS(r.seed == seed) = crps;
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
  r.RMSE(r.seed == seed) = sqrt(mean((yp - mu).^2));
  s = sqrt(s2);
  z = (yp - mu)./s;
  crps = mean(s.*(-1/sqrt(pi) + 2.*normpdf(z) + z.*(2.*normcdf(z)-1)));
  r.CRPS(r.seed == seed) = crps;
  writetable(r, 'results/pred_bk.csv');

  bk_results.seed(seed) = seed;
  for j = 1:d
    bk_results{seed, j+1} = contains(regrFunc, 'x' + string(j));
  end
  writetable(bk_results, 'results/bk_in_out.csv');

end

