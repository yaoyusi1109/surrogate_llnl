
# Other TODO items:
#    1. make sure the surrogates do not estimate a noise parameter
#    2. Upgrade number of MCMC iterations to 5000?  Check trace plots and see 
#       if that's enough to get good burn-in (keep the same for all)
#    3. Incorporate a fixed random seed, and repeat for 30 or 50 different seeds
#       (could look into a foreach loop)
# After this is all done, in a new R script, plot the results

# What is the point?
# Does the DGP with a single latent dimension do as well as the other models?

library(deepgp)
library(foreach)
library(doParallel)

# Setup Parallel backend
# Detect number of cores (physical)
n_cores <- parallel::detectCores(logical = FALSE)
# Using n_cores - 1 to be safe, or just n_cores if user wants max speed. 
# Let's use n_cores but max out at say 8 if it's a huge machine to not bog everything down.
# Or just use detectCores(). 
n_cores <- max(1, parallel::detectCores(logical = FALSE))
# outfile = "" allows the worker nodes to print to the master console
cl <- makeCluster(n_cores, outfile = "")
registerDoParallel(cl)

cat(sprintf("Running on %d cores...\n", n_cores))
# Approximate run time calculation: 
# Each seed takes ~60s total for all 3 models.
# Batches = ceiling(30 seeds / n_cores).
# Approx Time = Batches * 60s.
est_batches <- ceiling(30/n_cores)
est_time_min <- est_batches * 1
cat(sprintf("Estimated completion time: ~%d minute(s) (assuming ~60s per seed)\n", est_time_min))

# TODO: 3. Incorporate a fixed random seed, and repeat for 30 or 50 different seeds
n_seeds <- 30

# Using foreach loop for parallel execution
results <- foreach(seed_i = 1:n_seeds, .combine = rbind, .packages = c("deepgp")) %dopar% {
  # Progress indicator
  cat(sprintf("[Seed %d] Starting...\n", seed_i))
  
  set.seed(seed_i)
  
  d <- 2
  n <- 20
  np <- 500
  visual <- FALSE
  f <- function(x) rowSums(x)

  # Collect training data
  x <- matrix(runif(n * d), ncol = d)
  y <- f(x)

  # Collect testing data
  xp <- matrix(runif(np * d), ncol = d)
  yp <- f(xp)

  # Optionally visualize surface
  # Note: Visualization in parallel workers usually doesn't display to the main session
  if (visual & d == 2 & seed_i == 1) {
    i <- interp::interp(x[, 1], x[, 2], y)
    image(i)
    abline(0, 1)
    persp(i)
    pairs(cbind(x, y))
  }

  # Fit stationary GP
  # TODO
  # TODO: 1. make sure the surrogates do not estimate a noise parameter (true_g = 1e-6)
  # TODO: 2. Upgrade number of MCMC iterations to 5000 (nmcmc = 5000, 2500 burnin)
  t0 <- proc.time()[3]
  fit_gp <- fit_one_layer(x, y, nmcmc = 5000, true_g = 1e-6, verb = FALSE)
  fit_gp <- trim(fit_gp, 2500, 2)
  fit_gp <- predict(fit_gp, xp, lite = FALSE)
  time_gp <- proc.time()[3] - t0

  # Fit DGP with D = 1 (which we know is the right latent dimension)
  # TODO
  t0 <- proc.time()[3]
  fit_dgp1 <- fit_two_layer(x, y, D = 1, nmcmc = 5000, true_g = 1e-6, verb = FALSE)
  fit_dgp1 <- trim(fit_dgp1, 2500, 2)
  fit_dgp1 <- predict(fit_dgp1, xp, lite = FALSE)
  time_dgp1 <- proc.time()[3] - t0

  # Fit DGP with D = d
  t0 <- proc.time()[3]
  fit <- fit_two_layer(x, y, D = d, nmcmc = 5000, true_g = 1e-6, verb = FALSE)
  fit <- trim(fit, 2500, 2)
  fit <- predict(fit, xp, lite = FALSE)
  time_dgp <- proc.time()[3] - t0

  # Evaluate and save their performance
  # TODO: for each model, calculate RMSE on testing set
  rmse_gp <- sqrt(mean((fit_gp$mean - yp)^2))
  rmse_dgp1 <- sqrt(mean((fit_dgp1$mean - yp)^2))
  rmse_dgp <- sqrt(mean((fit$mean - yp)^2))

  # TODO: for each model, calculate CRPS on testing set
  crps_gp <- mean(deepgp::crps(yp, fit_gp$mean, diag(fit_gp$Sigma)))
  crps_dgp1 <- mean(deepgp::crps(yp, fit_dgp1$mean, diag(fit_dgp1$Sigma)))
  crps_dgp <- mean(deepgp::crps(yp, fit$mean, diag(fit$Sigma)))

  # TODO: for each model, store the computation time (in seconds)
  # (Calculated above)

  # TODO: save these values in a CSV file, labeled with their random seed
  # Return the data frame for this iteration
  data.frame(
    seed = seed_i,
    model = c("Stationary GP", "DGP (D=1)", "DGP (D=d)"),
    rmse = c(rmse_gp, rmse_dgp1, rmse_dgp),
    crps = c(crps_gp, crps_dgp1, crps_dgp),
    time = c(time_gp, time_dgp1, time_dgp)
  )
}

stopCluster(cl)

# Robustly determine output path
# Try to save in dimreduct/yusi_demo if it exists (run from root)
# Otherwise save in current dir (run from folder)
outfile <- "simple_sum_results.csv"
if (dir.exists("dimreduct/yusi_demo")) {
  outfile <- "dimreduct/yusi_demo/simple_sum_results.csv"
}

write.csv(results, outfile, row.names = FALSE)
cat(sprintf("Results saved to %s\n", outfile))





#plot(fit$mean, yp)
#points(fit$mean, yp, col = 2)
#j <- 1
#i <- interp::interp(x[, 1], x[, 2], fit$w[[j]][, 1])
#image(i)
#i <- interp::interp(x[, 1], x[, 2], fit$w[[j]][, 2])
#image(i)
#i <- interp::interp(x[, 1], x[, 2], x[, 1] + x[, 2])
#image(i)

