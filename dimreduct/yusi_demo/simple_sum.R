
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
set.seed(1)

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
if (visual & d == 2) {
  i <- interp::interp(x[, 1], x[, 2], y)
  image(i)
  abline(0, 1)
  persp(i)
  pairs(cbind(x, y))
}

# Fit stationary GP
# TODO

# Fit DGP with D = 1 (which we know is the right latent dimension)
# TODO

# Fit DGP with D = d
fit <- fit_two_layer(x, y, D = d, nmcmc = 3000)
fit <- trim(fit, 2000, 2)
fit <- predict(fit, xp)

# Evaluate and save their performance
# TODO: for each model, calculate RMSE on testing set
# TODO: for each model, calculate CRPS on testing set
# TODO: for each model, store the computation time (in seconds)
# TODO: save these values in a CSV file, labeled with their random seed




#plot(fit$mean, yp)
#points(fit$mean, yp, col = 2)
#j <- 1
#i <- interp::interp(x[, 1], x[, 2], fit$w[[j]][, 1])
#image(i)
#i <- interp::interp(x[, 1], x[, 2], fit$w[[j]][, 2])
#image(i)
#i <- interp::interp(x[, 1], x[, 2], x[, 1] + x[, 2])
#image(i)

