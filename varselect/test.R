
# Numerical issue

# Sometimes, tau2_w goes to zero
# This makes all points converge to a single point
# Then tau2_y goes to infinity
# Model is garbage
# WHY IS THIS HAPPENING AND HOW DO WE STOP IT?

# Other times, tau2_w goes to infinity
# Warpings (for no reason) go to large values even though they hold their shape
# This happens when theta_w is too small?

# Could this be fixed simply by estimating the nugget?
# I'm pretty confident we should NOT be fixing the nugget if we are potentially
# deselecting inputs

# Perhaps we need a lower bound on tau2_w???

library(deepgp)
library(lhs)
source("functions.R")

d <- 3
n <- 50

gfunc <- function(x, a = (1:d - 1)/2) {
  if (!is.matrix(x)) x <- as.matrix(x)
  prod <- 1
  for (i in 1:ncol(x))
    prod <- prod * (abs(4*x[, i] - 2) + a[i]) / (1 + a[i])
  return(prod)
}

set.seed(1) 
a <- c(0, 1, 99)
x <- randomLHS(n, d)
y <- gfunc(x, a)

fit <- fit_two_layer(x, y, nmcmc = 5000, varselect = TRUE)
fit$settings$swap = TRUE
plot(fit)
par(mfrow = c(1, 3))
plot(fit$theta_w[, 1], type = "l")
plot(fit$theta_w[, 2], type = "l")
plot(fit$theta_w[, 3], type = "l")

# fit <- trim(fit, 3000, 5)

par(mfrow = c(1, 3))
for (i in 1:3) {
  o <- order(x[, i])
  matplot(x[o, i], t(fit$w[, o, i]), type = "l")
}

for (i in 1:3) {
  matplot(fit$x_grid[, i], t(fit$w_grid[, , i]), type = "l")
}

# The prior mean is zero, why are we getting such high and low samples???
ng <- nrow(fit$x_grid)
xdmat_grid <- array(dim = c(ng, ng, D))
for (i in 1:D) xdmat_grid[, , i] <- sq_dist(fit$x_grid[, i])
grid_index <- deepgp:::fo_approx_init(x_grid, x)
sigma <- Matern(xdmat_grid[, , 1], tau2 = fit$tau2_w[1, 1], theta = fit$theta_w[1, 1], 
                g = eps, v = 2.5)
w_grid_prior <- mvtnorm::rmvnorm(100, mean = rep(0, times = ng), sigma = sigma)

matplot(fit$x_grid, t(w_grid_prior), type = "l")
matplot(fit$x_grid, t(w_grid_prior * 0.5), type = "l")


K <- Matern(xdmat_grid[, , i], 1, theta_w[j, i], eps, v) 





plot_tau2(fit)
summarize_tau2(fit)

# NOW, how does variable selection affect predictions? ------------------------
# Estimate nugget???

# What should we really be comparing?

# All we need to show is that our variable selecting DGP does better than 
# a regular DGP applied on all the variables
# The variable selecting DGP should match the DGP with dummy variables removed


# Use high, medium, low setting
a <- c(0, 1, 99)
x <- randomLHS(n, d)
y <- gfunc(x, a)

xx <- randomLHS(500, d)
yy <- gfunc(xx)

# First, fit with all 3 variables
fit <- fit_two_layer(x, y, nmcmc = 5000, monowarp = TRUE, swap = TRUE)
plot(fit, hidden = TRUE)
fit <- trim(fit, 3000, 2)
fit <- predict(fit, xx)

# Non monowarp
fit1 <- fit_two_layer(x, y, nmcmc = 5000)
plot(fit1, hidden = TRUE)
fit1 <- trim(fit1, 3000, 2)
fit1 <- predict(fit1, xx)

# Now, fit with only the first 2 variables (estimate g?) - Non monowarp?
fit2 <- fit_two_layer(x[, 1:2], y, nmcmc = 5000)
plot(fit2, hidden = TRUE)
fit2 <- trim(fit2, 3000, 2)
fit2 <- predict(fit2, xx[, 1:2])

plot(yy, fit$mean)
points(yy, fit1$mean, col = 2)
points(yy, fit2$mean, col = 3)

rmse(yy, fit$mean)
rmse(yy, fit1$mean)
rmse(yy, fit2$mean)

crps(yy, fit$mean, fit$s2)
crps(yy, fit1$mean, fit1$s2)
crps(yy, fit2$mean, fit2$s2)

fit$time
fit1$time
fit2$time
