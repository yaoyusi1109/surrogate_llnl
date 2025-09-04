
source("functions.R")
library(deepgp)
library(lhs)
set.seed(1)

# Works great with n = 50 random LHS
# with n = 20, we get an error with the monowarp version
# SOLUTION: force an upper bound on tau2_w of 1

f <- function(x) {
  x <- x*4 - 2
  p1 <- abs(100 - sqrt(apply(x^2, 1, sum)) / pi)
  p2 <- abs(apply(sin(x), 1, prod) * exp(p1)) + 1
  y <- -0.0001 * (p2)^(0.1)
  return((y + 1.9) / 0.2)
}

# Train/test data
n <- 50
x <- randomLHS(n, 2)
y <- f(x)
grid <- seq(0, 1, length = 30)
xx <- as.matrix(expand.grid(grid, grid))
yy <- f(xx)

# Dummy variable in third column
xdummy <- cbind(x, runif(nrow(x))) 
xxdummy <- cbind(xx, runif(nrow(xx)))

# Visualize surface
par(mfrow = c(1, 2))
persp(grid, grid, matrix(yy, ncol = length(grid)), theta = 30, r = 30)
image(grid, grid, matrix(yy, ncol = length(grid)), col = heat.colors(128))
contour(grid, grid, matrix(yy, ncol = length(grid)), add = TRUE)

# Fits with only two important inputs -----------------------------------------

# regular GP
fit0 <- fit_one_layer(x, y, nmcmc = 5000, sep = TRUE)
plot(fit0)
fit0 <- trim(fit0, 3000, 2)
fit0 <- predict(fit0, xx, lite = TRUE)
plot(fit0)

# monowarp DGP
fit1 <- fit_two_layer(x, y, nmcmc = 5000, monowarp = TRUE)
plot(fit1, trace = TRUE, hidden = TRUE)
fit1 <- trim(fit1, 3000, 2)
fit1 <- predict(fit1, xx, lite = TRUE)
plot(fit1)

# varselect monowarp DGP
fit2 <- fit_two_layer(x, y, nmcmc = 5000, varselect = TRUE)
plot(fit2, trace = TRUE, hidden = TRUE)
# Ok, what if we just put a cap on tau2?
#par(mfrow = c(2, 2))
#plot(fit2$tau2_w[, 1], type = "l")
#plot(fit2$tau2_w[, 2], type = "l")
#plot(fit2$theta_w[, 1], type = "l")
#plot(fit2$theta_w[, 2], type = "l")
# The tau2 values go off the chain, but the theta values stay fine
#matplot(fit2$x_grid[, 1], t(fit2$w_grid[, , 1]), type = "l")
#matplot(fit2$x_grid[, 2], t(fit2$w_grid[, , 2]), type = "l")
#matplot(x[order(x[, 1]), 1], t(fit2$w[, order(x[, 1]), 1]), type = "l")
#matplot(x[order(x[, 2]), 2], t(fit2$w[, order(x[, 2]), 2]), type = "l")
fit2 <- trim(fit2, 3000, 2)
fit2 <- predict(fit2, xx, lite = TRUE)
plot(fit2)

# How do these fits compare? 
results <- data.frame("model" = c("GP", "mDGP", "vDGP"),
                      "rmse" = c(rmse(yy, fit0$mean),
                                 rmse(yy, fit1$mean),
                                 rmse(yy, fit2$mean)),
                      "crps" = c(crps(yy, fit0$mean, fit0$s2),
                                 crps(yy, fit1$mean, fit1$s2),
                                 crps(yy, fit2$mean, fit2$s2)))
results

# Fits with a third dummy variable --------------------------------------------

# regular GP
fit0 <- fit_one_layer(xdummy, y, nmcmc = 5000, sep = TRUE)
plot(fit0) 
fit0 <- trim(fit0, 3000, 2)
fit0 <- predict(fit0, xxdummy, lite = TRUE)

# monowarp DGP
fit1 <- fit_two_layer(xdummy, y, nmcmc = 5000, monowarp = TRUE)
plot(fit1, trace = TRUE, hidden = TRUE)
fit1 <- trim(fit1, 3000, 2)
fit1 <- predict(fit1, xxdummy, lite = TRUE)

# varselect monowarp DGP
fit2 <- fit_two_layer(xdummy, y, nmcmc = 5000, varselect = TRUE)
plot(fit2, trace = TRUE, hidden = TRUE)
fit2 <- trim(fit2, 3000, 2)
fit2 <- predict(fit2, xxdummy, lite = TRUE)

# How do these fits compare?
results_dummy <- data.frame("model" = c("GP", "mDGP", "vDGP"),
                            "rmse" = c(rmse(yy, fit0$mean),
                                       rmse(yy, fit1$mean),
                                       rmse(yy, fit2$mean)),
                             "crps" = c(crps(yy, fit0$mean, fit0$s2),
                                        crps(yy, fit1$mean, fit1$s2),
                                        crps(yy, fit2$mean, fit2$s2)))
results
results_dummy
# The fits without the 3rd dummy variable did a bit better
# If we could decide to remove the 3rd one, we could refit without it

# Posterior of tau2_w
plot_tau2(fit2)
summarize_tau2(fit2)

