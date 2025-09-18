
source("functions.R")
library(deepgp)
library(lhs)
set.seed(1)

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

# Fits with a third dummy variable --------------------------------------------

# regular GP
fit0 <- fit_one_layer(xdummy, y, nmcmc = 5000, sep = TRUE)
plot(fit0) 
fit0 <- trim(fit0, 3000, 2)
fit0 <- predict(fit0, xxdummy, lite = TRUE)

# regular DGP
fit1 <- fit_one_layer(xdummy, y, nmcmc = 5000)
plot(fit1)
fit1 <- trim(fit1, 3000, 2)
fit1 <- predict(fit1, xxdummy, lite = TRUE)

# varselect monowarp DGP
fit2 <- fit_two_layer(xdummy, y, nmcmc = 5000, monowarp = "axis-aligned")
plot(fit2)
fit2 <- trim(fit2, 3000, 2)
plot(fit2, hidden = TRUE)
fit2 <- predict(fit2, xxdummy, lite = TRUE)

# How do these fits compare?
rmse(yy, fit0$mean)
rmse(yy, fit1$mean)
rmse(yy, fit2$mean)
crps(yy, fit0$mean, fit0$s2)
crps(yy, fit1$mean, fit1$s2)
crps(yy, fit2$mean, fit2$s2)

plot_tau2(fit2)

