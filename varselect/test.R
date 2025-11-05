
library(deepgp)
library(lhs)

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
xx <- randomLHS(500, d)
yy <- gfunc(xx, a)

fit <- fit_two_layer(x, y, nmcmc = 5000, monowarp = TRUE)
plot(fit)
fit <- trim(fit, 3000, 2)
plot(fit, hidden = TRUE)
fit <- predict(fit, xx)

plot(yy, fit$mean)

# Compare to original fit
fit0 <- fit_two_layer(x, y, nmcmc = 5000)
plot(fit0)
fit0 <- trim(fit0, 3000, 2)
fit0 <- predict(fit0, xx)

# Performance in our variable selectin model is BETTER!
plot(yy, fit$mean)
points(yy, fit0$mean, col = 2)
rmse(yy, fit$mean)
rmse(yy, fit0$mean)
crps(yy, fit$mean, fit$s2)
crps(yy, fit0$mean, fit0$s2)

# First two high, last one IRRELEVANT -----------------------------------------

a <- c(0, 0, 99)
x <- randomLHS(n, d)
y <- gfunc(x, a)

fit1 <- fit_two_layer(x, y, nmcmc = 5000, varselect = TRUE)
plot(fit1, hidden = TRUE)
fit1 <- trim(fit1, 3000, 2)
plot_tau2(fit1)
summarize_tau2(fit1)

# First two high, last one low ------------------------------------------------

a <- c(0, 0, 9)
x <- randomLHS(n, d)
y <- gfunc(x, a)

fit2 <- fit_two_layer(x, y, nmcmc = 5000, varselect = TRUE)
plot(fit2, hidden = TRUE)
fit2 <- trim(fit2, 3000, 2)
plot_tau2(fit2)
summarize_tau2(fit2)

# First one high, second one medium, last one low -----------------------------

a <- c(0, 1, 5)
x <- randomLHS(n, d)
y <- gfunc(x, a)

fit3 <- fit_two_layer(x, y, nmcmc = 5000, varselect = TRUE) 
plot(fit3, hidden = TRUE)
fit3 <- trim(fit3, 3000, 2)
plot_tau2(fit3)
summarize_tau2(fit3)

# NOW, how does variable selection affect predictions? ------------------------

# What should we really be comparing?

# All we need to show is that our variable selecting DGP does better than 
# a regular DGP applied on all the variables, right?
# The variable selecting DGP should match the DGP with dummy variables removed

# When the last input is irrelevant
a <- c(0, 0, 99)
x <- randomLHS(n, d)
y <- gfunc(x, a)
xx <- randomLHS(500, d)
yy <- gfunc(xx, a)

# Our monowarp DGP without the 3rd input
fit1 <- fit_two_layer(x[, 1:2], y, nmcmc = 5000, monowarp = TRUE)
fit1 <- trim(fit1, 3000, 2)
fit1 <- predict(fit1, xx[, 1:2])

# Or if we leave it in, but let it take itself out
fit2 <- fit_two_layer(x, y, nmcmc = 5000, varselect = TRUE)
fit2 <- trim(fit2, 3000, 2)
fit2 <- predict(fit2, xx)

# How do they compare?
plot(yy, fit1$mean)
points(yy, fit2$mean, col = 2)

rmse(yy, fit1$mean)
rmse(yy, fit2$mean)
crps(yy, fit1$mean, fit1$s2)
crps(yy, fit2$mean, fit2$s2)

# We did MARGINIALLY better without the 3rd input

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

# regular DGP
fit1 <- fit_two_layer(xdummy, y, nmcmc = 5000)
plot(fit1)
fit1 <- trim(fit1, 3000, 2)
fit1 <- predict(fit1, xxdummy, lite = TRUE)
plot(yy, fit1$mean)

# monowarp DGP - bugs out?
fit2 <- fit_two_layer(xdummy, y, nmcmc = 5000, monowarp = TRUE)
plot(fit2, hidden = TRUE)
fit2 <- trim(fit2, 3000, 2)
plot(fit2, hidden = TRUE)
fit2 <- predict(fit2, xxdummy, lite = TRUE)
plot(yy, fit1$mean)
points(yy, fit2$mean, col = 2)

# monowarp DGP with pmx
fit3 <- fit_two_layer(xdummy, y, nmcmc = 5000, monowarp = TRUE, pmx = TRUE)
plot(fit3)
fit3 <- trim(fit3, 3000, 2)
plot(fit3, hidden = TRUE)
fit3 <- predict(fit3, xxdummy, lite = TRUE)

# How do these fits compare?
rmse(yy, fit1$mean)
rmse(yy, fit2$mean)
rmse(yy, fit3$mean)
crps(yy, fit1$mean, fit1$s2)
crps(yy, fit2$mean, fit2$s2)
crps(yy, fit3$mean, fit3$s2)



