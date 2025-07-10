
library(deepgp)
library(lhs)

f <- function(x) {
  x <- x*4 - 2
  p1 <- abs(100 - sqrt(apply(x^2, 1, sum)) / pi)
  p2 <- abs(apply(sin(x), 1, prod) * exp(p1)) + 1
  y <- -0.0001 * (p2)^(0.1)
  return((y + 1.9) / 0.2)
}

n <- 50
x <- randomLHS(n, 1)
y <- f(x)
grid <- seq(0, 1, length = 20)
#xx <- as.matrix(expand.grid(grid, grid))
xx <- matrix(seq(0, 1, length = 100), ncol = 1)
yy <- f(xx)

fit <- fit_three_layer(x, y, nmcmc = 1000, true_g = 1e-6)
plot(fit, hidden = TRUE)

fit <- trim(fit, 500, 2)
fit <- predict(fit, xx, lite = TRUE)
plot(fit)


### Monowarp fit with only the two important inputs
fit <- fit_two_layer(x, y, nmcmc = 5000, monowarp = TRUE, true_g = 1e-6)
plot(fit, trace = FALSE, hidden = TRUE)
fit <- trim(fit, 3000, 2)
fit <- predict(fit, xx, lite = TRUE)

### SWAPPED Monowarp fit with only the two important inputs
fit2 <- fit_two_layer(x, y, nmcmc = 5000, monowarp = TRUE, swap = TRUE, true_g = 1e-6)
plot(fit2, trace = FALSE, hidden = TRUE)
fit2 <- trim(fit2, 3000, 2)
fit2 <- predict(fit2, xx, lite = TRUE)

par(mfrow = c(1, 2))
image(grid, grid, matrix(fit$mean, nrow = length(grid)), col = heat.colors(128))
contour(grid, grid, matrix(fit$mean, nrow = length(grid)), add = TRUE)
image(grid, grid, matrix(fit2$mean, nrow = length(grid)), col = heat.colors(128))
contour(grid, grid, matrix(fit2$mean, nrow = length(grid)), add = TRUE)

### Monowarp fit with a third dummy variable
fit_dummy <- fit_two_layer(cbind(x, runif(n)), y, nmcmc = 5000, monowarp = TRUE, true_g = 1e-6)
plot(fit_dummy, trace = FALSE, hidden = TRUE)
fit_dummy <- trim(fit_dummy, 3000, 2)
fit_dummy <- predict(fit_dummy, cbind(xx, runif(nrow(xx))), lite = TRUE)

### SWAPPED Monowarp fit with a third dummy variable
fit_dummy2 <- fit_two_layer(cbind(x, runif(n)), y, nmcmc = 5000, monowarp = TRUE, swap = TRUE, true_g = 1e-6)
plot(fit_dummy2, trace = FALSE, hidden = TRUE)
fit_dummy2 <- trim(fit_dummy2, 3000, 2)
fit_dummy2 <- predict(fit_dummy2, cbind(xx, runif(nrow(xx))), lite = TRUE)

par(mfrow = c(1, 2))
image(grid, grid, matrix(fit_dummy$mean, nrow = length(grid)), col = heat.colors(128))
contour(grid, grid, matrix(fit_dummy$mean, nrow = length(grid)), add = TRUE)
image(grid, grid, matrix(fit_dummy2$mean, nrow = length(grid)), col = heat.colors(128))
contour(grid, grid, matrix(fit_dummy2$mean, nrow = length(grid)), add = TRUE)

# How does performance compare?  About the same!
rmse(yy, fit$mean)
rmse(yy, fit2$mean)
rmse(yy, fit_dummy$mean)
rmse(yy, fit_dummy2$mean)
crps(yy, fit$mean, fit$s2)
crps(yy, fit2$mean, fit2$s2)
crps(yy, fit_dummy$mean, fit_dummy$s2)
crps(yy, fit_dummy2$mean, fit_dummy2$s2)

# -----------------------------------------------------------------------------

# Investigate - what can we objectively use to decide that x3 is not important?

# The lengthscale is much larger
par(mfrow = c(2, 3))
plot(fit_dummy$theta_w[, 1], type = "l")
plot(fit_dummy$theta_w[, 2], type = "l")
plot(fit_dummy$theta_w[, 3], type = "l"); abline(h = 1, col = 2, lty = 2)
hist(fit_dummy$theta_w[, 1])
hist(fit_dummy$theta_w[, 2])
hist(fit_dummy$theta_w[, 3]); abline(v = 1, col = 2, lty = 2)

ranges <- matrix(nrow = fit_dummy$nmcmc, ncol = 3)
for (i in 1:fit_dummy$nmcmc) {
  for (j in 1:3) {
    ranges[i, j] <- diff(range(fit_dummy$w[[i]][, j]))
  }
}

par(mfrow = c(2, 3))
plot(ranges[, 1], type = "l")
plot(ranges[, 2], type = "l")
plot(ranges[, 3], type = "l")
hist(ranges[, 1])
hist(ranges[, 2])
hist(ranges[, 3])







