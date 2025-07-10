
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

# Fits with only two important inputs -----------------------------------------

# regular GP
fit0 <- fit_one_layer(x, y, nmcmc = 5000, true_g = 1e-6, sep = TRUE)
plot(fit0)
fit0 <- trim(fit0, 3000, 2)
fit0 <- predict(fit0, xx, lite = TRUE)
plot(fit0)

# monowarp DGP
fit1 <- fit_two_layer(x, y, nmcmc = 5000, monowarp = TRUE, true_g = 1e-6)
plot(fit1, trace = TRUE, hidden = TRUE)
fit1 <- trim(fit1, 3000, 2)
fit1 <- predict(fit1, xx, lite = TRUE)
plot(fit1)

# swapped monowarp DGP, theta = 1
fit2 <- fit_two_layer(x, y, nmcmc = 5000, monowarp = TRUE, swap = TRUE, 
                      true_g = 1e-6, settings = list(theta_w = 1))
plot(fit2, trace = TRUE, hidden = TRUE)
fit2 <- trim(fit2, 3000, 2)
fit2 <- predict(fit2, xx, lite = TRUE)
plot(fit2)

# swapped monowarp DGP, theta = 0.1
fit3 <- fit_two_layer(x, y, nmcmc = 5000, monowarp = TRUE, swap = TRUE, 
                      true_g = 1e-6, settings = list(theta_w = 0.1))
plot(fit3, trace = TRUE, hidden = TRUE)
fit3 <- trim(fit3, 3000, 2)
fit3 <- predict(fit3, xx, lite = TRUE)
plot(fit3)

# swapped monowarp DGP, theta = 0.01
fit4 <- fit_two_layer(x, y, nmcmc = 5000, monowarp = TRUE, swap = TRUE, 
                      true_g = 1e-6, settings = list(theta_w = 0.01))
plot(fit4, trace = TRUE, hidden = TRUE)
fit4 <- trim(fit4, 3000, 2)
fit4 <- predict(fit4, xx, lite = TRUE)
plot(fit4)

save(fit0, fit1, fit2, fit3, fit4, file = "crosstray_2d_fits.RData")

# How do these fits compare? --------------------------------------------------

load("crosstray_2d_fits.RData")

results <- data.frame("model" = c("GP", "mDGP", "msDGP_1", "msDGP_0.1", "msDGP_0.01"),
                      "rmse" = c(rmse(yy, fit0$mean),
                                 rmse(yy, fit1$mean),
                                 rmse(yy, fit2$mean),
                                 rmse(yy, fit3$mean),
                                 rmse(yy, fit4$mean)),
                      "crps" = c(crps(yy, fit0$mean, fit0$s2),
                                 crps(yy, fit1$mean, fit1$s2),
                                 crps(yy, fit2$mean, fit2$s2),
                                 crps(yy, fit3$mean, fit3$s2),
                                 crps(yy, fit4$mean, fit4$s2)))

par(mfrow = c(1, 2), mar = c(7, 5, 2, 2))
plot(1:5, results$rmse, xaxt = "n", ylab = "RMSE", xlab = "")
axis(1, 1:5, labels = results$model, las = 2)
plot(1:5, results$crps, xaxt = "n", ylab = "CRPS", xlab = "")
axis(1, 1:5, labels = results$model, las = 2)

# Zoom in on fit1 (the original monoDGP) and fit3 (the "default" swapped monowarp DGP)
plot(fit1, trace = TRUE, predict = FALSE)
plot(fit3, trace = TRUE, predict = FALSE)
# All these values are reasonable

# Fits with a third dummy variable --------------------------------------------

# regular GP
fit0 <- fit_one_layer(xdummy, y, nmcmc = 5000, true_g = 1e-6, sep = TRUE)
plot(fit0) 
fit0 <- trim(fit0, 3000, 2)
fit0 <- predict(fit0, xxdummy, lite = TRUE)

# monowarp DGP
fit1 <- fit_two_layer(xdummy, y, nmcmc = 5000, monowarp = TRUE, true_g = 1e-6)
plot(fit1, trace = TRUE, hidden = TRUE) # thid warping is linear, relatively flat
fit1 <- trim(fit1, 3000, 2)
fit1 <- predict(fit1, xxdummy, lite = TRUE)

# swapped monowarp DGP, theta = 1
fit2 <- fit_two_layer(xdummy, y, nmcmc = 5000, monowarp = TRUE, swap = TRUE, 
                      true_g = 1e-6, settings = list(theta_w = 1))
plot(fit2, trace = TRUE, hidden = TRUE)
fit2 <- trim(fit2, 3000, 2)
fit2 <- predict(fit2, xxdummy, lite = TRUE)

# swapped monowarp DGP, theta = 0.1
fit3 <- fit_two_layer(xdummy, y, nmcmc = 5000, monowarp = TRUE, swap = TRUE, 
                      true_g = 1e-6, settings = list(theta_w = 0.1))
plot(fit3, trace = TRUE, hidden = TRUE)
fit3 <- trim(fit3, 3000, 2)
fit3 <- predict(fit3, xxdummy, lite = TRUE)

# swapped monowarp DGP, theta = 0.01
fit4 <- fit_two_layer(xdummy, y, nmcmc = 5000, monowarp = TRUE, swap = TRUE, 
                      true_g = 1e-6, settings = list(theta_w = 0.01))
plot(fit4, trace = TRUE, hidden = TRUE)
fit4 <- trim(fit4, 3000, 2)
fit4 <- predict(fit4, xxdummy, lite = TRUE)

save(fit0, fit1, fit2, fit3, fit4, file = "crosstray_3d_fits.RData")

# How do these fits compare? --------------------------------------------------

results <- data.frame("model" = c("GP", "mDGP", "msDGP_1", "msDGP_0.1", "msDGP_0.01"),
                      "rmse" = c(rmse(yy, fit0$mean),
                                 rmse(yy, fit1$mean),
                                 rmse(yy, fit2$mean),
                                 rmse(yy, fit3$mean),
                                 rmse(yy, fit4$mean)),
                      "crps" = c(crps(yy, fit0$mean, fit0$s2),
                                 crps(yy, fit1$mean, fit1$s2),
                                 crps(yy, fit2$mean, fit2$s2),
                                 crps(yy, fit3$mean, fit3$s2),
                                 crps(yy, fit4$mean, fit4$s2)))

par(mfrow = c(1, 2), mar = c(7, 5, 2, 2))
plot(1:5, results$rmse, xaxt = "n", ylab = "RMSE", xlab = "")
axis(1, 1:5, labels = results$model, las = 2)
plot(1:5, results$crps, xaxt = "n", ylab = "CRPS", xlab = "")
axis(1, 1:5, labels = results$model, las = 2)

# Zoom in on fit1 (regular monowarp DGP) and fit4 (best swap monowarp DGP)
plot(fit1, trace = TRUE, predict = FALSE) # no easy decision rule...
plot(fit4, trace = TRUE, predict = FALSE) # clear decision rule!

# What about the posterior of tau2? -------------------------------------------

library(invgamma)
library(viridis)
col <- viridis(50)

fit <- fit_two_layer(xdummy, y, nmcmc = 5000, monowarp = TRUE, swap = TRUE,
                     true_g = 1e-6, settings = list(theta_w = 0.01))


dx <- sq_dist(x)
alpha <- n/2
tau2_grid <- seq(0, 4, length = 100)

# Visualize posterior distribution across iterations
par(mfrow = c(1, 3))
for (d in 1:3) {
  col_indx <- 1
  for (t in floor(seq(1, fit$nmcmc, length = 50))) {
    w <- fit$w[t, , d]
    K <- deepgp:::Matern(dx, tau2 = fit$tau2_w[t], theta = fit$settings$theta_w,
                         g = deepgp:::eps, v = fit$v)
    Kinv <- solve(K)
    beta <- (1/2) * t(w) %*% Kinv %*% w

    dens <- dinvgamma(tau2_grid, shape = alpha, rate = beta)
    if (t == 1) {
      plot(tau2_grid, dens/max(dens), 
           col = col[1], type = "l", xlab = "tau2_w", ylab = "Density", 
           main = paste0("Dimension ", d))
    } else {
      lines(tau2_grid, dens/max(dens), col = col[col_indx])
    }
    col_indx <- col_indx + 1
  }
}

# Track posterior probability of being less than 1?

threshold <- 0.1

post_prob_less <- matrix(nrow = fit$nmcmc, ncol = 3)
for (d in 1:3) {
  for (t in 1:fit$nmcmc) {
    w <- fit$w[t, , d]
    K <- deepgp:::Matern(dx, tau2 = fit$tau2_w[t], theta = fit$settings$theta_w,
                         g = deepgp:::eps, v = fit$v)
    Kinv <- solve(K)
    beta <- (1/2) * t(w) %*% Kinv %*% w
    post_prob_less_than_1[t, d] <- pinvgamma(threshold, shape = alpha, rate = beta)
  }
}

par(mfrow = c(1, 3))
for (d in 1:3) {
  plot(post_prob_less_than_1[, d], type = "l", ylim = c(0, 1),
       xlab = "Iteration", ylab = paste0("P(tau2 < ", threshold, ")"), 
       main = paste0("Dimension ", d))
}
