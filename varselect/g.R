
library(deepgp)
library(lhs)
source("functions.R")

# The "a" parameter of the G function controls how important a variable is
# Here, we test out several variations on this function in 3 dimensions

d <- 3
n <- 50

gfunc <- function(x, a = (1:ncol(x) - 1)/2) {
  if (!is.matrix(x)) x <- as.matrix(x)
  n <- nrow(x)
  d <- ncol(x)
  prod <- rep(1, times = n)
  for (i in 1:d)
    prod <- prod * (abs(4*x[, i] - 2) + a[i]) / (1 + a[i])
  return(prod)
}

# First two high, last one IRRELEVANT -----------------------------------------

a <- c(0, 0, 99)
x <- randomLHS(n, d)
y <- gfunc(x, a)

fit1 <- fit_two_layer(x, y, nmcmc = 1000, swap = TRUE)
par(mfrow = c(1, 3)) 
for (i in 1:d) {
  plot(fit1$tau2_w[,i], type = "l")
}
for (i in 1:d) {
  plot(fit1$theta_w[,i], type = "l")
}


#plot(fit1, hidden = TRUE)
par(mfrow = c(1, 3))
for (i in 1:3) {
  o <- order(x[, i])
  matplot(x[o, i], t(fit1$w[, o, i]), type = "l", ylim = c(min(fit1$w), max(fit1$w)))
}
fit1 <- trim(fit1, 500)
plot_tau2(fit1)
summarize_tau2(fit1)

# First two high, last one low ------------------------------------------------

a <- c(0, 0, 9)
x <- randomLHS(n, d)
y <- gfunc(x, a)

fit2 <- fit_two_layer(x, y, nmcmc = 5000, swap = TRUE) # nugget a bit high, but still works
plot(fit2, hidden = TRUE)
par(mfrow = c(1, 3))
for (i in 1:3) {
  o <- order(x[, i])
  matplot(x[o, i], t(fit3$w[, o, i]), type = "l")
}

fit2 <- trim(fit2, 3000, 2)
plot_tau2(fit2)
summarize_tau2(fit2)

# First one high, second one medium, last one low -----------------------------

a <- c(0, 1, 5)
x <- randomLHS(n, d)
y <- gfunc(x, a)

fit3 <- fit_two_layer(x, y, nmcmc = 5000, swap = TRUE, true_g = 1e-6) 
# TODO: stop this converging to flat
plot(fit3, hidden = TRUE)
par(mfrow = c(1, 3))
for (i in 1:3) {
  o <- order(x[, i])
  matplot(x[o, i], t(fit3$w[, o, i]), type = "l")
}

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

# Our DGP that de-selected the third input
fit <- fit_two_layer(x, y, nmcmc = 5000, swap = TRUE)
fit <- trim(fit, 3000, 2)
fit <- predict(fit, xx)

# A monowarp DGP with all 3 inputs
fit_mono <- fit_two_layer(x, y, nmcmc = 5000, monowarp = TRUE)
fit_mono <- trim(fit_mono, 3000, 2)
fit_mono <- predict(fit_mono, xx)

# A monowarp DGP with only the first two inputs
fit_mono_trim <- fit_two_layer(x[, 1:2], y, nmcmc = 5000, monowarp = TRUE)
fit_mono_trim <- trim(fit_mono_trim, 3000, 2)
fit_mono_trim <- predict(fit_mono_trim, xx[, 1:2])

# A full DGP with all 3 inputs
fit_full <- fit_two_layer(x, y, nmcmc = 5000)
fit_full <- trim(fit_full, 3000, 2)
fit_full <- predict(fit_full, xx)

# A full DGP with only the first two inputs
fit_trim <- fit_two_layer(x[, 1:2], y, nmcmc = 5000)
fit_trim <- trim(fit_trim, 3000, 2)
fit_trim <- predict(fit_trim, xx[, 1:2])

# How do they compare?
par(mfrow = c(2, 3))
plot(yy, fit$mean, ylim = c(min(yy), max(yy)),
     main = paste0("RMSE = ", round(rmse(yy, fit$mean), 3), "\n CRPS = ", 
                   round(crps(yy, fit$mean, fit$s2), 3)))
plot(yy, fit_mono$mean, col = 2, ylim = c(min(yy), max(yy)),
       main = paste0("RMSE = ", round(rmse(yy, fit_mono$mean), 3), "\n CRPS = ", 
                     round(crps(yy, fit_mono$mean, fit_mono$s2), 3)))
plot(yy, fit_mono_trim$mean, col = 3, ylim = c(min(yy), max(yy)),
       main = paste0("RMSE = ", round(rmse(yy, fit_mono_trim$mean), 3), "\n CRPS = ", 
                     round(crps(yy, fit_mono_trim$mean, fit_mono_trim$s2), 3)))
plot(yy, fit_full$mean, col = 4, ylim = c(min(yy), max(yy)),
       main = paste0("RMSE = ", round(rmse(yy, fit_full$mean), 3), "\n CRPS = ", 
                     round(crps(yy, fit_full$mean, fit_full$s2), 3)))
plot(yy, fit_trim$mean, col = 5, ylim = c(min(yy), max(yy)),
       main = paste0("RMSE = ", round(rmse(yy, fit_trim$mean), 3), "\n CRPS = ", 
                     round(crps(yy, fit_trim$mean, fit_trim$s2), 3)))

