
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
xx <- randomLHS(500, d)
yy <- gfunc(xx, a)

fit <- fit_two_layer(x, y, nmcmc = 5000, monowarp = "axis-aligned")
plot(fit)
fit <- trim(fit, 3000, 5)
plot(fit, hidden = TRUE)
fit <- predict(fit, xx)

plot_tau2(fit)
summarize_tau2(fit)

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

