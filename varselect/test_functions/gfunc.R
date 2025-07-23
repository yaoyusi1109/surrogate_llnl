
library(deepgp)
library(lhs)
source("functions.R")

# The "a" parameter of the G function controls how important a variable is
# Here, we test out several variations on this function in 3 dimensions

d <- 3
n <- 50

gfunc <- function(x, a = (1:d - 1)/2) {
  if (!is.matrix(x)) x <- as.matrix(x)
  prod <- 1
  for (i in 1:ncol(x))
    prod <- prod * (abs(4*x[, i] - 2) + a[i]) / (1 + a[i])
  return(prod)
}

# First two high, last one IRRELEVANT -----------------------------------------

a <- c(0, 0)
x <- randomLHS(n, d - 1)
y <- gfunc(x)
x <- cbind(x, runif(n))

fit <- fit_two_layer(x, y, nmcmc = 5000, monowarp = TRUE, swap = TRUE,
                     true_g = 1e-6, pmx = TRUE)
plot(fit, hidden = TRUE)
fit <- trim(fit, 3000, 2)
plot_tau2(fit)
summarize_tau2(fit)

# First two high, last one low effect -----------------------------------------

a <- c(0, 0, 99)
x <- randomLHS(n, d)
y <- gfunc(x, a)

fit <- fit_two_layer(x, y, nmcmc = 5000, monowarp = TRUE, swap = TRUE,
                     true_g = 1e-6, pmx = TRUE)
plot(fit, hidden = TRUE)
fit <- trim(fit, 3000, 2)
plot_tau2(fit)
summarize_tau2(fit)

# First one high, second one medium, last one low -----------------------------

a <- c(0, 1, 99)
x <- randomLHS(n, d)
y <- gfunc(x, a)

fit <- fit_two_layer(x, y, nmcmc = 5000, monowarp = TRUE, swap = TRUE,
                     true_g = 1e-6, pmx = TRUE) 
plot(fit, hidden = TRUE)
fit <- trim(fit, 3000, 2)
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
