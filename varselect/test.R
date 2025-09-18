
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
