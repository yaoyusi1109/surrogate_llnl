
library(deepgp)
library(lhs)
library(invgamma)

borehole <- function(x) {
  if (!is.matrix(x)) x <- as.matrix(x)
  rw <- x[, 1] * (0.15 - 0.05) + 0.05
  r  <- x[, 2] * (50000 - 100) + 100
  Tu <- x[, 3] * (115600 - 63070) + 63070
  Hu <- x[, 4] * (1110 - 990) + 990
  Tl <- x[, 5] * (116 - 63.1) + 63.1
  Hl <- x[, 6] * (820 - 700) + 700
  L  <- x[, 7] * (1680 - 1120) + 1120
  Kw <- x[, 8] * (12045 - 9855) + 9855
  
  frac1 <- 2 * pi * Tu * (Hu-Hl)
  frac2a <- 2*L*Tu / (log(r/rw)*rw^2*Kw)
  frac2b <- Tu / Tl
  frac2 <- log(r/rw) * (1+frac2a+frac2b)
  y <- frac1 / frac2
  return((y - 78) / 46) # about zero mean with unit variance
}

d <- 10 # 8 meaningful, 2 irrelevant
n <- 100
x <- randomLHS(n, d)
y <- borehole(x)
np <- 500
xp <- randomLHS(np, d)
yp <- borehole(xp)

# Regular GP
fit <- fit_one_layer(x, y, nmcmc = 3000, sep = TRUE)
plot(fit)
fit <- trim(fit, 1000, 20)
fit <- predict(fit, xp)

# Varselect DGP
fits <- fit_two_layer(x, y, nmcmc = 2000, monowarp = "axis-aligned")
plot(fits)
fits <- trim(fits, 1000, 2)
plot(fits, hidden = TRUE)
fits <- predict(fits, xp)

upper <- apply(fits$tau2_w, 2, quantile, p = 0.99)
plot(1:(d+1), upper)

plot(yp, fit$mean)
points(yp, fits$mean, col = 2)
abline(0, 1)

rmse(yp, fit$mean); crps(yp, fit$mean, fit$s2)
rmse(yp, fits$mean); crps(yp, fits$mean, fits$s2)

# Our varselect DGP does way better!
