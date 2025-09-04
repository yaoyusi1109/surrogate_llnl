
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

d <- 8 
n <- 100
x <- randomLHS(n, d)
x <- cbind(x, runif(n))
y <- borehole(x)
np <- 500
xp <- randomLHS(np, d)
xp <- cbind(xp, runif(np))
yp <- borehole(xp)

# Regular GP
fit <- fit_one_layer(x, y, nmcmc = 3000, sep = TRUE)
plot(fit)
fit <- trim(fit, 1000, 20)
fit <- predict(fit, xp)

# Varselect DGP
fits <- fit_two_layer(x, y, nmcmc = 2000, varselect = TRUE)
plot(fits)
par(mfrow = c(3, 3))
for (i in 1:(d+1))
  plot(fits$tau2_w[, i], type = "l")
for (i in 1:(d+1))
  plot(fits$theta_w[, i], type = "l")
#fits <- trim(fits, 1000, 2)
plot(fits, trace = FALSE, hidden = TRUE)
# OK here all the tau2 are going to zero, this is a new problem
# Perhaps we also need a lower bound on tau2????
fits <- predict(fits, xp)

plot(yp, fit$mean)
points(yp, fits$mean, col = 2)
abline(0, 1)

par(mfrow = c(1, 3))
hist(sqrt(fit$s2))
hist(sqrt(fits$s2))

rmse(yp, fit$mean); crps(yp, fit$mean, fit$s2)
rmse(yp, fits$mean); crps(yp, fits$mean, fits$s2)

