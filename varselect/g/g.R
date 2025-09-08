
library(deepgp)
library(lhs)
source("../functions.R")

set.seed(1)

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

a <- c(0, 0, 99)
x <- randomLHS(n, d)
y <- gfunc(x, a)

fit <- fit_two_layer(x, y, nmcmc = 5000, varselect = TRUE)
fit <- trim(fit, 3000, 2)

alpha <- n/2
beta <- fit$tau2_w*n/2
upper <- qinvgamma(1 - 0.01, shape = alpha, rate = beta)
meds <- apply(upper, 2, median)
important <- (meds > 1e-4)
