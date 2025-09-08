
library(deepgp)
library(mvtnorm)
samples <- 5

x <- as.matrix(seq(0, 1, length = 50))
xdmat <- sq_dist(x)

# Draw from a GP prior
w <- t(rmvnorm(samples, sigma = 0.1*exp(-xdmat/0.01)))

par(mfrow = c(1, 3))
matplot(x, w, type = "l")

# Original monowarp
index <- deepgp:::fo_approx_init(x, x)
w1 <- matrix(nrow = nrow(w), ncol = ncol(w))
for (i in 1:samples) {
  w1[, i] <- deepgp:::monowarp_ref(x, x, w[, i], index)
}
matplot(x, w1, type = "l")

# New monowarp
w2 <- matrix(nrow = nrow(w), ncol = ncol(w))
for (i in 1:samples) {
  w2[, i] <- c(0, cumsum(abs(diff(w[, i]))))
  w2[, i] <- w2[, i] - mean(w2[, i])
}
matplot(x, w2, type = "l")
