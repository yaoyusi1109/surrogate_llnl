
# What is really going on with our prior distribution of the warpings
# Allow both tau2 and theta to vary?
# Prior mean of X or 0?

library(deepgp)

reps <- 10
n <- 50
x <- seq(0, 1, length = n)
warp_ord <- order(x, decreasing = FALSE)
dx <- sq_dist(x)

# For specific tau2/theta values

tau2 <- 0.01
theta <- 0.01
par(mfrow = c(2, 2))

K <- deepgp:::Matern(dx, tau2, theta, 1e-6, 2.5)
wprior <- t(mvtnorm::rmvnorm(reps, mean = rep(0, n), sigma = K))
wwarp <- deepgp:::monotransform(wprior, warp_ord)
matplot(x, wprior, type = "l", main = "pm0, original")
matplot(x, wwarp, type = "l", main = "pm0, warped")

K <- deepgp:::Matern(dx, tau2, theta, 1e-6, 2.5)
wprior <- t(mvtnorm::rmvnorm(reps, mean = x, sigma = K))
wwarp <- deepgp:::monotransform(wprior, warp_ord)
matplot(x, wprior, type = "l", main = "pmx, original")
matplot(x, wwarp, type = "l", main = "pmx, warped")

# Across various tau2/theta values

par(mfrow = c(3, 3), mar = c(2, 2, 1, 1))
for (tau2 in c(0.01, 0.1, 1)) {
  for (theta in c(0.01, 0.1, 1)) {
    K <- deepgp:::Matern(dx, tau2, theta, 1e-6, 2.5)
    wprior <- t(mvtnorm::rmvnorm(reps, mean = rep(0, n), sigma = K))
    wwarp <- deepgp:::monotransform(wprior, warp_ord)
    matplot(x, wwarp, type = "l",
            main = paste0("pm0, tau2 = ", tau2, " theta = ", theta))
  }
}

par(mfrow = c(3, 3), mar = c(2, 2, 1, 1))
for (tau2 in c(0.01, 0.1, 1)) {
  for (theta in c(0.01, 0.1, 1)) {
    K <- deepgp:::Matern(dx, tau2, theta, 1e-6, 2.5)
    wprior <- t(mvtnorm::rmvnorm(reps, mean = x, sigma = K))
    wwarp <- deepgp:::monotransform(wprior, warp_ord)
    matplot(x, wwarp, type = "l",
            main = paste0("pmx, tau2 = ", tau2, " theta = ", theta))
  }
}

# Key takeaways:
# tau2 mostly controls the range
# theta mostly controls the wiggliness
# They are both related though
# pmx is less likely to become totally flat, but will become "practically flat"
