

x <- matrix(seq(0, 1, length = 100), ncol = 1)
dx <- sq_dist(x)
x_grid <- matrix(seq(0, 1, length = 50), ncol = 1)
dx_grid <- sq_dist(x_grid)
grid_index <- deepgp:::fo_approx_init(x_grid, x)

r <- 10
tau2 <- 0.001
theta <- 1

# Original monowarp transformation
w_grid <- t(rmvnorm(r, sigma = tau2*exp(-dx_grid/theta))) # draw from prior on grid
w1 <- matrix(nrow = nrow(x), ncol = r)
for (i in 1:r) {
  w1[, i] <- deepgp:::monowarp_ref(x, x_grid, w_grid[, i], grid_index)
  w1[, i] <- (w1[, i] - mean(w1[, i])) # center?
}

# New monowarp transformation
wo <- t(rmvnorm(r, sigma = tau2*exp(-dx/theta))) # draw from prior on x
w2 <- matrix(nrow = nrow(x), ncol = r)
for (i in 1:r) {
  wo_centered <- wo[, i] - wo[1, i] # make sure we start at zero
  w2[, i] <- c(0, cumsum(abs(diff(wo_centered))))
  w2[, i] <- (w2[, i] - mean(w2[, i])) # center? scale?
}

par(mfrow = c(2, 2))
matplot(x_grid, w_grid, type = "l", main = "Original GP Draws")
matplot(x, w1, type = "l", main = "Monowarped GP Draws")
matplot(x, wo, type = "l", main = "NEW GP Draws")
matplot(x, w2, type = "l", main = "NEW Monowarped GP Draws")