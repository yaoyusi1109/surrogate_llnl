
library(deepgp)
library(lhs)

gfunc <- function(x, a = (1:ncol(x) - 1)/2) {
  if (!is.matrix(x)) x <- as.matrix(x)
  n <- nrow(x)
  d <- ncol(x)
  prod <- rep(1, times = n)
  for (i in 1:d)
    prod <- prod * (abs(4*x[, i] - 2) + a[i]) / (1 + a[i])
  return(prod)
}

d <- 3
n <- 100
a <- c(0, 0, 99)
reps <- 30

dgp_results <- matrix(NA, nrow = reps, ncol = d+1)
colnames(dgp_results) <- c("seed", paste0("x", 1:d))

for (seed in 1:reps) {
  set.seed(seed)
  x <- randomLHS(n, d)
  y <- gfunc(x, a)

  fit <- fit_two_layer(x, y, nmcmc = 5000, varselect = TRUE)
  fit <- trim(fit, 3000, 2)

  alpha <- n/2
  beta <- fit$tau2_w*n/2
  upper <- qinvgamma(1 - 0.01, shape = alpha, rate = beta)
  meds <- apply(upper, 2, median)
  
  dgp_results[seed, ] <- c(seed, (meds > 1e-4))
  write.csv(dgp_results, paste0("results/dgp_n", n, ".csv"), row.names = FALSE)
}

# Something odd is happening here - why are zero being selected sometimes?
# Because the scale of all the variables is shrinking
# The fits are still good!  The scale is just annoying

d <- 3
n <- 100
set.seed(1)
x <- randomLHS(n, d)
y <- gfunc(x, a)

fit <- fit_two_layer(x, y, nmcmc = 100, varselect = TRUE)
#fit = trim(fit, 3000, 2)
plot(fit)
plot(fit, hidden = TRUE)

matplot(fit$x_grid, t(fit$w_grid[, , 1]), type = "l")

# What happens if we do a normal monowarp fit?
fit <- fit_two_layer(x, y, nmcmc = 5000, monowarp = TRUE)
plot(fit)
plot(fit, hidden = TRUE)
# It figures things out, but doesn't flatten out so badly
# Is there a way to do pmx with monowarp?
# Right now, the implementation doesn't make sense
# The identity on the grid turns into

# NEW IDEA FOR MONOTONIC WARPING
# Take whatever distance it goes down, and just make it go up

# THIS IS AN ISSUE
# Can we fix this by changing the prior?
# Let's change the prior on theta_y (this is easy)

# Current prior:
d <- seq(0, 5, length = 100)
plot(d, dgamma(d, 1.2, 4), type = "l")
lines(d, dgamma(d, 10, 10), col = 2)

# This didn't help at all
# I think the monowarp functionality is the problem

deepgp:::monowarp_ref

fit <- trim(fit, 3000, 2)
fit <- predict(fit, xx)
plot(yy, fit$mean)
abline(0, 1)

# It's figuring out what is going on, but it's not robust enough

# Ok, here's what I want to do
# NOT USE MONOTONIC WARPINGS
# But use axis-aligned warpings where the prior mean is the original input
# This STRONGLY encourages monotonicity (but does not force it)

