
library(deepgp)
library(mvtnorm)

# Goal: create some toy data that can simulate the data set-up Laura described

# There are two classes of inputs
# First - scalar values
  # x1: a scalar value, ranging from [0, 1]
  # x2: a scalar value, ranging from [0, 1]
# Second - one smooth curve (ncurve = 40, could go up to )

draw_curve <- function(x) {
  K <- exp(-sq_dist(x) / 0.1)
  y <- rmvnorm(1, sigma = K)
}

n <- 40
x <- seq(0, 1, length = n)
y <- draw_curve(x)

plot(x, y)

# Then these inputs go into a function which returns one scalar

f <- function (y, scalar1, scalar2) {
  (scaler1 + 1) * sum(cos(y)) + scalar2
}
