
# Goal: create some toy data that can simulate the data set-up Laura described
# There are two classes of inputs (s indicates scalar, x indicates curve)
# s1: a scalar value, ranging from [0, 1]
# s2: a scalar value, ranging from [0, 1]
# x: a grid of 20 values which generate a smooth curve 
#    (implicitly, these correspond to known grid points)
# There is one scalar response (y)

library(deepgp)
library(mvtnorm)
set.seed(1)

f <- function (x, s1, s2) {
  term1 <- (s1 + 1) * rowSums(cos(x))
  term2 <- s2 + exp(x[, 1])
  return((term1 + term2 - 1) / 10)
}

grid <- seq(0, 1, length = 20)
n <- 500

x <- rmvnorm(n, sigma = exp(-sq_dist(grid)/0.1))
s1 <- runif(n)
s2 <- runif(n)
y <- f(x, s1, s2)

dat <- data.frame(x, s1, s2, y)
write.csv(dat, paste0("random", n, ".csv"), row.names = FALSE)

#matplot(grid, t(x), type = "l")
#plot(s1, y)
#plot(s2, y)
#hist(y)
