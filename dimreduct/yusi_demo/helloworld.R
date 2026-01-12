n <- 50
x <- matrix(runif(n * 2), ncol = 2)

f <- function(x) {
  x[, 1] + x[, 2]
}

y <- f(x)

library(interp)

i <- interp::interp(x[, 1], x[, 2], y)
image(i)
abline(0, 1)
persp(i)
pairs(cbind(x, y))

library(deepgp)

xp <- matrix(runif(500 * 2), ncol = 2)
yp <- f(xp)

fit <- fit_two_layer(x, y, D = 2, nmcmc = 3000)
fit <- trim(fit, 2000, 2)

plot(fit, hidden = TRUE)

fit <- predict(fit, xp)
plot(fit$mean, yp)
points(fit$mean, yp, col = 2)

j <- 1
i <- interp::interp(x[, 1], x[, 2], fit$w[[j]][, 1])
image(i)

i <- interp::interp(x[, 1], x[, 2], fit$w[[j]][, 2])
image(i)

i <- interp::interp(x[, 1], x[, 2], x[, 1] + x[, 2])
image(i)

