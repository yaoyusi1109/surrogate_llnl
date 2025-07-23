
library(deepgp)
library(lhs)

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
  return(y)
}

d <- 9 # one dummy variable in 9th spot
n <- 200
x <- randomLHS(200, d - 1)
x <- cbind(x, runif(n))
y <- borehole(x)

# n = 100 is not enough data to figure things out

fit <- fit_two_layer(x, y, nmcmc = 5000, monowarp = TRUE, swap = TRUE,
                     true_g = 1e-6, pmx = TRUE) # pmx = FALSE gives error???
fit <- trim(fit, 100)
#plot(fit, hidden = TRUE)
#fit <- trim(fit, 3000, 2)

# NEED TO GET VECCHIA IMPLEMENTATION WORKING

alpha <- n/2
beta <- fit$tau2_w*n/2
upper99 <- qinvgamma(0.99, shape = alpha, rate = beta)
par(mfrow = c(3, 3))
for (i in 1:d) plot(upper99[, i], type = "l", ylim = c(0, max(upper99)))

meds <- apply(upper99, 2, median)
order(meds, decreasing = TRUE)
sort(meds, decreasing = TRUE)
