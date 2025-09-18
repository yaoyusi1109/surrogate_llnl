
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
n <- 50
a <- c(0, 0, 99)
reps <- 30

for (seed in 31:50) {

  set.seed(seed)
  x <- randomLHS(n, d)
  y <- gfunc(x, a)

  fit <- fit_two_layer(x, y, nmcmc = 5000, monowarp = "axis-aligned")
  fit <- trim(fit, 3000, 2)
  write.csv(fit$tau2_w, file = paste0("tau2_store/n", n, "_seed", seed, ".csv"), 
            row.names = F)
  #alpha <- n/2
  #beta <- fit$tau2_w*n/2
  #upper <- qinvgamma(1 - 0.01, shape = alpha, rate = beta)
  #meds <- apply(upper, 2, median)
  
  #dgp_meds[seed, ] <- c(seed, meds)
  #write.csv(dgp_meds, paste0("results/dgp_n", n, ".csv"), row.names = FALSE)
}

