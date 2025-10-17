
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

d <- 4
n <- 50
np <- 500
a <- c(0, 0, 99, 99)
reps <- 50

for (seed in 1:reps) {

  set.seed(seed)
  x <- randomLHS(n, d)
  y <- gfunc(x, a)
  xp <- randomLHS(np, d)
  yp <- gfunc(xp, a)

  # Fit regular DGP
  #fit <- fit_two_layer(x, y, nmcmc = 5000)
  #fit <- trim(fit, 3000, 2)
  #fit <- predict(fit, xp)
  #r <- read.csv("results/pred_dgp_all.csv")
  #r$RMSE[r$seed == seed] <- rmse(yp, fit$mean)
  #r$CRPS[r$seed == seed] <- crps(yp, fit$mean, fit$s2)
  #write.csv(r, "results/pred_dgp_all.csv", row.names = FALSE)

  # Fit regular DGP with only the important inputs
  #fit <- fit_two_layer(x[, 1:2], y, nmcmc = 5000)
  #fit <- trim(fit, 3000, 2)
  #fit <- predict(fit, xp[, 1:2])
  #r <- read.csv("results/pred_dgp_ideal.csv")
  #r$RMSE[r$seed == seed] <- rmse(yp, fit$mean)
  #r$CRPS[r$seed == seed] <- crps(yp, fit$mean, fit$s2)
  #write.csv(r, "results/pred_dgp_ideal.csv", row.names = FALSE)
  
  # Fit mono DGP
  fit <- fit_two_layer(x, y, nmcmc = 10000, monowarp = TRUE)
  fit <- trim(fit, 3000, 2)
  fit <- predict(fit, xp)
  r <- read.csv("results/pred_monodgp.csv")
  r$RMSE[r$seed == seed] <- rmse(yp, fit$mean)
  r$CRPS[r$seed == seed] <- crps(yp, fit$mean, fit$s2)
  write.csv(r, "results/pred_monodgp.csv", row.names = FALSE)
  write.csv(fit$tau2_w, file = paste0("results/tau2/seed", seed, ".csv"), 
            row.names = F)
}

alpha = 1.2
beta = 0.2

x = seq(0.01, 10, length = 100)
plot(x, dgamma(x, alpha, beta), type = "l")

qgamma(0.95, alpha, beta)
