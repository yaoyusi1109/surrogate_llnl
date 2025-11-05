
library(deepgp)
library(MOFAT)

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

for (seed in 2:reps) {

  set.seed(seed)
  x <- randomLHS(n, d) #mofat(d, floor(n/(d+1))) 
  # MOFAT design is way worse for regular DGP and ideal DGP, slightly worse for monoDGP
  y <- gfunc(x, a)
  xp <- randomLHS(np, d)
  yp <- gfunc(xp, a)

  # Fit regular DGP
  fit <- fit_two_layer(x, y, nmcmc = 5000)
  fit <- trim(fit, 3000, 2)
  fit <- predict(fit, xp)
  r <- read.csv("results/pred_dgp_all.csv")
  r$RMSE[r$seed == seed] <- rmse(yp, fit$mean)
  r$CRPS[r$seed == seed] <- crps(yp, fit$mean, fit$s2)
  write.csv(r, "results/pred_dgp_all.csv", row.names = FALSE)

  # Fit regular DGP with only the important inputs
  fit <- fit_two_layer(x[, 1:2], y, nmcmc = 5000)
  fit <- trim(fit, 3000, 2)
  fit <- predict(fit, xp[, 1:2])
  r <- read.csv("results/pred_dgp_ideal.csv")
  r$RMSE[r$seed == seed] <- rmse(yp, fit$mean)
  r$CRPS[r$seed == seed] <- crps(yp, fit$mean, fit$s2)
  write.csv(r, "results/pred_dgp_ideal.csv", row.names = FALSE)
  
  # Fit mono DGP
  fit <- fit_two_layer(x, y, nmcmc = 5000, monowarp = TRUE, varselect = TRUE)
  fit <- trim(fit, 3000, 2)
  fit <- predict(fit, xp)
  r <- read.csv("results/pred_monodgp.csv")
  r$RMSE[r$seed == seed] <- rmse(yp, fit$mean)
  r$CRPS[r$seed == seed] <- crps(yp, fit$mean, fit$s2)
  write.csv(r, "results/pred_monodgp.csv", row.names = FALSE)
  wr <- matrix(nrow = fit$nmcmc, ncol = d)
  for (i in 1:d) wr[, i] <- apply(fit$w[, , i], 1, max)
  write.csv(wr, file = paste0("results/wrange/seed", seed, ".csv"), 
            row.names = F)
}
