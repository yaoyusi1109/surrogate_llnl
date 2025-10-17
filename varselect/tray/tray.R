
library(deepgp)
library(lhs)

tray <- function(x) {
  x <- x[, 1:2] # only first two inputs are relevant
  x <- x*4 - 2
  p1 <- abs(100 - sqrt(apply(x^2, 1, sum)) / pi)
  p2 <- abs(apply(sin(x), 1, prod) * exp(p1)) + 1
  y <- -0.0001 * (p2)^(0.1)
  return((y + 1.9) / 0.2)
}

d <- 5 # 2 important, 3 dummy
n <- 100
np <- 500
reps <- 50

for (seed in 1:reps) {

  set.seed(seed)
  x <- randomLHS(n, d)
  y <- tray(x)
  xp <- randomLHS(np, d)
  yp <- tray(xp)

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
  fit <- fit_two_layer(x, y, nmcmc = 5000, monowarp = TRUE)
  fit <- trim(fit, 3000, 2)
  fit <- predict(fit, xp)
  r <- read.csv("results/pred_monodgp.csv")
  r$RMSE[r$seed == seed] <- rmse(yp, fit$mean)
  r$CRPS[r$seed == seed] <- crps(yp, fit$mean, fit$s2)
  write.csv(r, "results/pred_monodgp.csv", row.names = FALSE)
  write.csv(fit$tau2_w, file = paste0("results/tau2/seed", seed, ".csv"), 
            row.names = F)
  write.csv(fit$theta_w, file = paste0("results/theta/seed", seed, ".csv"), 
            row.names = F)
}

#file <- "results/pred_monodgp.csv"
#results <- data.frame(seed = 1:reps, RMSE = NA, CRPS = NA)
#write.csv(results, file, row.names = FALSE)
