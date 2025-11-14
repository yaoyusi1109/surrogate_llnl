
library(deepgp)
library(lhs)

ignition <- function(x) {
  r <- sqrt(rowSums(x[, 1:10]^2))
  t <- 200000*pnorm(sqrt(2)*10*(r - 2))
  y <- log10(r^5*(1 + t))
  return((y - 3.9)/2.5)
}

d <- 13 # 10 important, 3 dummy
n <- 200
np <- 1000
reps <- 50

for (seed in 31:50) {

  set.seed(seed)
  x <- randomLHS(n, d)
  y <- ignition(x)
  xp <- randomLHS(np, d)
  yp <- ignition(xp)
  
  # Fit mono DGP
  fit <- fit_two_layer(x, y, nmcmc = 10000, monowarp = TRUE, varselect = TRUE,
                       vecchia = TRUE)
  fit <- trim(fit, 8000, 2)
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

#reps <- 50
#setwd("~/llnl-dgp-git/varselect/ignition/")
#file <- "results/zhang_probs.csv"
#results <- data.frame(seed = 1:reps, x1 = NA, x2 = NA, x3 = NA, x4 = NA, x5 = NA,
#                     x6 = NA, x7 = NA, x8 = NA, x9 = NA, x10 = NA,
#                     x11 = NA, x12 = NA, x13 = NA)
#results <- data.frame(seed = 1:reps, RMSE = NA, CRPS = NA)
#write.csv(results, file, row.names = FALSE)
