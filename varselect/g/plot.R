
# What is a good decision rule for our dgp tau2 values?
reps <- 15
upper <- matrix(nrow = reps, ncol = 4)
for (seed in 1:reps) {
  tau2 <- read.csv(paste0("results/tau2/seed", seed, ".csv"))
  upper[seed, ] <- apply(tau2, 2, quantile, p = 0.99)
  #par(mfrow = c(1, 3))
  #for (i in 1:3)
  #  plot(tau2[, i], type = "l", ylim = c(0, 10))
  #Sys.sleep(1)
}

boxplot(upper)
summary(upper)
apply(upper > 0.5, 2, mean)

bk <- read.csv("results/pred_bk.csv")
dgp <- read.csv("results/pred_dgp.csv")
monodgp <- read.csv("results/pred_monodgp.csv")

boxplot(bk$RMSE, dgp$RMSE, monodgp$RMSE, log = "y")
boxplot(bk$CRPS, dgp$CRPS, monodgp$CRPS, log = "y")

bk <- read.csv(paste0("results/bk_n50.csv"))[, -1]
zhang <- read.csv(paste0("results/zhang_n50.csv"))[, -1]
dgp <- (upper > 0.1)

# Proportions of time a variable was selected
results <- data.frame(method = c("Ideal", "Blind Kriging", "Zhang et al.", "DGP"),
                      x1 = NA, x2 = NA, x3 = NA)
results[1, 2:4] <- c(1, 1, 0)
results[2, 2:4] <- apply(bk, 2, mean)
results[3, 2:4] <- apply(zhang, 2, mean)
results[4, 2:4] <- apply(dgp, 2, mean)

knitr::kable(results)
