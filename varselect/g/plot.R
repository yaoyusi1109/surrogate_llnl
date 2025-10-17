
# What is a good decision rule for our dgp tau2 values? -----------------------

reps <- 36
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
abline(h = 1, col = 2, lty = 2, lwd = 2)
summary(upper)
apply(upper > 1, 2, mean)

# Proportion of times a variable was selected ---------------------------------

bk <- read.csv(paste0("results/bk_in_out.csv"))[, -1]
zhang <- read.csv(paste0("results/zhang_probs.csv"))[, -1]
zhang <- (zhang > 0.5)
monodgp <- (upper > 1)

results <- data.frame(method = c("Ideal", "Blind Kriging", "Zhang et al.", "monoDGP"),
                      x1 = NA, x2 = NA, x3 = NA, x4 = NA)
results[1, 2:5] <- c(1, 1, 0, 0)
results[2, 2:5] <- apply(bk, 2, mean)
results[3, 2:5] <- apply(zhang, 2, mean)
results[4, 2:5] <- apply(monodgp, 2, mean)

knitr::kable(results)

# Predictive performance ------------------------------------------------------

bk <- read.csv("results/pred_bk.csv")
zhang <- read.csv("results/pred_zhang.csv")
dgp_all <- read.csv("results/pred_dgp_all.csv")
dgp_ideal <- read.csv("results/pred_dgp_ideal.csv")
dgp_ideal_fixed <- read.csv("results/pred_dgp_ideal_fixed.csv")
monodgp <- read.csv("results/pred_monodgp.csv")

par(mfrow = c(1, 2))
boxplot(bk$RMSE, zhang$RMSE, dgp_all$RMSE, dgp_ideal$RMSE, dgp_ideal_fixed$RMSE,
        monodgp$RMSE, log = "y",
        names = c("BK", "Zhang", "DGP All", "DGP Ideal", "DGP Ideal Fixed", "monoDGP"), 
        ylab = "RMSE (log scale)")
boxplot(bk$CRPS, zhang$CRPS, dgp_all$CRPS, dgp_ideal$CRPS, dgp_ideal_fixed$CRPS,
        monodgp$CRPS, log = "y",
        names = c("BK", "Zhang", "DGP All", "DGP Ideal", "DGP Ideal Fixed", "monoDGP"), 
        ylab = "CRPS (log scale)")


