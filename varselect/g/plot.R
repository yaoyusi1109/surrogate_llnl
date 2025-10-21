
# What is a good decision rule for our dgp tau2 values? -----------------------

reps <- 50
upper <- matrix(nrow = reps, ncol = 4)
for (seed in 1:reps) {
  wrange <- read.csv(paste0("results/wrange/seed", seed, ".csv"))
  upper[seed, ] <- apply(wrange, 2, quantile, p = 0.99)
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
monodgp <- read.csv("results/pred_monodgp.csv")

col <- RColorBrewer::brewer.pal(5, "Set2")
par(mfrow = c(1, 2))
boxplot(dgp_ideal$RMSE, dgp_all$RMSE, monodgp$RMSE, bk$RMSE, zhang$RMSE, 
        col = col, ylab = "RMSE",
        names = c("DGP Ideal", "DGP All", "monoDGP", "BK", "Zhang"))
abline(v = 1.5, lty = 2, lwd = 2, col = "grey")
boxplot(dgp_ideal$CRPS, dgp_all$CRPS, monodgp$CRPS, bk$CRPS, zhang$CRPS,
        col = col, ylab = "CRPS",
        names = c("DGP Ideal", "DGP All", "monoDGP", "BK", "Zhang"))
abline(v = 1.5, lty = 2, lwd = 2, col = "grey")
