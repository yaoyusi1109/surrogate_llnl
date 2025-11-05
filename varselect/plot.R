
func <- "borehole"

if (func == "g") {
  d <- 4
  ideal <- c(1, 1, 0, 0)
} else if (func == "tray") {
  d <- 5
  ideal <- c(1, 1, 0, 0, 0)
} else if (func == "borehole") {
  d <- 10
  ideal <- c(1, 1, 1, 1, 1, 1, 1, 1, 0, 0)
}

# What is a good decision rule for our dgp tau2 values? -----------------------

reps <- 2
upper <- matrix(nrow = reps, ncol = d)
for (seed in 1:reps) {
  wrange <- read.csv(paste0(func, "/results/wrange/seed", seed, ".csv"))
  upper[seed, ] <- apply(wrange/apply(wrange, 1, max), 2, quantile, p = 0.99)
}

boxplot(upper)
abline(h = 0.2, col = 2, lty = 2, lwd = 2)

# Proportion of times a variable was selected ---------------------------------

bk <- read.csv(paste0(func, "/results/bk_in_out.csv"))[, -1]
zhang <- read.csv(paste0(func, "/results/zhang_probs.csv"))[, -1]
zhang <- (zhang > 0.5)
monodgp <- (upper > 0.2)

results <- data.frame(matrix(NA, nrow = 4, ncol = d))
colnames(results) <- paste0("x", 1:d)
rownames(results) <- c("Ideal", "Blind Kriging", "Zhang", "monoDGP")
results[1, ] <- ideal
results[2, ] <- apply(bk, 2, mean, na.rm = TRUE)
results[3, ] <- apply(zhang, 2, mean, na.rm = TRUE)
results[4, ] <- apply(monodgp, 2, mean)

knitr::kable(results)

# Predictive performance ------------------------------------------------------

names <- c("BK", "Zhang", "monoDGP", "DGP-All", "DGP-Ideal")

bk <- read.csv(paste0(func, "/results/pred_bk.csv"))
zhang <- read.csv(paste0(func, "/results/pred_zhang.csv"))
monodgp <- read.csv(paste0(func, "/results/pred_monodgp.csv"))
RMSE <- cbind(bk$RMSE, zhang$RMSE, monodgp$RMSE)
CRPS <- cbind(bk$CRPS, zhang$CRPS, monodgp$CRPS)
if (file.exists(paste0(func, "/results/pred_dgp_all.csv"))) {
  dgp_all <- read.csv(paste0(func, "/results/pred_dgp_all.csv"))
  RMSE <- cbind(RMSE, dgp_all$RMSE)
  CRPS <- cbind(CRPS, dgp_all$CRPS)
} else names <- names[names != "DGP-All"]
if (file.exists(paste0(func, "/results/pred_dgp_ideal.csv"))) {
  dgp_ideal <- read.csv(paste0(func, "/results/pred_dgp_ideal.csv"))
  RMSE <- cbind(RMSE, dgp_ideal$RMSE)
  CRPS <- cbind(CRPS, dgp_ideal$CRPS)
} else names <- names[names != "DGP-Ideal"]

col <- RColorBrewer::brewer.pal(5, "Set2")
par(mfrow = c(1, 2))
boxplot(RMSE, col = col, ylab = "RMSE", names = names)
boxplot(CRPS, col = col, ylab = "CRPS", names = names)
