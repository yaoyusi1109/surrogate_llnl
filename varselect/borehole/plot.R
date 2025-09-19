
# What is a good decision rule for our dgp tau2 values? -----------------------

d <- 10
reps <- 10
upper <- matrix(nrow = reps, ncol = d)
for (seed in 2:reps) {
  tau2 <- read.csv(paste0("results/tau2/seed", seed, ".csv"))
  upper[seed, ] <- apply(tau2, 2, quantile, p = 0.99)
  #par(mfrow = c(1, 3))
  #for (i in 1:3)
  #  plot(tau2[, i], type = "l", ylim = c(0, 10))
  #Sys.sleep(1)
}

boxplot(upper)
boxplot(upper, ylim = c(0, 0.5))
summary(upper)
apply(upper > 0.1, 2, mean, na.rm = TRUE)

# Proportion of times a variable was selected ---------------------------------

bk <- read.csv(paste0("results/bk_in_out.csv"))[, -1]
#zhang <- read.csv(paste0("results/zhang_probs.csv"))[, -1]
#zhang <- (zhang > 0.5)
dgp <- (upper > 0.1)

results <- data.frame(method = c("Ideal", "Blind Kriging", "monoDGP"),
                      x1 = NA, x2 = NA, x3 = NA, x4 = NA, x5 = NA,
                      x6 = NA, x7 = NA, x8 = NA, x9 = NA, x10 = NA)
results[1, -1] <- c(rep(1, 8), rep(0, 2))
results[2, -1] <- apply(bk, 2, mean, na.rm = TRUE)
results[3, -1] <- apply(dgp, 2, mean, na.rm = TRUE)

knitr::kable(results)

# Predictive performance ------------------------------------------------------

bk <- read.csv("results/pred_bk.csv")[1:10, ]
dgp <- read.csv("results/pred_dgp.csv")[1:10, ]
monodgp <- read.csv("results/pred_monodgp.csv")[1:10, ]

par(mfrow = c(1, 2))
boxplot(bk$RMSE, dgp$RMSE, monodgp$RMSE, log = "y", 
        names = c("BK", "DGP", "monoDGP"), ylab = "RMSE (log scale)")
boxplot(bk$CRPS, dgp$CRPS, monodgp$CRPS, log = "y",
        names = c("BK", "DGP", "monoDGP"), ylab = "CRPS (log scale)")


