
# What is a good decision rule for our dgp tau2 values?
upper <- matrix(nrow = 30, ncol = 3)
for (seed in 1:30) {
  tau2 <- read.csv(paste0("tau2_store/n50_seed", seed, ".csv"))
  upper[seed, ] <- apply(tau2, 2, quantile, p = 0.99)
  par(mfrow = c(1, 3))
  #for (i in 1:3)
  #  plot(dgp[, i], type = "l", ylim = c(0, 10))
  #Sys.sleep(1)
}

upper
boxplot(upper)
summary(upper)

bk <- read.csv(paste0("results/bk_n50.csv"))[, -1]
zhang <- read.csv(paste0("results/zhang_n50.csv"))[, -1]
dgp <- (upper > 0.1)

# Proportions of time a variable was selected
cat("BK = ", apply(bk, 2, mean), "\n")
cat("Zhang = ", apply(zhang, 2, mean), "\n")
cat("DGP = ", apply(dgp, 2, mean), "\n")
