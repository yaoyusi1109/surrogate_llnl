
library(lhs)
library(deepgp)

# Friedman function (Chapter 5 in Surrogates textbook)
f <- function(x) {
  y <- 10*sin(pi*x[,1]*x[,2]) + 20*(x[,3] - 0.5)^2 + 10*x[,4] + 5*x[,5]
  return(y)
}

d <- 8 # last 3 are dummies
n <- 200
np <- 500
x <- matrix(runif(n * d), ncol = d)
y <- f(x)
xp <- randomLHS(np, d)
yp <- f(xp)

# Isotropic GP
gp <- fit_one_layer(x, y, nmcmc = 10000, true_g = 1e-6)
plot(gp$ll, type = "l")
gp <- trim(gp, 5000, 5)
gp <- predict(gp, xp, lite = TRUE)
gp_rmse <- round(rmse(yp, gp$mean), 3)
gp_crps <- round(crps(yp, gp$mean, gp$s2), 3)

# Separable GP
gpsep <- fit_one_layer(x, y, nmcmc = 10000, sep = TRUE, true_g = 1e-6)
plot(gpsep$ll, type = "l")
gpsep <- trim(gpsep, 5000, 5)
gpsep <- predict(gpsep, xp, lite = TRUE)
gpsep_rmse <- round(rmse(yp, gpsep$mean), 3)
gpsep_crps <- round(crps(yp, gpsep$mean, gpsep$s2), 3)

# DGP with 8 nodes
dgp8 <- fit_two_layer(x, y, nmcmc = 20000, D = 8, true_g = 1e-6)
plot(dgp8$ll, type = "l")
dgp8 <- trim(dgp8, 10000, 10)
dgp8 <- predict(dgp8, xp, lite = TRUE)
dgp8_rmse <- round(rmse(yp, dgp8$mean), 3)
dgp8_crps <- round(crps(yp, dgp8$mean, dgp8$s2), 3)

# DGP with 5 nodes
dgp5 <- fit_two_layer(x, y, nmcmc = 20000, D = 5, w_0 = x[, 1:5], true_g = 1e-6)
plot(dgp5$ll, type = "l")
dgp5 <- trim(dgp5, 10000, 10)
dgp5 <- predict(dgp5, xp, lite = TRUE)
dgp5_rmse <- round(rmse(yp, dgp5$mean), 3)
dgp5_crps <- round(crps(yp, dgp5$mean, dgp5$s2), 3)

#pred <- data.frame(y = yp,
#                   gpmean = gp$mean,
#                   gps2 = gp$s2,
#                   gpsepmean = gpsep$mean,
#                   gpseps2 = gpsep$s2,
#                   dgp8mean = dgp8$mean,
#                   dgp8s2 = dgp8$s2,
#                   dgp5mean = dgp5$mean,
#                   dgp5s2 = dgp5$s2)
#write.csv(pred, "pred.csv", row.names = FALSE)
pred <- read.csv("pred.csv")

#results <- data.frame(model = c("gp", "gpsep", "dgp8", "dgp5"),
#                      rmse = c(gp_rmse, gpsep_rmse, dgp8_rmse, dgp5_rmse),
#                      crps = c(gp_crps, gpsep_crps, dgp8_crps, dgp5_crps))
#write.csv(results, "results.csv", row.names = FALSE)
results <- read.csv("results.csv")

par(mfrow = c(2, 2))
plot(yp, pred$gpmean, xlab = "Actual", ylab = "Predicted", 
     main = paste0("Isotropic GP \n RMSE = ", results$rmse[1]))
abline(0, 1, col = 3)
plot(yp, pred$gpsepmean, xlab = "Actual", ylab = "Predicted", 
     main = paste0("Separable GP \n RMSE = ", results$rmse[2]))
abline(0, 1, col = 3)
plot(yp, pred$dgp8mean, xlab = "Actual", ylab = "Predicted", 
     main = paste0("DGP 8 nodes \n RMSE = ", results$rmse[3]))
abline(0, 1, col = 3)
plot(yp, pred$dgp5mean, xlab = "Actual", ylab = "Predicted", 
     main = paste0("DGP 5 nodes \n RMSE = ", results$rmse[4]))
abline(0, 1, col = 3)
