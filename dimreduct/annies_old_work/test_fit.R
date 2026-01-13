
# Goal: test some GP and DGP fits to the toy data
# How does the DGP do?  Can it dimension reduct?

library(deepgp)
set.seed(1)

dat <- read.csv("random500.csv")
x <- as.matrix(dat[, -ncol(dat)])
y <- dat[, ncol(dat)]

# Train/test split
train <- sample(1:nrow(dat), 200, replace = FALSE)
xtrain <- x[train, ]
ytrain <- y[train]
xtest <- x[-train, ]
ytest <- y[-train]

# Isotropic GP
gp <- fit_one_layer(xtrain, ytrain, nmcmc = 10000, sep = FALSE, true_g = 1e-6)
plot(gp$ll, type = "l")
gp <- trim(gp, 5000, 5)
gp <- predict(gp, xtest, lite = TRUE)
gp_rmse <- round(rmse(ytest, gp$mean), 3)
gp_crps <- round(crps(ytest, gp$mean, gp$s2), 3)

# Separable GP
gpsep <- fit_one_layer(xtrain, ytrain, nmcmc = 10000, sep = TRUE, true_g = 1e-6)
plot(gpsep$ll, type = "l") # did not get many acceptances, sticky chain
gpsep <- trim(gpsep, 5000, 5)
gpsep <- predict(gpsep, xtest, lite = TRUE)
gpsep_rmse <- round(rmse(ytest, gpsep$mean), 3)
gpsep_crps <- round(crps(ytest, gpsep$mean, gpsep$s2), 3)

# Two-layer DGP with D = 8
dgp8 <- fit_two_layer(xtrain, ytrain, D = 8, nmcmc = 20000, true_g = 1e-6)
dgp8 <- continue(dgp8, 10000)
plot(dgp8$ll, type = "l")
dgp8 <- trim(dgp8, 20000, 10)
dgp8 <- predict(dgp8, xtest, lite = TRUE)
dgp8_rmse <- round(rmse(ytest, dgp8$mean), 3)
dgp8_crps <- round(crps(ytest, dgp8$mean, dgp8$s2), 3)

# Two-layer DGP with D = 4
dgp4 <- fit_two_layer(xtrain, ytrain, D = 4, nmcmc = 30000, true_g = 1e-6)
dgp4 <- continue(dgp4, 10000)
plot(dgp4$ll, type = "l")
dgp4 <- trim(dgp4, 20000, 20)
dgp4 <- predict(dgp4, xtest, lite = TRUE)
dgp4_rmse <- round(rmse(ytest, dgp4$mean), 3)
dgp4_crps <- round(crps(ytest, dgp4$mean, dgp4$s2), 3)

# Two-layer DGP with D = 2
dgp2 <- fit_two_layer(xtrain, ytrain, D = 2, nmcmc = 30000, true_g = 1e-6)
dgp2 <- continue(dgp2, 10000)
dgp2 <- continue(dgp2, 10000)
dgp2 <- continue(dgp2, 10000)
plot(dgp2$ll, type = "l")
dgp2 <- trim(dgp2, 50000, 10)
dgp2 <- predict(dgp2, xtest, lite = TRUE)
dgp2_rmse <- round(rmse(ytest, dgp2$mean), 3)
dgp2_crps <- round(crps(ytest, dgp2$mean, dgp2$s2), 3)

# DGP cheat (initialize at the 4 known latent quantities)
w_0 <- cbind(xtrain[, 21], rowSums(cos(xtrain[, 1:20])), xtrain[, 22], 
             exp(xtrain[, 1]))
dgp_cheat <- fit_two_layer(xtrain, ytrain, D = 4, w_0 = w_0, nmcmc = 10000, 
                           true_g = 1e-6)
dgp_cheat <- continue(dgp_cheat, 10000)
dgp_cheat <- continue(dgp_cheat, 10000)
dgp_cheat <- continue(dgp_cheat, 10000)
plot(dgp_cheat$ll, type = "l")
dgp_cheat <- trim(dgp_cheat, 20000, 20)
dgp_cheat <- predict(dgp_cheat, xtest, lite = TRUE)
dgp_cheat_rmse <- round(rmse(ytest, dgp_cheat$mean), 3)
dgp_cheat_crps <- round(crps(ytest, dgp_cheat$mean, dgp_cheat$s2), 3)

#pred <- data.frame(y = ytest,
#                   gpmean = gp$mean,
#                   gps2 = gp$s2,
#                   gpsepmean = gpsep$mean,
#                   gpseps2 = gpsep$s2,
#                   dgp8mean = dgp8$mean,
#                   dgp8s2 = dgp8$s2,
#                   dgp4mean = dgp4$mean,
#                   dgp4s2 = dgp4$s2,
#                   dgp2mean = dgp2$mean,
#                   dgp2s2 = dgp2$s2)
pred <- read.csv("pred.csv")
#pred$dgpcheatmean <- dgp_cheat$mean
#pred$dgpcheats2 <- dgp_cheat$s2
#write.csv(pred, "pred.csv", row.names = FALSE)

#results <- data.frame(model = c("gp", "gpsep", "dgp8", "dgp4", "dgp2"),
#                      rmse = c(gp_rmse, gpsep_rmse, dgp8_rmse, dgp4_rmse, dgp2_rmse),
#                      crps = c(gp_crps, gpsep_crps, dgp8_crps, dgp4_crps, dgp2_crps))
results <- read.csv("results.csv")
#results <- rbind(results, c("dgpcheat", dgp_cheat_rmse, dgp_cheat_crps))
#write.csv(results, "results.csv", row.names = FALSE)

par(mfrow = c(2, 3))
plot(ytest, pred$gpmean, main = "Isotropic GP", xlab = "Actual", ylab = "Predicted")
abline(0, 1, col = 2)
plot(ytest, pred$gpsepmean, main = "Separable GP", xlab = "Actual", ylab = "Predicted")
abline(0, 1, col = 2)
plot(ytest, pred$dgp8mean, main = "DGP 8 nodes", xlab = "Actual", ylab = "Predicted")
abline(0, 1, col = 2)
plot(ytest, pred$dgp4mean, main = "DGP 4 nodes", xlab = "Actual", ylab = "Predicted")
abline(0, 1, col = 2)
plot(ytest, pred$dgp2mean, main = "DGP 2 nodes", xlab = "Actual", ylab = "Predicted")
abline(0, 1, col = 2)
plot(ytest, pred$dgpcheatmean, main = "DGP 4 nodes (cheat)", xlab = "Actual", ylab = "Predicted")
abline(0, 1, col = 2)
