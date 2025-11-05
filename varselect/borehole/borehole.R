
library(deepgp)
library(lhs)

borehole <- function(x) {
  if (!is.matrix(x)) x <- as.matrix(x)
  rw <- x[, 1] * (0.15 - 0.05) + 0.05
  r  <- x[, 2] * (50000 - 100) + 100
  Tu <- x[, 3] * (115600 - 63070) + 63070
  Hu <- x[, 4] * (1110 - 990) + 990
  Tl <- x[, 5] * (116 - 63.1) + 63.1
  Hl <- x[, 6] * (820 - 700) + 700
  L  <- x[, 7] * (1680 - 1120) + 1120
  Kw <- x[, 8] * (15000 - 1500) + 1500
  
  frac1 <- 2 * pi * Tu * (Hu-Hl)
  frac2a <- 2*L*Tu / (log(r/rw)*rw^2*Kw)
  frac2b <- Tu / Tl
  frac2 <- log(r/rw) * (1+frac2a+frac2b)
  y <- frac1 / frac2
  return((y - 59)/49) # about zero mean with unit variance
}

seed <- 2
args <- commandArgs(TRUE)
if(length(args) > 0) 
  for(i in 1:length(args)) 
    eval(parse(text = args[[i]]))
cat("seed is ", seed, "\n")
set.seed(seed)

d <- 10 # 8 meaningful, 2 irrelevant
n <- 200
np <- 1000

x <- randomLHS(n, d) #mofat(d, floor(n/(d+1)))
y <- borehole(x)
xp <- randomLHS(np, d)
yp <- borehole(xp)

# Varselect DGP
fit <- fit_two_layer(x, y, nmcmc = 5000, vecchia = TRUE, monowarp = TRUE, varselect = TRUE)
fit <- trim(fit, 3000, 2)
#plot(fit, hidden = TRUE)
fit <- predict(fit, xp)
r <- read.csv("results/pred_monodgp.csv")
r$RMSE[r$seed == seed] <- rmse(yp, fit$mean)
r$CRPS[r$seed == seed] <- crps(yp, fit$mean, fit$s2)
write.csv(r, "results/pred_monodgp.csv", row.names = FALSE)
wr <- matrix(nrow = fit$nmcmc, ncol = d)
for (i in 1:d) wr[, i] <- apply(fit$w[, , i], 1, max)
write.csv(wr, file = paste0("results/wrange/seed", seed, ".csv"), 
          row.names = F)

