
plot_tau2 <- function(object, p = 0.01) {
  n <- nrow(object$x)
  d <- ncol(object$x)
  alpha <- n/2
  beta <- fit$tau2_w*n/2
  upper <- qinvgamma(1 - p, shape = alpha, rate = beta)
  if (d > 4) {
    par(mfrow = c(2, ceiling(nplots/2)), mar = c(5, 4, 2, 2))
  } else par(mfrow = c(1, d), mar = c(5, 4, 2, 2))
  for (i in 1:d) 
    plot(upper[, i], type = "l", ylim = c(0, max(upper)),
         xlab = "Iteration", ylab = "Upper quantile of tau2",
         main = paste0("Dimension", i))
}

summarize_tau2 <- function(object, p = 0.01) {
  n <- nrow(object$x)
  d <- ncol(object$x)
  alpha <- n/2
  beta <- fit$tau2_w*n/2
  upper <- qinvgamma(1 - p, shape = alpha, rate = beta)
  results <- data.frame("dimension" = 1:d,
                        "tau2_med" = apply(upper, 2, median))
  rownames(results) <- NULL
  return(results[order(results$tau2_med, decreasing = TRUE), ])
}
