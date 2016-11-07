
# Experiment
n <- 200
p <- n - 2
M <- 100
set.seed(123456789)
X <- matrix(rnorm(n * p), nrow = n, ncol = p)
beta <- c(0.5, -0.5, rep(0, p - 2))
rSquared <- adjRSquared <- bic <- aic <- matrix(nrow = M, ncol = p)
pb <- txtProgressBar(style = 3)

# Monte Carlo loop
for (i in 1:M) {
  
  # Generate data
  Y <- 1 + drop(X %*% beta + rnorm(n, sd = 3))
  data <- data.frame(y = Y, X)

  # Loop in p's
  for (j in 1:p) {
    
    mod <- lm(Y ~ X[, 1:j])
    s <- summary(mod)
    rSquared[i, j] <- s$r.squared
    adjRSquared[i, j] <- s$adj.r.squared
    bic[i, j] <- BIC(mod)
    aic[i, j] <- AIC(mod)
    
  }

  # Update pb
  setTxtProgressBar(pb, i / M)
  
}

# Plot R2 and AdjR2
png(filename = "R2vsAdjR2.png", width = 7, height = 7, res = 200, units = "in")
par(mar = c(4, 4, 2, 1) + 0.1, lwd = 2)
matplot(t(rSquared), ylim = c(-1, 1), 
     xlab = "Number of predictors", ylab = "", lwd = 1, 
     col = rgb(0, 0, 0, alpha = 0.1), type = "l", lty = 1)
matlines(t(adjRSquared), lwd = 1, lty = 1, col = rgb(1, 0, 0, alpha = 0.1))
lines(colMeans(rSquared), type = "l", col = "1", lwd = 3)
lines(colMeans(adjRSquared), type = "l", col = "2", lwd = 3)
abline(h = 0, col = gray(0.5))
legend("topleft", legend = expression(R^2, R[Adj]^2), lwd = 2, col = 1:2)
dev.off()

# Plot BIC and AIC
png(filename = "BICandAIC.png", width = 7, height = 7, res = 200, units = "in")
par(mar = c(4, 4, 2, 1) + 0.1, lwd = 2)
matplot(t(bic),
        xlab = "Number of predictors", ylab = "", lwd = 1, 
        col = rgb(0, 0, 0, alpha = 0.1), type = "l", lty = 1)
matlines(t(aic), lwd = 1, lty = 1, col = rgb(1, 0, 0, alpha = 0.1))
lines(colMeans(bic), type = "l", col = "1", lwd = 3)
lines(colMeans(aic), type = "l", col = "2", lwd = 3)
legend("topleft", legend = c("BIC", "AIC"), lwd = 2, col = 1:2)
dev.off()

# Comparison
library(RcmdrMisc)
mod1 <- stepwise(lm(y ~ ., data = data))
mod2 <- stepwise(lm(y ~ ., data = data), "backward")
mod3 <- stepwise(lm(y ~ ., data = data), "forward")
summary(mod1)
summary(mod2)
summary(mod3)
BIC(mod1)
BIC(mod2)
BIC(mod3)
