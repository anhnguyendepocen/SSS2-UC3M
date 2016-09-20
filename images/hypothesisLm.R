

# Sample the predictor
set.seed(345612131)
n <- 100
x <- rnorm(n)
xDisc <- as.numeric(cut(x = x, breaks = 5))
xNoNorm <- c(rnorm(n%/%2, mean = -2, sd = 0.5), 1 + rexp(n%/%2, rate = 1))

# Linear and quadratic response
y <- -0.5 + 1.5 * x
yDisc <- -0.5 + 1.5 * xDisc
yDisp <- -0.5 + 0.5 * x
yNoNorm <- -0.5 + 0.5 * xNoNorm
y2 <- -0.5 + 1.5 * x^2

# Sample 'good' error
eps <- rnorm(n, sd = 0.5)
epsDisp <- rnorm(n, sd = 2)

# Heteroskedastic error
epsNoHomo <- rnorm(n, sd = 0.25 + (x - min(x)))

# Error dependent
epsNoIndep <- sdetorus::rTrajOu(x0 = 0, alpha = 0.1, mu = 0, sigma = 2, N = n - 1, delta = 0.05)
plot(epsNoIndep)

# Non-normal error
epsNoNorm <- rexp(n, rate = 1) - 1

# Valid linear model
png("linearmodelgood.png", width = 10, height = 10, res = 200, units = "in")
par(mfrow = c(2, 2), mar = c(4, 4, 2, 1) + 0.1)
plot(x, y + eps, pch = 16, xlab = "x", ylab = "y", main = "X normal")
abline(lm(I(y + eps) ~ x)$coefficients, col = 2)
plot(xNoNorm, yNoNorm + eps, pch = 16, xlab = "x", ylab = "y", main = "X continuous, non-normal")
abline(lm(I(yNoNorm + eps) ~ xNoNorm)$coefficients, col = 2)
plot(xDisc, yDisc + eps, pch = 16, xlab = "x", ylab = "y", main = "X discrete")
abline(lm(I(yDisc + eps) ~ xDisc)$coefficients, col = 2)
plot(x, yDisp + epsDisp, pch = 16, xlab = "x", ylab = "y", main = "High dispersion")
abline(lm(I(yDisp + epsDisp) ~ x)$coefficients, col = 2)
dev.off()

# Incorrect linear model
png("linearmodelbad.png", width = 10, height = 10, res = 200, units = "in")
par(mfrow = c(2, 2), mar = c(4, 4, 2, 1) + 0.1)
plot(x, y2 + eps, pch = 16, xlab = "x", ylab = "y", main = "No linearity")
abline(lm(I(y2 + eps) ~ x)$coefficients, col = 2)
plot(x, y + epsNoHomo, pch = 16, xlab = "x", ylab = "y", main = "No homcedasticity")
abline(lm(I(y + epsNoHomo) ~ x)$coefficients, col = 2)
plot(x, y + epsNoNorm, pch = 16, xlab = "x", ylab = "y", main = "No normality")
abline(lm(I(y + epsNoNorm) ~ x)$coefficients, col = 2)
plot(x, y + epsNoIndep, pch = 16, xlab = "x", ylab = "y", main = "No independence")
abline(lm(I(y + epsNoIndep) ~ x)$coefficients, col = 2)
dev.off()
