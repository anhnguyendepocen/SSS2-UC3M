

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
par(mfrow = c(2, 2), mar = c(4, 4, 2, 1) + 0.1, lwd = 2)
plot(x, y + eps, pch = 16, xlab = "x", ylab = "y", main = "X normal")
abline(lm(I(y + eps) ~ x)$coefficients, col = 2, lwd = 2)
plot(xNoNorm, yNoNorm + eps, pch = 16, xlab = "x", ylab = "y", main = "X continuous, non-normal")
abline(lm(I(yNoNorm + eps) ~ xNoNorm)$coefficients, col = 2, lwd = 2)
plot(xDisc, yDisc + eps, pch = 16, xlab = "x", ylab = "y", main = "X discrete")
abline(lm(I(yDisc + eps) ~ xDisc)$coefficients, col = 2, lwd = 2)
plot(x, yDisp + epsDisp, pch = 16, xlab = "x", ylab = "y", main = "High dispersion")
abline(lm(I(yDisp + epsDisp) ~ x)$coefficients, col = 2, lwd = 2)
dev.off()

# Incorrect linear model
png("linearmodelbad.png", width = 10, height = 10, res = 200, units = "in")
par(mfrow = c(2, 2), mar = c(4, 4, 2, 1) + 0.1)
plot(x, y2 + eps, pch = 16, xlab = "x", ylab = "y", main = "No linearity")
abline(lm(I(y2 + eps) ~ x)$coefficients, col = 2, lwd = 2)
plot(x, y + epsNoHomo, pch = 16, xlab = "x", ylab = "y", main = "No homcedasticity")
abline(lm(I(y + epsNoHomo) ~ x)$coefficients, col = 2, lwd = 2)
plot(x, y + epsNoNorm, pch = 16, xlab = "x", ylab = "y", main = "No normality")
abline(lm(I(y + epsNoNorm) ~ x)$coefficients, col = 2, lwd = 2)
plot(x, y + epsNoIndep, pch = 16, xlab = "x", ylab = "y", main = "No independence")
abline(lm(I(y + epsNoIndep) ~ x)$coefficients, col = 2, lwd = 2)
dev.off()

# Quiz linear model
png("linearmodelquiz.png", width = 10, height = 10, res = 200, units = "in")
set.seed(223971)
par(mfrow = c(3, 3), mar = c(4, 4, 2, 1) + 0.1)

# OK
x1 <- rexp(200)
eps <- rnorm(200)
y1 <- -0.5 * x1 + eps
plot(x1, y1, pch = 16, xlab = "x", ylab = "y", main = "1")
abline(lm(y1 ~ x1)$coefficients, col = 2, lwd = 2)

# Nonlinear
x2 <- runif(200)
eps <- rnorm(200)
y2 <- 1 - log(0.5 * x2) + eps
plot(x2, y2, pch = 16, xlab = "x", ylab = "y", main = "2")
abline(lm(y2 ~ x2)$coefficients, col = 2, lwd = 2)

# Heteroskedastic
x3 <- c(rpois(100, 5), rnorm(100))
y3 <- 0.5 + 0.5 * x3 + 0.5 * sqrt(abs(x3)) * eps
plot(x3, y3, pch = 16, xlab = "x", ylab = "y", main = "3")
abline(lm(y3 ~ x3)$coefficients, col = 2, lwd = 2)

# Non-normal
x4 <- rnorm(200)
epst <- rt(200, 2)
y4 <- 0.5 * x4 + epst
plot(x4, y4, pch = 16, xlab = "x", ylab = "y", main = "4")
abline(lm(y4 ~ x4)$coefficients, col = 2, lwd = 2)

# Dependent
x5 <- rnorm(200)
epsNoIndep <- sdetorus::rTrajOu(x0 = 0, alpha = 0.01, mu = 0, sigma = 0.5, N = 200 - 1, delta = 0.01)
y5 <- 1 - 0.5 * x5 + epsNoIndep
plot(x5, y5, pch = 16, xlab = "x", ylab = "y", main = "5")
abline(lm(y5 ~ x5)$coefficients, col = 2, lwd = 2)

# Non-normal
x6 <- rnorm(200, sd = 0.25)
eps <- rpois(200, 3) - 3
y6 <- 1 + x6 + eps
plot(x6, y6, pch = 16, xlab = "x", ylab = "y", main = "6")
abline(lm(y6 ~ x6)$coefficients, col = 2, lwd = 2)

# OK
x7 <- rnbinom(200, 10, 0.5)
y7 <- -0.1 * x7 + rnorm(100)
plot(x7, y7, pch = 16, xlab = "x", ylab = "y", main = "7")
abline(lm(y7 ~ x7)$coefficients, col = 2, lwd = 2)

# Nonlinear, heteroskedastic
x8 <- rnorm(200, sd = 2)
y8 <- -1 + x8^3 + 5 * (x8 - min(x8)) * rnorm(200, sd = 2)
plot(x8, y8, pch = 16, xlab = "x", ylab = "y", main = "8")
abline(lm(y8 ~ x8)$coefficients, col = 2, lwd = 2)

# Non linear, heteroskedastic
x9 <- rt(200, 10)
y9 <- -2 - 0.5 * log(abs(x9)) + rnorm(200)
plot(x9, y9, pch = 16, xlab = "x", ylab = "y", main = "9")
abline(lm(y9 ~ x9)$coefficients, col = 2, lwd = 2)

dev.off()

# Save RData
assumptions <- data.frame(x1, x2, x3, x4, x5, x6, x7, x8, x9,
                          y1, y2, y3, y4, y5, y6, y7, y8, y9)
save(assumptions, file = "assumptions.RData")


# t
png("ttest.png", width = 7, height = 7, res = 200, units = "in")
xx <- seq(-7, 7, l = 500)
plot(xx, dt(x = xx, df = 198), type = "l", xlab = "x", ylab = "Density of the Student's t with 198 df", lwd = 2)
q1 <- qt(p = 0.975, df = 198, lower.tail = FALSE)
q2 <- qt(p = 0.025, df = 198, lower.tail = FALSE)
xvals1 <- seq(q1, q2, length = 200)
dvals1 <- dt(x = xvals1, df = 198)
xvals2 <- seq(xx[1], q1, length = 200)
dvals2 <- dt(x = xvals2, df = 198)
xvals3 <- seq(q2, xx[length(xx)], length = 200)
dvals3 <- dt(x = xvals3, df = 198)
polygon(c(xvals1, rev(xvals1)), c(rep(0, 200), rev(dvals1)), col = "lightblue")
polygon(c(xvals2, rev(xvals2)), c(rep(0, 200), rev(dvals2)), col = "lightgreen")
polygon(c(xvals3, rev(xvals3)), c(rep(0, 200), rev(dvals3)), col = "lightgreen")
text(x = q2, y = dvals1[1], labels = expression(t[list(n - 2, alpha/2)]), pos = 4)
text(x = q1, y = dvals1[1], labels = expression(-t[list(n - 2, alpha/2)]), pos = 2)
text(x = 0, y = 0.1, labels = expression(1 - alpha), pos = 3)
text(x = q1 + 0.3, y = 0.007, labels = expression(alpha / 2), pos = 2)
text(x = q2 - 0.3, y = 0.007, labels = expression(alpha / 2), pos = 4)
rug(-0.353, col = 2, lwd = 2)
rug(-6.170, col = 6, lwd = 2)
legend("topright", legend = expression(100 * (1 - alpha) * "% CI of the " * t[n - 2], "Tails with " * alpha/2 * " probability", t * "-statistic for " * beta[0], t * "-statistic for " * beta[1]), col = c("lightblue", "lightgreen", 2, 6), lwd = 2)
dev.off()
