

# Sample the predictor
set.seed(345612131)
n <- 100
x <- rnorm(n)

# Linear and quadratic response
y <- -0.5 + 1.5 * x
y2 <- -0.5 + 1.5 * x^2

# Sample 'good' error
eps <- rnorm(n, sd = 0.5)

# Heteroskedastic error
epsNoHomo <- rnorm(n, sd = 0.75 * (0.25 + x^2))

# Error dependent
epsNoIndep <- sdetorus::rTrajOu(x0 = 0, alpha = 0.1, mu = 0, sigma = 2, N = n - 1, delta = 0.05)
plot(epsNoIndep)

# Non-normal error
epsNoNorm <- rexp(n, rate = 1) - 1

# Linear model
plot(x, y + eps, pch = 16)
abline(a = -0.5, b = 1.5, col = 2)

# Fails linearity
plot(x, y2 + eps, pch = 16)
abline(a = -0.5, b = 1.5, col = 2)

# Fails homocedasticity
plot(x, y + epsNoHomo, pch = 16)
abline(a = -0.5, b = 1.5, col = 2)

# Fails independence
plot(x, y + epsNoIndep, pch = 16)
abline(a = -0.5, b = 1.5, col = 2)

# Fails normality
plot(x, y + epsNoNorm, pch = 16)
abline(a = -0.5, b = 1.5, col = 2)


