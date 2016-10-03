
# Generate linear model
set.seed(4567)
n <- 25
x <- rnorm(n, sd = 2)
y <- -0.5 + 0.5 * x + rnorm(n)
mod <- lm(y ~ x)
my <- mean(y)

# Plot
png("anova.png", width = 5, height = 15, res = 200, units = "in")
par(mfrow = c(3, 1))

plot(x, y, xlim = c(-4, 4), ylim = c(-4, 4), pch = 16, main = paste("SST =", round(sum((y - my)^2), 2)))
abline(mod$coefficients, col = 2, lwd = 2)
abline(h = my, col = 4, lty = 2)
segments(x0 = x, y0 = my, x1 = x, y1 = y, col = 4)
legend("topleft", legend = expression("Data", "Fitted line", "Sample mean " * bar(Y), (Y[i] - bar(Y))^2), lwd = 2, col = c(1, 2, 4, 4), lty = c(1, 1, 2, 1))

plot(x, y, xlim = c(-4, 4), ylim = c(-4, 4), pch = 16, main = paste("SSR =", round(sum((mod$fitted.values - my)^2), 2)))
abline(mod$coefficients, col = 2, lwd = 2)
abline(h = my, col = 3, lty = 2)
segments(x0 = x, y0 = my, x1 = x, y1 = mod$fitted.values, col = 3)
points(x, mod$fitted.values, pch = 16, col = 3)
legend("topleft", legend = expression("Data", "Fitted line", "Sample mean " * bar(Y), (hat(Y)[i]- bar(Y))^2), lwd = 2, col = c(1, 2, 3, 3), lty = c(1, 1, 2, 1))

plot(x, y, xlim = c(-4, 4), ylim = c(-4, 4), pch = 16, main = paste("SSE =", round(sum((y - mod$fitted.values)^2), 2)))
abline(mod$coefficients, col = 2, lwd = 2)
segments(x0 = x, y0 = y, x1 = x, y1 = mod$fitted.values, col = 6)
points(x, mod$fitted.values, pch = 16, col = 6)
legend("topleft", legend = expression("Data", "Fitted line", (hat(Y)[i]- Y[i])^2), lwd = 2, col = c(1, 2, 6))

dev.off()
