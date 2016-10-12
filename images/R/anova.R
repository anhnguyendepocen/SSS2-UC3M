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

plot(x, y, xlim = c(-4, 4), ylim = c(-4, 4), pch = 16, type = "n",
     main = paste("SST =", round(sum((y - my)^2), 2)), col.main = 4)
abline(mod$coefficients, col = 2, lwd = 2)
abline(h = my, col = 2, lty = 2)
segments(x0 = x, y0 = my, x1 = x, y1 = y, col = 4, lwd = 2)
legend("topleft", legend = expression("Fitted line", "Sample mean " * bar(Y),
                                      (Y[i] - bar(Y))^2), lwd = c(2, 1, 2),
       col = c(2, 2, 4), lty = c(1, 2, 1))
points(x, y, pch = 16)

plot(x, y, xlim = c(-4, 4), ylim = c(-4, 4), pch = 16, type = "n",
     main = paste("SSR =", round(sum((mod$fitted.values - my)^2), 2)),
     col.main = "forestgreen")
abline(mod$coefficients, col = 2, lwd = 2)
abline(h = my, col = 2, lty = 2)
segments(x0 = x, y0 = my, x1 = x, y1 = mod$fitted.values, col = "forestgreen",
         lwd = 2)
points(x, mod$fitted.values, pch = 16, col = "forestgreen")
legend("topleft", legend = expression("Fitted line", "Sample mean " * bar(Y),
                                      (hat(Y)[i] - bar(Y))^2), lwd = c(2, 1, 2),
       col = c(2, 2, "forestgreen"), lty = c(1, 2, 1))
points(x, y, pch = 16)

plot(x, y, xlim = c(-4, 4), ylim = c(-4, 4), pch = 16, type = "n",
     main = paste("SSE =", round(sum((y - mod$fitted.values)^2), 2)),
     col.main = "orange")
abline(mod$coefficients, col = 2, lwd = 2)
segments(x0 = x, y0 = y, x1 = x, y1 = mod$fitted.values, col = "orange", lwd = 2)
points(x, mod$fitted.values, pch = 16, col = "orange")
legend("topleft", legend = expression("Fitted line", (hat(Y)[i] - Y[i])^2),
       lwd = 2, col = c(2, "orange"))
points(x, y, pch = 16)

dev.off()
