
library(plot3D)

# Add an alpha value to a colour
addAlpha <- function(col, alpha = 1) {

  if (missing(col)) stop("Please provide a vector of colours.")
  apply(sapply(col, col2rgb) / 255, 2,
        function(x) rgb(x[1], x[2], x[3], alpha = alpha))

}

# Generate linear model
set.seed(4567)
n <- 25
X1 <- rnorm(n, sd = 1.5)
X2 <- rnorm(n)
Y <- 1 + 2 * X1 - X2 + rnorm(n, sd = 2)
data <- data.frame(X = X1, Y = X2, Z = Y)
my <- mean(Y)

# Parameters
nGrid <- 5
theta <- -25
phi <- 10

# Estimate lm
mod <- lm(Z ~ X + Y, data = data)

# Create and plot initial XYZ-grid
sdX <- sd(data$X)
sdY <- sd(data$Y)
sdZ <- sd(data$Z)
gX <- seq(min(data$X) - sdX, max(data$X) + sdX, length = nGrid)
gY <- seq(min(data$Y) - sdY, max(data$Y) + sdY, length = nGrid)
gZ <- seq(min(data$Z) - sdZ, max(data$Z) + sdZ, length = nGrid)

# Compute regression surface
x <- pretty(gX, nGrid)
y <- pretty(gY, nGrid)
x <- c(gX[1], x[x > gX[1] & x < gX[nGrid]], gX[nGrid])
y <- c(gY[1], y[y > gY[1] & y < gY[nGrid]], gY[nGrid])
lx <- length(x)
ly <- length(y)
xy <- expand.grid(x = x, y = y)
new <- data.frame(X = xy$x, Y = xy$y)
pred <- predict(mod, newdata = new, type = "response")
gZ <- seq(min(c(gZ, pred)), max(c(gZ, pred)), length = nGrid)
M <- mesh(x, y)

# Plot data
plotData <- function(...) {

  gridMat <- scatter3D(data$X, data$Y, data$Z, pch = 16, theta = theta, phi = phi,
                       bty = "g", axes = FALSE, colkey = FALSE, col = 1,
                       xlim = range(gX), ylim = range(gY), zlim = range(gZ),
                       nticks = nGrid, cex = 0.75, ...)
  text(x = trans3d(median(gX), gY[1], gZ[1], gridMat), labels = "x1", pos = 1)
  text(x = trans3d(gX[1], median(gY), gZ[1], gridMat), labels = "x2", pos = 2)
  text(x = trans3d(gX[1], gY[nGrid], median(gZ), gridMat), labels = "y", pos = 2)

}

# Plot
png("anova3D.png", width = 5, height = 15, res = 200, units = "in")
par(mfrow = c(3, 1))

plotData(main = paste("SST =", round(sum((Y - my)^2), 2)), col.main = 4)
segments3D(x0 = data$X, y0 = data$Y, z0 = data$Z, x1 = data$X, y1 = data$Y,
           z1 = rep(my, n), col = 4, lwd = 2, add = TRUE)
surf3D(x = M$x, y = M$y, z = matrix(rnorm(lx * ly, mean = my, sd = 1e-6),
                                    nrow = lx, ncol = ly),
       col = addAlpha("red", 0), border = addAlpha("red", 0.5), lty = 2,
       add = TRUE)
surf3D(x = M$x, y = M$y, z = matrix(pred, nrow = lx, ncol = ly),
       col = addAlpha("red", 0.1), border = addAlpha("red", 0.2), add = TRUE)
legend("bottomright", legend = expression("Fitted plane", "Sample mean " * bar(Y),
                                      (Y[i] - bar(Y))^2), lwd = c(2, 1, 2),
       col = c(2, 2, 4), lty = c(1, 2, 1))

plotData(main = paste("SSR =", round(sum((mod$fitted.values - my)^2), 2)),
         col.main = "forestgreen")
segments3D(x0 = data$X, y0 = data$Y, z0 = rep(my, n), x1 = data$X, y1 = data$Y,
           z1 = mod$fitted.values, col = "forestgreen", lwd = 2, add = TRUE)
points3D(data$X, data$Y, mod$fitted.values, pch = 16, add = TRUE,
         col = "forestgreen")
surf3D(x = M$x, y = M$y, z = matrix(rnorm(lx * ly, mean = my, sd = 1e-6),
                                    nrow = lx, ncol = ly),
       col = addAlpha("red", 0), border = addAlpha("red", 0.5), lty = 2,
       add = TRUE)
surf3D(x = M$x, y = M$y, z = matrix(pred, nrow = lx, ncol = ly),
       col = addAlpha("red", 0.1), border = addAlpha("red", 0.2), add = TRUE)
legend("bottomright", legend = expression("Fitted plane", "Sample mean " * bar(Y),
                                      (hat(Y)[i] - bar(Y))^2), lwd = c(2, 1, 2),
       col = c(2, 2, "forestgreen"), lty = c(1, 2, 1))

plotData(main = paste("SSE =", round(sum((Y - mod$fitted.values)^2), 2)),
         col.main = "orange")
segments3D(x0 = data$X, y0 = data$Y, z0 = data$Z, x1 = data$X, y1 = data$Y,
           z1 = mod$fitted.values, col = "orange", lwd = 2, add = TRUE)
points3D(data$X, data$Y, mod$fitted.values, pch = 16, add = TRUE,
         col = "orange")
surf3D(x = M$x, y = M$y, z = matrix(pred, nrow = lx, ncol = ly),
       col = addAlpha("red", 0.1), border = addAlpha("red", 0.2), add = TRUE)
legend("bottomright", legend = expression("Fitted plane", (hat(Y)[i] - Y[i])^2),
       lwd = 2, col = c(2, "orange"))

dev.off()
