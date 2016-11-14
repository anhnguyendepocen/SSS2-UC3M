

#' @title Visualize the fit of a simple linear model
#'
#' @description Generates a plot illustrating the key concepts for a fitted simple linear model: linear trend, model-based confidence bands and normality around the mean.
#'
#' @param data a \code{data.frame} containing \code{X} and \code{Y} as variables.
#' @param nGrid number of cuts used to produce the XY grid.
#' @param zTop upper z-limit of the 3D bounding box.
#' @inheritParams graphics::persp
#' @param alpha desired level for the theoretical confidence intervals. If set to \code{0}, no confidence intervals are plotted.
#' @return
#' Nothing. The function is called to produce a plot.
#' @examples
#' # Generate data
#' X <- rnorm(100)
#' Y <- 0.5 + 1.5 * X + rnorm(100)
#' data <- data.frame(X = X, Y = Y)
#'
#' par(mar = rep(0, 4), oma = rep(0, 4))
#' visualizeFitLm(data = data)
#' visualizeFitLm(data = data, alpha = 0)
#' @author Eduardo García-Portugués (\email{edgarcia@est-econ.uc3m.es}), based on the original code from Arthur Charpentier (\url{http://freakonometrics.hypotheses.org/9593}).
#' @export
visualizeFitLm <- function(data, nGrid = 6, zTop = 0.5, theta = -30, phi = 20,
                           alpha = 0.05) {

  # Estimate lm
  n <- length(data$X)
  mod <- lm(Y ~ X, data = data)

  # Create and plot initial XY-grid
  sdX <- sd(data$X)
  sdY <- sd(data$Y)
  gX <- seq(min(data$X) - sdX, max(data$X) + sdX, length = nGrid)
  gY <- seq(min(data$Y) - sdY, max(data$Y) + sdY, length = nGrid)
  gridMat <- persp(gX, gY, matrix(0, nGrid, nGrid), zlim = c(0, zTop),
                   theta = theta, phi = phi, box = FALSE, border = gray(0.75))
  text(x = trans3d(median(gX), min(gY), 0, gridMat), labels = "x", pos = 1)
  text(x = trans3d(min(gX), median(gY), 0, gridMat), labels = "y", pos = 2)

  # Compute regression curve
  lx <- 501
  x <- seq(gX[1], gX[nGrid], length = lx)
  zeros <- rep(0, lx)
  new <- data.frame(X = x)
  pred <- predict(mod, newdata = new, type = "response")
  pred[pred < gY[1]] <- NA
  pred[pred > gY[nGrid]] <- NA

  # Compute theoretical confidence bands
  sigma <- summary(mod)$sigma
  if (alpha > 0) {

    # Limits
    yDown <- qnorm(alpha/2, pred, sigma)
    yUp <- qnorm(1 - alpha/2, pred, sigma)
    yDownCut <- pmax(yDown, gY[1])
    yUpCut <- pmin(yUp, gY[nGrid])
    yDown[yDown < gY[1]] <- NA
    yUp[yUp > gY[nGrid]] <- NA

    # Plot confidence region
    polygon(trans3d(c(x, rev(x)), c(yDownCut, rev(yUpCut)), rep(0, 2 * lx),
                    gridMat), border = NA, col = "yellow", density = 40)
    lines(trans3d(x, yDown, zeros, gridMat), lty = 2)
    lines(trans3d(x, yUp, zeros, gridMat), lty = 2)

  }

  # Plot regression curve
  lines(trans3d(x, pred, zeros, gridMat), lwd = 2)

  # Plot data
  points(trans3d(data$X, data$Y, rep(0, n), gridMat), pch = 16, col = "red",
         cex = 0.75)

  # Plot densities
  mgig <- predict(mod, newdata = data.frame(X = gX))
  nGridDens <- 251
  z0 <- rep(0, nGridDens)
  y <- seq(gY[1], gY[nGrid], length = nGridDens)
  for (j in (nGrid - 1):2) {

    x <- rep(gX[j], nGridDens)
    z <- dnorm(y, mean = mgig[j], sd = sigma)
    polygon(trans3d(rep(x, 2), c(y, rev(y)), c(z, z0), gridMat),
            border = NA, col = "light blue", density = 40)
    lines(trans3d(x, y, z0, gridMat), col = "light blue", lty = 2)
    lines(trans3d(x, y, z, gridMat), col = "blue")

  }

}


#' @title Visualize the fit of a logistic model
#'
#' @description Generates a plot illustrating the key concepts for a fitted logistic model: linear trend, model-based confidence bands and normality around the mean.
#'
#' @param data a \code{data.frame} containing \code{X} and \code{Y} as variables.
#' @param nGrid number of cuts used to produce the XY grid.
#' @inheritParams graphics::persp
#' @return
#' Nothing. The function is called to produce a plot.
#' @examples
#' # Generate data
#' X <- rnorm(100)
#' L <- 0.5 - 1.5 * X
#' Y <- rbinom(100, size = 1, p = exp(L) / (1 + exp(L)))
#' data <- data.frame(X = X, Y = Y)
#'
#' par(mar = rep(0, 4), oma = rep(0, 4))
#' visualizeFitLog(data = data)
#' @author Eduardo García-Portugués (\email{edgarcia@est-econ.uc3m.es}), based on the original code from Arthur Charpentier (\url{http://freakonometrics.hypotheses.org/9593}).
#' @export
visualizeFitLog <- function(data, nGrid = 10, theta = -30, phi = 20) {

  # Estimate glm
  n <- length(data$X)
  mod <- glm(Y ~ X, data = data, family = "binomial")

  # Create and plot initial XY-grid
  sdX <- sd(data$X)
  gX <- seq(min(data$X) - sdX, max(data$X) + sdX, length = nGrid)
  gY <- seq(-1/(nGrid - 3), 1 + 1/(nGrid - 3), length = nGrid)
  gridMat <- persp(gX, gY, matrix(0, nGrid, nGrid), zlim = c(0, 1),
                   theta = theta, phi = phi, box = FALSE, border = gray(0.75))
  text(x = trans3d(median(gX), min(gY), 0, gridMat), labels = "x", pos = 1)
  text(x = trans3d(min(gX), median(gY), 0, gridMat), labels = "y", pos = 2)

  # Compute regression curve
  lx <- 501
  x <- seq(gX[1], gX[nGrid], length = lx)
  zeros <- rep(0, lx)
  new <- data.frame(X = x)
  pred <- predict.glm(mod, newdata = new, type = "response")

  # Plot regression curve
  lines(trans3d(x, pred, zeros, gridMat), lwd = 2)

  # Plot data
  points(trans3d(data$X, data$Y, rep(0, n), gridMat), pch = 16, col = "red",
         cex = 0.75)

  # Plot bars
  mgig <- predict(mod, newdata = data.frame(X = gX), type = "response")
  nGridDens <- 251
  y <- c(0, 1)
  for (j in (nGrid - 1):2) {

    bar <- gX[j] + c(-1, 1) * (gX[2] - gX[1]) / 10
    z <- dbinom(x = y, size = 1, prob = mgig[j])
    polygon(trans3d(c(bar, rev(bar)), 1, c(0, 0, rep(z[2], 2)), gridMat),
            border = NA, col = "light blue", density = 40)
    lines(trans3d(rep(bar, each = 2), 1, c(0, z[2], z[2], 0), gridMat),
          col = "blue")
    polygon(trans3d(c(bar, rev(bar)), 0, c(0, 0, rep(z[1], 2)), gridMat),
            border = NA, col = "light blue", density = 40)
    lines(trans3d(rep(bar, each = 2), 0, c(0, z[1], z[1], 0), gridMat),
          col = "blue")

  }

}


#' @title Visualize the fit of a multiple linear model
#'
#' @description Generates a plot illustrating the key concepts for a fitted multiple linear model: linear trend, model-based confidence bands and normality around the mean.
#'
#' @param data a \code{data.frame} containing \code{X}, \code{Y} and \code{Z} as variables.
#' @param nGrid (approximate) number of cuts used to produce the XYZ grid. Computed to match the effective \code{nticks} in \code{\link[graphics]{persp}}.
#' @inheritParams visualizeFitLm
#' @inheritParams graphics::persp
#' @param basalScatter Add the projections of points in the XY plane?
#' @param lmLaterals Add the projections of points in the XZ and YZ planes, with a (marginal) regression line?
#' @return
#' Nothing. The function is called to produce a plot.
#' @examples
#' # Generate data
#' X <- rnorm(100, sd = 1.5)
#' Y <- rnorm(100, sd = 1.5)
#' Z <- 0.5 - 0.25 * X + 0.15 * Y + rnorm(100)
#' data <- data.frame(X = X, Y = Y, Z = Z)
#'
#' par(mar = rep(0, 4), oma = rep(0, 4))
#' visualizeFitLm3D(data = data, lmLaterals = FALSE)
#' visualizeFitLm3D(data = data, phi = 10, lmLaterals = FALSE)
#'
#' # Illustrate marginal regression lines
#' set.seed(212542)
#' n <- 100
#' x <- rnorm(n, sd = 2)
#' y <- rnorm(n, mean = x, sd = 3)
#' z <- 1 + 2 * x - y + rnorm(n, sd = 1)
#' summary(lm(z ~ x))
#' summary(lm(z ~ y))
#' summary(lm(z ~ x + y))
#' data <- data.frame(X = x, Y = y, Z = z)
#' par(mar = rep(0, 4), oma = rep(0, 4))
#' visualizeFitLm3D(data = data, alpha = 0, theta = -45, phi = 20)
#' @author Eduardo García-Portugués (\email{edgarcia@est-econ.uc3m.es}).
#' @export
visualizeFitLm3D <- function(data, nGrid = 5, theta = -30, phi = 20,
                             alpha = 0.05, basalScatter = TRUE,
                             lmLaterals = TRUE) {

  # Estimate lm
  mod <- lm(Z ~ X + Y, data = data)
  n <- length(data$X)

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

  # Compute theoretical confidence bands
  if (alpha > 0) {

    sigma <- summary(mod)$sigma
    zDown <- qnorm(alpha/2, pred, sigma)
    zUp <- qnorm(1 - alpha/2, pred, sigma)
    gZ <- seq(min(c(gZ, zDown)), max(c(gZ, zUp)), length = nGrid)

  }

  # Plot data and regression
  require(plot3D)
  panelFirst <- function(pmat) {

    # Plot points projection in the basal plane
    if (basalScatter) {

      XY <- trans3D(data$X, data$Y, rep(gZ[1], n), pmat = pmat)
      scatter2D(XY$x, XY$y, pch = 16, cex = 0.5, add = TRUE,
                colkey = FALSE, col = 1)

    }

    # Plot points projection in the basal plane
    if (lmLaterals) {

      XZ <- trans3D(data$X, rep(gY[nGrid], n), data$Z, pmat = pmat)
      scatter2D(XZ$x, XZ$y, pch = 16, cex = 0.5, add = TRUE,
                colkey = FALSE, col = 1)
      mod <- lm(Z ~ X, data = data)
      lines(trans3D(gX, rep(gY[nGrid], nGrid),
                    mod$coefficients[1] + mod$coefficients[2] * gX, pmat),
            col = 3, lwd = 2)

      YZ <- trans3D(rep(gX[nGrid], n), data$Y, data$Z, pmat = pmat)
      scatter2D(YZ$x, YZ$y, pch = 16, cex = 0.5, add = TRUE,
                colkey = FALSE, col = 1)
      mod <- lm(Z ~ Y, data = data)
      lines(trans3D(rep(gX[nGrid], nGrid), gY,
                    mod$coefficients[1] + mod$coefficients[2] * gY, pmat),
            col = 3, lwd = 2)

    }

  }
  gridMat <- scatter3D(data$X, data$Y, data$Z, pch = 16, theta = theta, phi = phi,
                       bty = "g", axes = FALSE, colkey = FALSE, col = 2,
                       xlim = range(gX), ylim = range(gY), zlim = range(gZ),
                       panel.first = panelFirst, nticks = nGrid, cex = 0.75)
  text(x = trans3d(median(gX), gY[1], gZ[1], gridMat), labels = "x1", pos = 1)
  text(x = trans3d(gX[1], median(gY), gZ[1], gridMat), labels = "x2", pos = 2)
  text(x = trans3d(gX[1], gY[nGrid], median(gZ), gridMat), labels = "y", pos = 2)

  # Plot confidence region
  M <- mesh(x, y)
  if (alpha > 0) {

    surf3D(x = M$x, y = M$y, z = matrix(zUp, nrow = lx, ncol = ly), col = "yellow",
           alpha = 0.1, add = TRUE, border = "yellow2")
    surf3D(x = M$x, y = M$y, z = matrix(zDown, nrow = lx, ncol = ly), col = "yellow",
           alpha = 0.1, add = TRUE, border = "yellow2")

  }
  surf3D(x = M$x, y = M$y, z = matrix(pred, nrow = lx, ncol = ly), col = "lightblue",
         alpha = 0.2, border = gray(0.5), add = TRUE)

}
