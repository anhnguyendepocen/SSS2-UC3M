# A simple and non-exhaustive analysis for the price of the houses in the Boston
# dataset. The purpose is to quantify, by means of a multiple linear model,
# the effect of 14 variables in the price of a house in the suburbs of Boston.

# Import data
library(MASS)
data(Boston)

# Make a multiple linear regression of medv in the rest of variables
mod <- lm(medv ~ ., data = Boston)
summary(mod)

# Check the linearty assumption
plot(mod, 1) # Clear non-linearity

# Let's consider the transformations given in Harrison and Rubinfeld (1978)
modTransf <- lm(I(log(medv * 1000)) ~ I(rm^2) + age + log(dis) +
                  log(rad) + tax + ptratio + I(black / 1000) +
                  I(log(lstat / 100)) + crim + zn + indus + chas +
                  I((10 * nox)^2), data = Boston)
summary(modTransf)

# The non-linearity is more subtle now
plot(modTransf, 1)

# Look for the best model in terms of the BIC
modTransfBIC <- stepwise(modTransf, trace = 0)
summary(modTransfBIC)

# Let's explore the most significant variables, to see if the model can be
# reduced drastically in complexity
mod3D <- lm(I(log(medv * 1000)) ~ I(log(lstat / 100)) + crim, data = Boston)
summary(mod3D)

# With only 2 variables, we explain the 72% of variability.
# Compared with the 80% with 10 variables, it is an important improvement
# in terms of simplicity.

# Let's add these variables to the dataset, so we can call scatterplotMatrix
# and scatter3d through R Commander's menu
Boston$logMedv <- log(Boston$medv * 1000)
Boston$logLstat <- log(Boston$lstat / 100)

# Visualize the pair-by-pair relations of the response and two predictors
scatterplotMatrix(~ crim + logLstat + logMedv, reg.line = lm, smooth = FALSE,
                  spread = FALSE, span = 0.5, ellipse = FALSE,
                  levels = c(.5, .9), id.n = 0, diagonal = 'histogram',
                  data = Boston)

# Visualize the full relation between the response and the two predictors
scatter3d(logMedv ~ crim + logLstat, data = Boston, fit = "linear",
          residuals = TRUE, bg = "white", axis.scales = TRUE, grid = TRUE,
          ellipsoid = FALSE)

