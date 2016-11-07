
#' @title Simplification of R's ANOVA table
#'
#' @description Performs the most simple ANOVA decomposition: SST = SSR + SSE.
#'
#' @inheritParams stats::anova.lm
#' @return
#' An object of class \code{anova} inheriting from class \code{data.frame} with a unique row for all the predictors.
#' @examples
#' data(iris)
#' # Simple regression
#' mod <- lm(Petal.Width ~ Sepal.Length, data = iris)
#' anova(mod)
#' simpleAnova(mod)
#'
#' # Multiple regression
#' mod <- lm(Petal.Width ~ ., data = iris)
#' mod0 <- lm(Petal.Width ~ 1, data = iris)
#' anova(mod)
#' simpleAnova(mod)
#' anova(mod0, mod)
#' @author Eduardo García-Portugués (\email{edgarcia@est-econ.uc3m.es}).
#' @export
simpleAnova <- function(object, ...) {

  # Compute anova table
  tab <- anova(object, ...)

  # Obtain number of predictors
  p <- nrow(tab) - 1

  # Add predictors row
  predictorsRow <- colSums(tab[1:p, 1:2])
  predictorsRow <- c(predictorsRow, predictorsRow[2] / predictorsRow[1])

  # F-quantities
  Fval <- predictorsRow[3] / tab[p + 1, 3]
  pval <- pf(Fval, df1 = p, df2 = tab$Df[p + 1], lower.tail = FALSE)
  predictorsRow <- c(predictorsRow, Fval, pval)

  # Simplified table
  tab <- rbind(predictorsRow, tab[p + 1, ])
  row.names(tab)[1] <- "Predictors"
  return(tab)

}
