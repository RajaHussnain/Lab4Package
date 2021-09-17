# 1.2 linreg()

#' Calculate Linear regression

#' @import stats
#' @import utils

#' @export


linreg <- function(formula, data){

  #' @title Linear Regression
  #' @description The 'linreg' function calculate the linear regression model
  #' @note 'linreg' is a function like "lm" from "stats" package
  #' @param formula an object of class "formula"
  #' @param data a data frame
  #' @param ... additional argument \code{model.matrix} is a function
  #' @return List of outputs
  #' @examples
  #' data("iris")
  #' linreg(Petal.Length ~ Species, iris)
  #' @source \url{https://en.wikipedia.org/wiki/Linear_regression}{Linear Regression}

  y <- data[, unique(all.vars(formula)[1])]

  X <- model.matrix(formula, data)


  coefficients <- as.vector(solve(t(X) %*% X) %*% t(X) %*% y)
  names(coefficients) <- colnames(X)
  fitted_values <- X %*% coefficients
  residuals <- y - fitted_values
  degrees_of_freedom <- nrow(X) - ncol(X)
  residual_variance <- as.numeric((t(residuals) %*% residuals) / degrees_of_freedom)
  variance_coefficients <- residual_variance * solve(t(X) %*% X)
  t_values <- coefficients / sqrt(diag(variance_coefficients))
  p_values = 2*pt(-abs(t_values), degrees_of_freedom)

  return_object <- list("Regressions coefficients" = coefficients, "The fitted values:" = head(fitted_values),
                        "The residuals:" = head(residuals), "The degrees of freedom:" = degrees_of_freedom,
                        "The residual variance:" = residual_variance,
                        "The variance of the regression coefficients:" = variance_coefficients, "t values:" = t_values,
                        "p values" = p_values)
  return(return_object)
}
# data(iris)
# linreg(Petal.Length~Species+Sepal.Width, data = iris)

