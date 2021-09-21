# 1.2 linreg()

#' Calculate Linear regression

#' @import stats
#' @import utils


#' @title Linreg Class Constructor
#' @param object A list
#' @return The list input from class 'linreg'
# build a 'linreg' class constructor
new_linreg <- function(object = list()) {
  structure(object, class = "linreg")
  # class(object) <- "linreg"
}

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
#' @export

linreg <- function(formula, data, ...){

  call <- match.call()
  y1 <- all.vars(formula)[1]
  y <- data[[y1]]
  X <- model.matrix(formula, data)
  QR <- qr(X)
  Q <- qr.Q(QR)
  R <- qr.R(QR)

  coefficients <- qr.coef(QR, y)
  fitted_values <- X %*% coefficients
  residuals <- qr.resid(QR, y)
  degrees_of_freedom <- nrow(X) - ncol(X)
  residual_variance <- as.numeric((t(residuals) %*% residuals) / degrees_of_freedom)
  variance_coefficients <- residual_variance * solve(t(R) %*% R)
  names(coefficients) <- colnames(X)
  t_values <- coefficients / sqrt(diag(variance_coefficients))
  p_values = pt(-abs(t_values), degrees_of_freedom)

  return_object <- list("Regressions coefficients" = coefficients, "The fitted values" = fitted_values,
                        "The residuals" = residuals, "The degrees of freedom" = degrees_of_freedom,
                        "The residual variance" = residual_variance,
                        "The variance of the regression coefficients" = variance_coefficients, "t values" = t_values,
                        "p values" = p_values, "call" = call)
  # return(return_object[["The fitted values"]])
  return(new_linreg(return_object))
}

# linreg(Petal.Length ~ Species + Sepal.Width, data = iris)


