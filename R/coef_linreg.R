#' Coefficients method of S3 class
#' @title S3 Generic function for coefficient \code{linreg}
#' @param object An object of class linreg
#' @param ... Parameters
#' @export

# S3 method

coef <- function (object, ...) UseMethod("coef")

#' @import stats
#' @title Linear Regression Coefficients
#' @description From S3 function return coefficient object
#' @param object object of class "linreg"
#' @param ... Parameters
#' @return Regression coefficient
#' @export

coef.linreg <- function(object, ...)
{
  return(object[[1]])
}
