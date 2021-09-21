#' Predicted values (y) method of S3 class
#' @title S3 Generic function for Predicted values (y) \code{linreg}
#' @param object An object of class linreg
#' @param ... Parameters
#' @export

# S3 method

pred <- function (object, ...) UseMethod("pred")

#' @import stats
#' @title Predicted values
#' @description From S3 function return predicted object
#' @param object object of class "linreg"
#' @param ... Parameters
#' @return The predicted value
#' @export

pred.linreg <- function(object, ...)
{
  return(object[["The fitted values"]])
}
