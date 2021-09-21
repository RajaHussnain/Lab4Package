
#' Vector of residuals (e) method of S3 class
#' @title S3 Generic function for Vector of residuals (e) \code{linreg}
#' @param object An object of class linreg
#' @param ... Parameters
#' @export

# S3 method

resid <- function (object, ...) UseMethod("resid")

#' @title Vector of residuals
#' @description From S3 function return residuals object
#' @param object object of class "linreg"
#' @param ... Parameters
#' @return The residuals
#' @export

resid.linreg <- function(object, ...)
{
  return(object[["The residuals"]])
}
