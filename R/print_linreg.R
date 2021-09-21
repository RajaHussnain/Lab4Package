

#' Print S3 class method
#' @title S3 Generic function to print linreg
#' @param object An object of class linreg
#' @param ... Parameters
#' @export

# S3 method

print <- function (object, ...) UseMethod("print")

#' @title Print coefficients
#' @description An S3 function to print coefficients
#' @param object An object of class linreg
#' @param ... Parameters
#' @export

print.linreg <- function(object, ...)
{
  cat("Call:\n")
  print(object[["call"]])
  # cat("lm(Petal.Length~Species, data = iris)")
  cat("\nCoefficients:\n")
  # print(object[[1]])
  print(object[["Regressions coefficients"]][1:3])
}

