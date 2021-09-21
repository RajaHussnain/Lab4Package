#' Summary method of S3 class
#' @title S3 Generic function for summary \code{linreg}
#' @param object An object of class linreg
#' @param ... Parameters
#' @export

# S3 method

summary <- function (object, ...) UseMethod("summary")

#' @import stats
#' @title Predicted values
#' @description From S3 function return predicted object
#' @param object object of class "linreg"
#' @param ... Parameters
#' @return The predicted value
#' @export

summary.linreg <- function(object, ...)
{
  summary_df <- as.data.frame(object[["Regressions coefficients"]])
  summary_df[,1] <- round(as.numeric(object[["Regressions coefficients"]]), 5)
  summary_df[,2] <- round(as.numeric(sqrt(diag(object[["The variance of the regression coefficients"]]))), 5)
  summary_df[,3] <- round(as.numeric(object[["t values"]]), 5)
  summary_df[,4] <- sapply(object[["p values"]],
                           function(x) if(x < 0.001) {"***"}
                           else if (x < 0.01) {"**"}
                           else if (x < 0.05) {"*"}
                           else if (x < 0.1) {"."}
                           else {" "})
  cat("Call:\n")
  print(object[["call"]])
  colnames(summary_df) <- c("Estimate", "Std. Error", "t value", "Pr(>|t|)")
  cat("\nCoefficients:\n")
  print(summary_df)
  cat("---\n")
  cat("Residual standard error:", sqrt(object[["The residual variance"]]), "on",
      object[["The degrees of freedom"]], "degrees of freedom")

}

