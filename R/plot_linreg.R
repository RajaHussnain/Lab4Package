#' Plot S3 class method
#' @import ggplot2

#' @title Plot Residuals vs Fitted values
#' @param object An object of class linreg
#' @param ... Parameters
#' @export

# S3 method

plot <- function (object, ...) UseMethod("plot")

#' @title Plot graph of Residuals vs Fitted values and Standardized residuals vs Fitted values
#' @description An S3 function to plot Residuals vs Fitted graph
#' @param object An object of class linreg
#' @param ... Parameters
#' @return Plots
#' @export


plot.linreg <- function(object, ...)
{
  data <- data.frame("fitted_values" = object[["The fitted values"]], "residuals" = object[["The residuals"]],
                     "standardized_residual" = sqrt(abs(object[["The residuals"]]/sd(object[["The residuals"]]))))
  p1 <- ggplot(data, aes(x=fitted_values, y=residuals)) +
    geom_point(shape = 1, size = 2) +
    theme(panel.border = element_rect(linetype = "solid", fill = NA)) +
    stat_summary(fun = "median", geom = "smooth", colour = "red") +
    labs(title = "Residuals vs Fitted",
         x = paste("Fitted values\n linreg(", paste(toString(object[["call"]][2]),")",sep=""),sep=""),
         y = "Residuals") +
    theme(plot.title = element_text(hjust = 0.5))

  p2 <- ggplot(data, aes(x=fitted_values, y=standardized_residual)) +
    geom_point(shape = 1, size = 2) +
    theme(panel.border = element_rect(linetype = "solid", fill = NA)) +
    stat_summary(fun = "mean", geom = "smooth", colour = "red") +
    labs(title = "Scale−Location",
         x = paste("Fitted values\n linreg(", paste(toString(object[["call"]][2]),")",sep=""),sep=""),
         y = expression(sqrt("Standardized residuals"))) +
    theme(plot.title = element_text(hjust = 0.5))
  return_objects <- list(p1, p2)
  return(return_objects)
}





