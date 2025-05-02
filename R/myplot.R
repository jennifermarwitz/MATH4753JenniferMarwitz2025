#' Piecewise Regression for Spruce Data
#'
#' This function fits a piecewise linear regression model to the spruce dataset
#' and plots the results.
#'
#' @importFrom graphics abline barplot curve polygon text
#' @name myplot
#' @param data A dataframe containing the variables "Height" and "BHDiameter".
#' @param breakpoint The breakpoint for the piecewise function (default is 18).
#' @return A list containing the linear model summary and the plot.
#' @export
myplot <- function(data, breakpoint = 18) {
  # Ensure required columns exist
  if (!all(c("Height", "BHDiameter") %in% colnames(data))) {
    stop("Data must contain 'Height' and 'BHDiameter' columns.")
  }

  # Create the piecewise variable
  data <- within(data, {
    X <- (BHDiameter - breakpoint) * (BHDiameter > breakpoint)
  })

  # Fit the piecewise linear model
  lmp <- lm(Height ~ BHDiameter + X, data = data)
  tmp <- summary(lmp)

  # Define function for prediction
  myf <- function(x, coef) {
    coef[1] + coef[2] * x + coef[3] * (x - breakpoint) * (x > breakpoint)
  }

  # Plot the data
  plot(data$BHDiameter, data$Height, main = "Piecewise Regression",
       xlab = "BHDiameter", ylab = "Height", pch = 16)

  # Add piecewise regression curve
  curve(myf(x, coef = tmp$coefficients[, "Estimate"]),
        add = TRUE, lwd = 2, col = "blue")

  # Add vertical line at breakpoint
  abline(v = breakpoint, lty = 2, col = "red")

  # Display R-squared value
  text(breakpoint, max(data$Height) * 0.9,
       paste("R sq.=", round(tmp$r.squared, 4)), pos = 4, col = "blue")

  # Return model summary
  return(tmp)
}


# piecewise_regression(spruce.df, breakpoint = 18)
