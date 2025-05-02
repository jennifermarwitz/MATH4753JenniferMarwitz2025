#' Central Limit Theorem Simulation for Poisson Distribution
#'
#' This function demonstrates the Central Limit Theorem (CLT) using samples from a Poisson distribution.
#' It generates histograms, bar plots, and probability distributions to visualize the behavior of sample means.
#'
#' @param n Sample size for each iteration.
#' @param iter Number of iterations (samples to draw).
#' @param lambda Rate parameter for the Poisson distribution (default is 10).
#' @param ... Additional arguments passed to the histogram function.
#'
#' @return A set of plots: histogram of sample means, bar plot of sampled values, and Poisson probability function.
#' @importFrom stats rpois dpois
#' @importFrom graphics hist layout barplot curve plot
#' @examples
#' mycltp(n = 20, iter = 10000, lambda = 10)
#' @export
mycltp <- function(n, iter, lambda = 10, ...) {

  # Generate random Poisson-distributed data
  y <- rpois(n * iter, lambda = lambda)

  # Reshape data into a matrix with 'n' rows and 'iter' columns
  data <- matrix(y, nrow = n, ncol = iter, byrow = TRUE)

  # Compute the mean of each column (each sample)
  w <- apply(data, 2, mean)

  # Histogram parameters
  param <- hist(w, plot = FALSE)
  ymax <- max(param$density)
  ymax <- 1.1 * ymax  # Adjust for better visualization

  # Layout setup for multiple plots
  layout(matrix(c(1,1,2,3), nrow = 2, ncol = 2, byrow = TRUE))

  # Plot histogram of sample means
  hist(w, freq = FALSE, ylim = c(0, ymax), col = rainbow(max(w)),
       main = paste("Histogram of Sample Mean", "\n", "Sample Size=", n, " Iter=", iter, " Lambda=", lambda, sep=""),
       xlab = "Sample Mean", ...)

  # Overlay normal approximation (Central Limit Theorem)
  curve(dnorm(x, mean = lambda, sd = sqrt(lambda / n)), add = TRUE, col = "Red", lty = 2, lwd = 3)

  # Bar plot of sampled values
  barplot(table(y) / (n * iter), col = rainbow(max(y)),
          main = "Barplot of Sampled y", ylab = "Relative Frequency", xlab = "y")

  # Probability function for Poisson distribution
  x <- 0:max(y)
  plot(x, dpois(x, lambda = lambda), type = "h", lwd = 5, col = rainbow(max(y)),
       main = "Probability Function for Poisson", ylab = "Probability", xlab = "y")
}
