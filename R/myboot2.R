#' Bootstrap Confidence Interval and Histogram
#'
#' Performs bootstrap resampling on a numeric vector to estimate the confidence interval
#' of a specified statistic (e.g., mean, median), and plots a histogram of the bootstrap
#' distribution.
#'
#' @param iter Integer. Number of bootstrap iterations to perform. Default is 10,000.
#' @param x Numeric vector. The data to bootstrap.
#' @param fun Character or function. The statistic to compute on each resample (e.g., "mean", "median").
#' @param alpha Numeric. The significance level for the confidence interval (e.g., 0.05 for a 95\% CI). Default is 0.05.
#' @param cx Numeric. Character expansion factor for plot text size. Default is 1.5.
#' @param ... Additional arguments passed to the \code{hist()} function.
#'
#' @return A list with the following components:
#' \describe{
#'   \item{ci}{A numeric vector with the lower and upper bounds of the bootstrap confidence interval.}
#'   \item{fun}{The statistic function used.}
#' \item{x}{The original data vector.}
#' }
#'
#' @details
#' This function resamples the input vector \code{x} with replacement \code{iter} times,
#' applies the specified function \code{fun} to each resample, and then computes the
#' confidence interval based on the empirical distribution of the resulting statistics.
#'
#' A histogram of the bootstrap distribution is plotted with:
#' \itemize{
#'   \item A vertical line for the observed statistic.
#'   \item A horizontal segment indicating the confidence interval.
#'   \item Text annotations for the CI and point estimate.
#' }
#'
#' @examples
#' set.seed(123)
#' data <- rnorm(100)
#' myboot2(iter = 5000, x = data, fun = "mean", alpha = 0.05)
#'
#' @importFrom stats quantile
#' @importFrom graphics segments hist abline text
#'
#' @export


myboot2 <- function(iter = 10000, x, fun = "mean", alpha = 0.05, cx = 1.5, ...) {
  n <- length(x)

  y <- sample(x, n * iter, replace = TRUE)
  rs.mat <- matrix(y, nrow = n, ncol = iter, byrow = TRUE)

  xstat <- apply(rs.mat, 2, fun)
  ci <- quantile(xstat, c(alpha / 2, 1 - alpha / 2))

  para <- hist(
    xstat, freq = FALSE, las = 1,
    main = paste("Histogram of Bootstrap sample statistics\nalpha=", alpha, " iter=", iter, sep = ""),
    ...
  )

  mat <- matrix(x, nrow = length(x), ncol = 1, byrow = TRUE)
  pte <- apply(mat, 2, fun)

  abline(v = pte, lwd = 3, col = "black")
  segments(ci[1], 0, ci[2], 0, lwd = 4)
  text(ci[1], 0, paste("(", round(ci[1], 2), sep = ""), col = "red", cex = cx)
  text(ci[2], 0, paste(round(ci[2], 2), ")", sep = ""), col = "red", cex = cx)
  text(pte, max(para$density) / 2, round(pte, 2), cex = cx)

  invisible(list(ci = ci, fun = fun, x = x))
}
