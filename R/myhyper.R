#' Hypergeometric Distribution Simulation
#'
#' This function simulates the hypergeometric distribution by randomly sampling
#' without replacement from a population containing a fixed number of successes.
#'
#' @importFrom grDevices rainbow
#' @importFrom graphics par
#' @name myhyper
#' @param iter Integer. The number of iterations (simulations) to perform. Default is 100.
#' @param N Integer. The total population size. Default is 20.
#' @param r Integer. The number of successes in the population. Default is 12.
#' @param n Integer. The sample size drawn in each iteration. Default is 5.
#'
#' @returns A named numeric vector representing the relative frequencies of the number of successes in the samples.
#' A barplot of the proportions is also displayed.
#' @export
#'
#' @examples
#' # Run a hypergeometric simulation with default parameters
#' myhyper()
#'
#' # Run a hypergeometric simulation with custom parameters
#' myhyper(iter = 1000, n = 19, N = 20, r = 12)
#'
#' # Compare with the theoretical hypergeometric probability distribution
#' dhyper(x = 0:19, m = 12, n = 8, k = 19)
#'
myhyper = function(iter = 100, N = 20, r = 12, n = 5) {
  # make a matrix to hold the samples
  # initially filled with NA's
  sam.mat = matrix(NA, nrow = n, ncol = iter, byrow = TRUE)

  # make a vector to hold the number of successes over the trials
  succ = c()
  for(i in 1:iter) {
    # fill each column with a new sample
    sam.mat[,i] = sample(rep(c(1,0), c(r, N-r)), n, replace = FALSE)
    # Calculate a statistic from the sample (this case it is the sum)
    succ[i] = sum(sam.mat[,i])
  }

  # Make a table of successes
  succ.tab = table(factor(succ, levels = 0:n))
  par(mar = c(2, 2, 2, 2))  # Decrease margins
  # Make a barplot of the proportions
  barplot(succ.tab/(iter), col = rainbow(n+1), main = "HYPERGEOMETRIC simulation", xlab = "Number of successes")
  succ.tab/iter
}
myhyper(iter = 1000, n= 19, N = 20, r = 12)
dhyper(x = 0:19, m = 12, n = 8, k = 19)
