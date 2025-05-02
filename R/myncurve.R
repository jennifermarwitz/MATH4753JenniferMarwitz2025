utils::globalVariables(c("x"))
#' Plot a Normal Distribution Curve with Shaded Area
#'
#' This function plots a normal distribution curve with a given mean and standard deviation,
#' shades the area under the curve from -∞ to a given value, and calculates the probability P(X ≤ a).
#'
#' utils::globalVariables(c("x"))  # Prevents NOTE about x being undefined
#' @importFrom stats dnorm lm pnorm
#' @name myncurve
#' @param mu Mean of the normal distribution.
#' @param sigma Standard deviation of the normal distribution.
#' @param a Cutoff point where the probability is calculated.
#' @return A list containing the mean, standard deviation, cutoff, and computed probability.
#' @examples
#' myncurve(mu = 0, sigma = 1, a = 1.5)
#' @export
myncurve = function(mu, sigma, a){
  curve(dnorm(x, mean=mu, sd=sigma), xlim = c(mu - 3*sigma, mu + 3*sigma),
        ylab="Density", xlab="X", main=paste("Normal(", mu, ",", sigma, ")"))

  xcurve = seq(mu - 3*sigma, a, length=1000)
  ycurve = dnorm(xcurve, mean=mu, sd=sigma)
  polygon(c(min(xcurve), xcurve, a), c(0, ycurve, 0), col="red", border=NA)

  prob = round(pnorm(a, mean=mu, sd=sigma), 4)
  text(x = mu, y = max(ycurve)/2, paste("P(X <=", a, ") =", prob), col="black")

  return(list(mu = mu, sigma = sigma, a = a, probability = prob))
}
