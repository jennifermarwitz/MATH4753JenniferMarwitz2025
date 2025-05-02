#' Calculate the Optimal Number of Tickets to Sell
#'
#' This function determines the optimal number of tickets to sell for a flight
#' with `N` available seats, given the probability of a passenger showing up (`p`)
#' and the probability of overbooking (`gamma`). The function uses both a
#' discrete probability distribution and a normal approximation.
#'
#' @param N Integer. The total number of available seats on the flight.
#' @param gamma Numeric. The allowed probability of overbooking (i.e., more passengers show up than available seats).
#' @param p Numeric. The probability that an individual passenger shows up.
#'
#' @return A named list containing:
#' \describe{
#'   \item{n_disc}{Optimal number of tickets to sell using the discrete distribution.}
#'   \item{n_cont}{Optimal number of tickets to sell using the normal approximation.}
#'   \item{N}{Input value for total available seats.}
#'   \item{p}{Input probability of a passenger showing up.}
#'   \item{gamma}{Input probability of overbooking.}
#' }
#' @importFrom stats pbinom pnorm
#' @examples
#' ntickets(N = 400, gamma = 0.02, p = 0.95)
#'
#' @export
ntickets <- function(N, gamma, p) {
  # Discrete calculation
  n_vals <- seq(N, floor(N + N/10), by = 1)
  obj_fn_disc <- 1 - gamma - pbinom(N, size = n_vals, prob = p)
  idx_disc <- which.min(abs(obj_fn_disc))
  n_disc <- n_vals[idx_disc]

  # Continuous calculation
  n_vals_cont <- seq(N, floor(N + N/10), by = 0.001)
  obj_fn_cont <- 1 - gamma - pnorm(N + 0.5, mean = n_vals_cont * p, sd = sqrt(n_vals_cont * p * (1 - p)))
  idx_cont <- which.min(abs(obj_fn_cont))
  n_cont <- n_vals_cont[idx_cont]

  # Set up plotting layout
  par(mfrow = c(2, 1), mar = c(4, 5, 2, 1))

  # Plot the discrete case
  plot(n_vals, obj_fn_disc, type = "b", col = "blue", pch = 16, lwd = 2,
       ylab = "Objective", xlab = "n", ylim = c(0, 1),
       main = paste("Objective Vs n to find optimal tickets sold\n(", n_disc, ") gamma=", gamma, " N=", N, " discrete"))
  abline(h = 0, col = "red", lty = 2)  # Reference line at 0
  abline(v = n_disc, col = "red", lwd = 2)  # Red vertical line at n_disc

  # Plot the continuous case
  plot(n_vals_cont, obj_fn_cont, type = "l", col = "black", lwd = 2,
       ylab = "Objective", xlab = "n", ylim = c(0, 1),
       main = paste("Objective Vs n to find optimal tickets sold\n(", round(n_cont, 2), ") gamma=", gamma, " N=", N, " continuous"))
  abline(h = 0, col = "blue", lty = 2)  # Reference line at 0
  abline(v = n_cont, col = "blue", lwd = 2)  # Blue vertical line at n_cont

  # Return named list
  return(list(n_disc = n_disc, n_cont = round(n_cont, 2), N = N, p = p, gamma = gamma))
}
