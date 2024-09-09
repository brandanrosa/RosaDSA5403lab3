#' myGamCon
#'
#' A plot of the posterior of a Gamma-Poisson distribution.
#'
#' @param a shape 1
#' @param b shape 2
#' @param n number of trials
#' @param x number of successes
#'
#' @return a plot of the posterior distribution
#' @export
#'
#' @examples \dontrun{myGamCon(a=5, b=7, n=10, x=4)}
myGamCon <- function(a, b, n, x) {

  # Numbers for plotting
  numbers <- seq(0, 1, by = 0.01)

  # Gamma Parameters for Posterior
  alpha <- a + x
  beta <- b + n

  # Mean, Variance, & Mode
  mu <- alpha / beta
  var <- alpha / beta^2
  w <- (alpha - 1) / beta

  # Posterior Distribution
  post <- stats::dgamma(x = numbers, shape = alpha, rate = beta)

  # Plot
  plot(numbers,
       post,
       type = "l",
       col = "darkgreen",
       main = "Gamma Posterior Distribution",
       ylab = "Posterior",
       xlab = "Theta",
       lwd = 3
  )

  # List
  list(Mean = mu, Variance = var, Mode = w)
}
