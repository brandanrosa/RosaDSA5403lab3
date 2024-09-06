#' myBetaCon
#'
#' Produces a plot of the posterior distribution for the Beta-Binomial conjugate pair
#'
#' @param a shape 1
#' @param b shape 2
#' @param n number of trials
#' @param x number of successes
#'
#' @return a plot of the posterior distribution
#' @export
#'
#' @examples \dontrun{myBetaCon(a=5, b=7, n=10, x=4)}
myBetaCon <- function(a, b, n, x) {

  # Numbers for plotting
  numbers <- seq(0, 1, by = 0.01)

  # Beta Parameters for Posterior
  alpha <- x + a
  beta <- n - x + b

  # mean, variance, & mode
  mu <- alpha / (alpha + beta)
  var <- (alpha*beta) / ((alpha + beta)^2 * (alpha + beta + 1))
  w <- (alpha - 1) / (alpha + beta - 2)

  # Posterior
  post <- stats::dbeta(x = numbers, shape1 = alpha, shape2 = beta)

  # Plot
  plot(numbers,
       post,
       type = "l",
       col = "hotpink",
       ylab = "Posterior",
       xlab = "Theta",
       lwd = 2.5
  )

  # List
  list(Mean = mu, Variance = var, Mode = w)
}
