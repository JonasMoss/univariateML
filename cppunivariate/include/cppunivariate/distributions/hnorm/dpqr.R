#' Translated half-normal distribution
#'
#' Density, distribution, quantile and random generation functions for a
#' half-normal with support \eqn{[mu, \infty)} and scale `sigma`.
#' @param x,q Vector of quantiles.
#' @param mu Endpoint (location).
#' @param sigma Positive scale parameter.
#' @param log,log.p Return logarithms of densities or probabilities.
#' @param lower.tail If `TRUE`, probabilities are lower-tail probabilities.
#' @param p Vector of probabilities.
#' @param n Number of observations; a vector uses its length.
#' @return A numeric vector of densities, probabilities, quantiles or draws.
#' @name halfnorm
#' @export
dhalfnorm <- function(x, mu = 0, sigma = 1, log = FALSE) {
  extraDistr::dhnorm(x - mu, sigma = sigma, log = log)
}

#' @rdname halfnorm
#' @export
phalfnorm <- function(q, mu = 0, sigma = 1, lower.tail = TRUE, log.p = FALSE) {
  extraDistr::phnorm(q - mu, sigma = sigma, lower.tail = lower.tail, log.p = log.p)
}

#' @rdname halfnorm
#' @export
qhalfnorm <- function(p, mu = 0, sigma = 1, lower.tail = TRUE, log.p = FALSE) {
  mu + extraDistr::qhnorm(p, sigma = sigma, lower.tail = lower.tail, log.p = log.p)
}

#' @rdname halfnorm
#' @export
rhalfnorm <- function(n, mu = 0, sigma = 1) {
  mu + extraDistr::rhnorm(n, sigma = sigma)
}
