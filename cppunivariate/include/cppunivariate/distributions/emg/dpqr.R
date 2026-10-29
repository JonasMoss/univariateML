#' Exponentially modified Gaussian distribution
#'
#' Density, CDF, quantiles and sampling for a normal plus independent exponential.
#' @param x,q Observations or quantiles.
#' @param p Probabilities.
#' @param n Number of draws; if length exceeds one its length is used.
#' @param mu Normal component mean.
#' @param sigma Positive normal component standard deviation.
#' @param lambda Positive exponential rate.
#' @param log,log.p Return or supply log probabilities.
#' @param lower.tail Use the lower tail if TRUE, upper tail otherwise.
#' @details Numeric arguments recycle; missing values propagate. Invalid
#' parameters or probabilities give NaN with a warning. Both tails are evaluated
#' in log scale. Quantiles use safeguarded Newton and bisection. Random draws
#' add independent normal and exponential draws using R's RNG.
#' @return A numeric vector.
#' @name emg-distribution
NULL

#' @rdname emg-distribution
#' @export
demg <- function(x, mu, sigma, lambda, log = FALSE) {
  demg_(x, mu, sigma, lambda, as.logical(log)[1L])
}

#' @rdname emg-distribution
#' @export
pemg <- function(q, mu, sigma, lambda, lower.tail = TRUE, log.p = FALSE) {
  pemg_(q, mu, sigma, lambda, as.logical(lower.tail)[1L], as.logical(log.p)[1L])
}

#' @rdname emg-distribution
#' @export
qemg <- function(p, mu, sigma, lambda, lower.tail = TRUE, log.p = FALSE) {
  qemg_(p, mu, sigma, lambda, as.logical(lower.tail)[1L], as.logical(log.p)[1L])
}

#' @rdname emg-distribution
#' @export
remg <- function(n, mu, sigma, lambda) {
  n <- univariateML_rng_length(n)
  if (!n || any(lengths(list(mu, sigma, lambda)) == 0L)) return(numeric())
  mu <- rep_len(mu, n)
  sigma <- rep_len(sigma, n)
  lambda <- rep_len(lambda, n)
  remg_(n, mu, sigma, lambda)
}

