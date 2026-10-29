#' Shifted Gompertz distribution
#'
#' Density, distribution, quantile and random generation functions for the
#' shifted Gompertz family, including the exponential member eta = 0.
#' There is no location parameter; b is a rate.
#' @param x,q Numeric vector of observations or quantiles.
#' @param p Numeric vector of probabilities.
#' @param n Number of observations; if length exceeds one, its length is used.
#' @param b Positive finite rate.
#' @param eta Nonnegative finite shape.
#' @param log,log.p Return or supply logarithmic probabilities.
#' @param lower.tail If TRUE use P(X <= q), otherwise P(X > q).
#' @details Numeric arguments recycle to their maximum length; zero length
#' returns numeric(0). Missing values propagate. Invalid parameters and
#' probabilities produce NaN with a warning. Logical flags use their first
#' element. Both tails are evaluated directly, including on the log scale.
#' Quantiles use safeguarded bisection. Random generation takes the maximum
#' of an exponential and a Gumbel variate using R's RNG; eta = 0 uses only
#' the exponential variate.
#' @return Numeric vector of densities, probabilities, quantiles or draws.
#' @name sgomp-distribution
NULL
#' @rdname sgomp-distribution
#' @export
dsgomp <- function(x, b, eta, log = FALSE) {
  dsgomp_(x, b, eta, as.logical(log)[1L])
}
#' @rdname sgomp-distribution
#' @export
psgomp <- function(q, b, eta, lower.tail = TRUE, log.p = FALSE) {
  psgomp_(q, b, eta, as.logical(lower.tail)[1L], as.logical(log.p)[1L])
}
#' @rdname sgomp-distribution
#' @export
qsgomp <- function(p, b, eta, lower.tail = TRUE, log.p = FALSE) {
  qsgomp_(p, b, eta, as.logical(lower.tail)[1L], as.logical(log.p)[1L])
}
#' @rdname sgomp-distribution
#' @export
rsgomp <- function(n, b, eta) {
  n <- univariateML_rng_length(n)
  if (!n || any(lengths(list(b, eta)) == 0L)) return(numeric())
  rsgomp_(n, rep_len(b, n), rep_len(eta, n))
}
