#' Azzalini skew-normal distribution
#'
#' Density, distribution, quantile and random generation in direct parameters.
#' This differs from the Fernandez--Steel two-piece normal (mlsnorm).
#' @param x,q Numeric observations or quantiles.
#' @param p Probabilities.
#' @param n Number of draws; a vector supplies its length.
#' @param xi Finite location.
#' @param omega Positive finite scale.
#' @param alpha Finite real shape.
#' @param log,log.p Return or supply logarithmic probabilities.
#' @param lower.tail If TRUE use P(X <= q), otherwise P(X > q).
#' @details Numeric arguments recycle; zero lengths give numeric(0). Missing
#' values propagate. Invalid parameters and probabilities give NaN with a warning.
#' Tail probabilities use Owen's T or direct scaled density quadrature to avoid
#' cancellation. Quantiles use bracketed bisection. Random draws use two normal
#' variates from R's RNG, so set.seed controls reproducibility.
#' @return A numeric vector.
#' @name sn-distribution
NULL

#' @rdname sn-distribution
#' @export
dsn <- function(x, xi = 0, omega = 1, alpha = 0, log = FALSE) {
  dsn_(x, xi, omega, alpha, as.logical(log)[1L])
}

#' @rdname sn-distribution
#' @export
psn <- function(q, xi = 0, omega = 1, alpha = 0, lower.tail = TRUE, log.p = FALSE) {
  psn_(q, xi, omega, alpha, as.logical(lower.tail)[1L], as.logical(log.p)[1L])
}

#' @rdname sn-distribution
#' @export
qsn <- function(p, xi = 0, omega = 1, alpha = 0, lower.tail = TRUE, log.p = FALSE) {
  qsn_(p, xi, omega, alpha, as.logical(lower.tail)[1L], as.logical(log.p)[1L])
}

#' @rdname sn-distribution
#' @export
rsn <- function(n, xi = 0, omega = 1, alpha = 0) {
  n <- univariateML_rng_length(n)
  if (!n || any(lengths(list(xi, omega, alpha)) == 0L)) return(numeric())
  rsn_(n, rep_len(xi, n), rep_len(omega, n), rep_len(alpha, n))
}
