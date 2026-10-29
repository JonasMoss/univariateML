#' Conway--Maxwell--Poisson distribution
#'
#' Density, distribution, quantile and random generation using the CMP normalizer.
#'
#' @param x,q Numeric vector of observations or quantiles.
#' @param p Numeric vector of probabilities.
#' @param n Number of observations. If length exceeds one, its length is used.
#' @param lambda Nonnegative rate coordinate; must be below one when nu is zero.
#' @param nu Nonnegative dispersion coordinate.
#' @param log,log.p Return or supply logarithmic probabilities.
#' @param lower.tail If TRUE, use P(X <= q); otherwise use P(X > q).
#' @details Numeric arguments are recycled to their maximum length (zero length
#' returns numeric(0)); logical flags use their first element. Missing values
#' propagate. Invalid parameters or probabilities produce NaN with a warning;
#' noninteger density observations produce zero with a warning. CDF arguments
#' are floored. Quantiles are the smallest support point k with F(k) >= p
#' (or P(X > k) <= p for upper tails). Tails are summed directly, including
#' on the logarithmic scale. Random draws use inversion and R's RNG.
#' @return A numeric vector of probabilities, quantiles or random draws.
#' @name cmp-distribution
NULL

#' @rdname cmp-distribution
#' @export
dcmp <- function(x, lambda, nu, log = FALSE) {
  dcmp_(x, lambda, nu, as.logical(log)[1L])
}

#' @rdname cmp-distribution
#' @export
pcmp <- function(q, lambda, nu, lower.tail = TRUE, log.p = FALSE) {
  pcmp_(q, lambda, nu, as.logical(lower.tail)[1L], as.logical(log.p)[1L])
}

#' @rdname cmp-distribution
#' @export
qcmp <- function(p, lambda, nu, lower.tail = TRUE, log.p = FALSE) {
  qcmp_(p, lambda, nu, as.logical(lower.tail)[1L], as.logical(log.p)[1L])
}

#' @rdname cmp-distribution
#' @export
rcmp <- function(n, lambda, nu) {
  n <- univariateML_rng_length(n)
  if (!n || any(lengths(list(lambda, nu)) == 0L)) return(numeric())
  lambda <- rep_len(lambda, n)
  nu <- rep_len(nu, n)
  rcmp_(n, lambda, nu)
}

