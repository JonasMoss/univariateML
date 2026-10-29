#' Zeta distribution
#'
#' Density, distribution, quantile and random generation functions.
#'
#' @param x,q Numeric vector of observations or quantiles.
#' @param p Numeric vector of probabilities.
#' @param n Number of observations. If length exceeds one, its length is used.
#' @param s Finite shape in (1, inf).
#' @param log,log.p Return or supply logarithmic probabilities.
#' @param lower.tail If TRUE, use P(X <= q); otherwise use P(X > q).
#' @details Numeric arguments are recycled to their maximum length (zero length
#' returns numeric(0)); logical flags use their first element. Missing values
#' propagate. Invalid parameters or probabilities produce NaN with a warning;
#' noninteger density observations produce zero with a warning. CDF arguments
#' are floored. Quantiles are the smallest support point k with F(k) >= p
#' (or P(X > k) <= p for upper tails). Tails are evaluated directly, including
#' on the logarithmic scale. Random draws use inversion and R's RNG.
#' @return A numeric vector of probabilities, quantiles or random draws.
#' @details Support starts at one and has no finite upper endpoint. Quantiles
#' beyond 2^53 use the representable double grid; a result beyond the largest
#' finite double is Inf. At that scale, adjacent integers cannot be distinguished.
#' @name zeta-distribution
NULL

#' @rdname zeta-distribution
#' @export
dzeta <- function(x, s, log = FALSE) {
  dzeta_(x, s, as.logical(log)[1L])
}

#' @rdname zeta-distribution
#' @export
pzeta <- function(q, s, lower.tail = TRUE, log.p = FALSE) {
  pzeta_(q, s, as.logical(lower.tail)[1L], as.logical(log.p)[1L])
}

#' @rdname zeta-distribution
#' @export
qzeta <- function(p, s, lower.tail = TRUE, log.p = FALSE) {
  qzeta_(p, s, as.logical(lower.tail)[1L], as.logical(log.p)[1L])
}

#' @rdname zeta-distribution
#' @export
rzeta <- function(n, s) {
  n <- univariateML_rng_length(n)
  if (!n || !length(s)) return(numeric())
  s <- rep_len(s, n)
  rzeta_(n, s)
}

