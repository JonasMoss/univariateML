#' Yule--Simon distribution
#'
#' Density, distribution, quantile and random generation functions.
#'
#' @param x,q Numeric vector of observations or quantiles.
#' @param p Numeric vector of probabilities.
#' @param n Number of observations. If length exceeds one, its length is used.
#' @param rho Finite shape in (0, inf).
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
#' @name yulesimon-distribution
NULL

#' @rdname yulesimon-distribution
#' @export
dyulesimon <- function(x, rho, log = FALSE) {
  dyulesimon_(x, rho, as.logical(log)[1L])
}

#' @rdname yulesimon-distribution
#' @export
pyulesimon <- function(q, rho, lower.tail = TRUE, log.p = FALSE) {
  pyulesimon_(q, rho, as.logical(lower.tail)[1L], as.logical(log.p)[1L])
}

#' @rdname yulesimon-distribution
#' @export
qyulesimon <- function(p, rho, lower.tail = TRUE, log.p = FALSE) {
  qyulesimon_(p, rho, as.logical(lower.tail)[1L], as.logical(log.p)[1L])
}

#' @rdname yulesimon-distribution
#' @export
ryulesimon <- function(n, rho) {
  n <- univariateML_rng_length(n)
  if (!n || !length(rho)) return(numeric())
  rho <- rep_len(rho, n)
  ryulesimon_(n, rho)
}

