#' Finite Zipf distribution
#'
#' Density, distribution, quantile and random generation functions.
#' Support is 1 to N; N is a positive integer at most .Machine$integer.max,
#' and s is finite and nonnegative. The member s = 0 is uniform.
#'
#' @param x,q Numeric vector of observations or quantiles.
#' @param p Numeric vector of probabilities.
#' @param n Number of observations. If length exceeds one, its length is used.
#' @param N Upper support endpoint.
#' @param s Nonnegative shape.
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
#' @name zipf-distribution
NULL

#' @rdname zipf-distribution
#' @export
dzipf <- function(x, N, s, log = FALSE) {
  dzipf_(x, N, s, as.logical(log)[1L])
}

#' @rdname zipf-distribution
#' @export
pzipf <- function(q, N, s, lower.tail = TRUE, log.p = FALSE) {
  pzipf_(q, N, s, as.logical(lower.tail)[1L], as.logical(log.p)[1L])
}

#' @rdname zipf-distribution
#' @export
qzipf <- function(p, N, s, lower.tail = TRUE, log.p = FALSE) {
  qzipf_(p, N, s, as.logical(lower.tail)[1L], as.logical(log.p)[1L])
}

#' @rdname zipf-distribution
#' @export
rzipf <- function(n, N, s) {
  n <- univariateML_rng_length(n)
  if (!n || any(lengths(list(N, s)) == 0L)) return(numeric())
  N <- rep_len(N, n)
  s <- rep_len(s, n)
  rzipf_(n, N, s)
}

