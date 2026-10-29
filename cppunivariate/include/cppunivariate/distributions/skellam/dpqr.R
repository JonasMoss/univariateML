#' Skellam distribution
#'
#' Difference of independent Poisson counts with nonnegative finite rates.
#' Zero rates give the corresponding signed Poisson member.
#' @param x,q Numeric vector of observations or quantiles.
#' @param p Numeric vector of probabilities.
#' @param n Number of draws; a vector requests its length.
#' @param mu1,mu2 Nonnegative finite Poisson rates.
#' @param log,log.p Return or supply log probabilities.
#' @param lower.tail Use P(X <= q) if TRUE, P(X > q) otherwise.
#' @details Arguments recycle; missing values propagate. Invalid arguments
#' give NaN with a warning. Noninteger density arguments give zero with a
#' warning; CDF arguments are floored. Quantiles are the smallest integer
#' reaching the requested probability. Infinite quantiles denote unbounded
#' support endpoints. Log tails are evaluated directly. Random generation
#' subtracts independent Poisson draws using R's RNG.
#' @return A numeric vector.
#' @name skellam-distribution
NULL

#' @rdname skellam-distribution
#' @export
dskellam <- function(x, mu1, mu2, log = FALSE) {
  dskellam_(x, mu1, mu2, as.logical(log)[1L])
}

#' @rdname skellam-distribution
#' @export
pskellam <- function(q, mu1, mu2, lower.tail = TRUE, log.p = FALSE) {
  pskellam_(q, mu1, mu2, as.logical(lower.tail)[1L], as.logical(log.p)[1L])
}

#' @rdname skellam-distribution
#' @export
qskellam <- function(p, mu1, mu2, lower.tail = TRUE, log.p = FALSE) {
  qskellam_(p, mu1, mu2, as.logical(lower.tail)[1L], as.logical(log.p)[1L])
}

#' @rdname skellam-distribution
#' @export
rskellam <- function(n, mu1, mu2) {
  n <- univariateML_rng_length(n)
  if (!n || any(lengths(list(mu1, mu2)) == 0L)) return(numeric())
  mu1 <- rep_len(mu1, n)
  mu2 <- rep_len(mu2, n)
  rskellam_(n, mu1, mu2)
}

