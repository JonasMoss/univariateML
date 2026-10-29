#' Conway--Maxwell--binomial distribution
#'
#' Density, distribution, quantile and random generation functions.
#' Support is 0 to size. size is a nonnegative integer at most 1e7;
#' prob lies between zero and one (inclusive), and nu is any finite real number. The member nu = 1
#' is binomial. Normalization enumerates the finite support in log scale.
#' Tables are shared by repeated parameter tuples within each call.
#'
#' @param x,q Numeric vector of observations or quantiles.
#' @param p Numeric vector of probabilities.
#' @param n Number of observations. If length exceeds one, its length is used.
#' @param size Known number of trials.
#' @param prob Canonical probability coordinate; generally not the mean divided by size.
#' @param nu Real dispersion coordinate.
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
#' @name cmb-distribution
NULL

#' @rdname cmb-distribution
#' @export
dcmb <- function(x, size, prob, nu, log = FALSE) {
  dcmb_(x, size, prob, nu, as.logical(log)[1L])
}

#' @rdname cmb-distribution
#' @export
pcmb <- function(q, size, prob, nu, lower.tail = TRUE, log.p = FALSE) {
  pcmb_(q, size, prob, nu, as.logical(lower.tail)[1L], as.logical(log.p)[1L])
}

#' @rdname cmb-distribution
#' @export
qcmb <- function(p, size, prob, nu, lower.tail = TRUE, log.p = FALSE) {
  qcmb_(p, size, prob, nu, as.logical(lower.tail)[1L], as.logical(log.p)[1L])
}

#' @rdname cmb-distribution
#' @export
rcmb <- function(n, size, prob, nu) {
  n <- univariateML_rng_length(n)
  if (!n || any(lengths(list(size, prob, nu)) == 0L)) return(numeric())
  size <- rep_len(size, n)
  prob <- rep_len(prob, n)
  nu <- rep_len(nu, n)
  rcmb_(n, size, prob, nu)
}

