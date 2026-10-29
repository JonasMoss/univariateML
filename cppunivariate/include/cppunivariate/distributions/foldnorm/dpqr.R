#' Folded normal distribution
#'
#' Density, distribution, quantile and random generation for the absolute
#' value of a normal variable with nonnegative location mu and positive scale sigma.
#' The sign of the parent location is unidentifiable. mu = 0 is half-normal.
#' @param x,q Numeric observations or quantiles.
#' @param p Numeric probabilities.
#' @param n Number of observations; if length exceeds one, its length is used.
#' @param mu Nonnegative finite parent normal location.
#' @param sigma Positive finite parent normal scale.
#' @param log,log.p Return or supply logarithmic probabilities.
#' @param lower.tail If TRUE use P(X <= q), otherwise P(X > q).
#' @details Numeric arguments recycle to their maximum length; zero lengths
#' return numeric(0). Missing values propagate. Invalid parameters and
#' probabilities produce NaN with a warning. Logical flags use their first
#' element. Both tails are evaluated directly, including logarithmic tails.
#' Quantiles use safeguarded bisection. Random draws are abs(rnorm(n, mu, sigma))
#' and use R's RNG, controlled by set.seed().
#' @return A numeric vector of densities, probabilities, quantiles or draws.
#' @name foldnorm-distribution
NULL

#' @rdname foldnorm-distribution
#' @export
dfoldnorm <- function(x, mu = 0, sigma = 1, log = FALSE) {
  dfoldnorm_(x, mu, sigma, as.logical(log)[1L])
}

#' @rdname foldnorm-distribution
#' @export
pfoldnorm <- function(q, mu = 0, sigma = 1, lower.tail = TRUE, log.p = FALSE) {
  pfoldnorm_(q, mu, sigma, as.logical(lower.tail)[1L], as.logical(log.p)[1L])
}

#' @rdname foldnorm-distribution
#' @export
qfoldnorm <- function(p, mu = 0, sigma = 1, lower.tail = TRUE, log.p = FALSE) {
  qfoldnorm_(p, mu, sigma, as.logical(lower.tail)[1L], as.logical(log.p)[1L])
}

#' @rdname foldnorm-distribution
#' @export
rfoldnorm <- function(n, mu = 0, sigma = 1) {
  n <- univariateML_rng_length(n)
  if (!n || any(lengths(list(mu, sigma)) == 0L)) return(numeric())
  rfoldnorm_(n, rep_len(mu, n), rep_len(sigma, n))
}
