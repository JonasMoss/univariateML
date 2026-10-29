#' SNORM distribution
#'
#' Density, distribution, quantile and random generation in the fGarch
#' mean and standard deviation parameterisation.
#' @param x,q Numeric observations or quantiles.
#' @param p Probabilities.
#' @param n Number of draws; a vector uses its length.
#' @param mean Finite mean.
#' @param sd Finite positive standard deviation.
#' @param xi Finite positive Fernandez--Steel skewness.
#' @param log,log.p Return or supply logarithmic probabilities.
#' @param lower.tail Use the lower tail if TRUE, the upper tail otherwise.
#' @details Numeric inputs recycle, zero-length inputs return numeric(0), and
#'   missing values propagate. Invalid parameters yield NaN with a warning.
#'   Logical flags use their first element. Densities and small tails are
#'   evaluated directly on the log scale. Random generation uses R's RNG.
#' @return A numeric vector.
#' @name snorm-distribution
NULL

#' @rdname snorm-distribution
#' @export
dsnorm <- function(x, mean = 0, sd = 1, xi = 1.5, log = FALSE) {
  dsnorm_(x, mean, sd, xi, as.logical(log)[1L])
}

#' @rdname snorm-distribution
#' @export
psnorm <- function(q, mean = 0, sd = 1, xi = 1.5, lower.tail = TRUE, log.p = FALSE) {
  psnorm_(q, mean, sd, xi, as.logical(lower.tail)[1L], as.logical(log.p)[1L])
}

#' @rdname snorm-distribution
#' @export
qsnorm <- function(p, mean = 0, sd = 1, xi = 1.5, lower.tail = TRUE, log.p = FALSE) {
  qsnorm_(p, mean, sd, xi, as.logical(lower.tail)[1L], as.logical(log.p)[1L])
}

#' @rdname snorm-distribution
#' @export
rsnorm <- function(n, mean = 0, sd = 1, xi = 1.5) {
  n <- univariateML_rng_length(n)
  if (!n || any(lengths(list(mean, sd, xi)) == 0L)) return(numeric())
  mean <- rep_len(mean, n)
  sd <- rep_len(sd, n)
  xi <- rep_len(xi, n)
  rsnorm_(n, mean, sd, xi)
}
