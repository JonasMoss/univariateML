#' SSTD distribution
#'
#' Density, distribution, quantile and random generation in the fGarch
#' mean and standard deviation parameterisation.
#' @param x,q Numeric observations or quantiles.
#' @param p Probabilities.
#' @param n Number of draws; a vector uses its length.
#' @param mean Finite mean.
#' @param sd Finite positive standard deviation.
#' @param nu Finite shape, greater than two.
#' @param xi Finite positive Fernandez--Steel skewness.
#' @param log,log.p Return or supply logarithmic probabilities.
#' @param lower.tail Use the lower tail if TRUE, the upper tail otherwise.
#' @details Numeric inputs recycle, zero-length inputs return numeric(0), and
#'   missing values propagate. Invalid parameters yield NaN with a warning.
#'   Logical flags use their first element. Densities and small tails are
#'   evaluated directly on the log scale. Random generation uses R's RNG.
#' @return A numeric vector.
#' @name sstd-distribution
NULL

#' @rdname sstd-distribution
#' @export
dsstd <- function(x, mean = 0, sd = 1, nu = 5, xi = 1.5, log = FALSE) {
  dsstd_(x, mean, sd, nu, xi, as.logical(log)[1L])
}

#' @rdname sstd-distribution
#' @export
psstd <- function(q, mean = 0, sd = 1, nu = 5, xi = 1.5, lower.tail = TRUE, log.p = FALSE) {
  psstd_(q, mean, sd, nu, xi, as.logical(lower.tail)[1L], as.logical(log.p)[1L])
}

#' @rdname sstd-distribution
#' @export
qsstd <- function(p, mean = 0, sd = 1, nu = 5, xi = 1.5, lower.tail = TRUE, log.p = FALSE) {
  qsstd_(p, mean, sd, nu, xi, as.logical(lower.tail)[1L], as.logical(log.p)[1L])
}

#' @rdname sstd-distribution
#' @export
rsstd <- function(n, mean = 0, sd = 1, nu = 5, xi = 1.5) {
  n <- univariateML_rng_length(n)
  if (!n || any(lengths(list(mean, sd, nu, xi)) == 0L)) return(numeric())
  mean <- rep_len(mean, n)
  sd <- rep_len(sd, n)
  nu <- rep_len(nu, n)
  xi <- rep_len(xi, n)
  rsstd_(n, mean, sd, nu, xi)
}
