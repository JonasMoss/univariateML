#' STD distribution
#'
#' Density, distribution, quantile and random generation in the fGarch
#' mean and standard deviation parameterisation.
#' @param x,q Numeric observations or quantiles.
#' @param p Probabilities.
#' @param n Number of draws; a vector uses its length.
#' @param mean Finite mean.
#' @param sd Finite positive standard deviation.
#' @param nu Finite shape, greater than two.
#' @param log,log.p Return or supply logarithmic probabilities.
#' @param lower.tail Use the lower tail if TRUE, the upper tail otherwise.
#' @details Numeric inputs recycle, zero-length inputs return numeric(0), and
#'   missing values propagate. Invalid parameters yield NaN with a warning.
#'   Logical flags use their first element. Densities and small tails are
#'   evaluated directly on the log scale. Random generation uses R's RNG.
#' @return A numeric vector.
#' @name std-distribution
NULL

#' @rdname std-distribution
#' @export
dstd <- function(x, mean = 0, sd = 1, nu = 5, log = FALSE) {
  dstd_(x, mean, sd, nu, as.logical(log)[1L])
}

#' @rdname std-distribution
#' @export
pstd <- function(q, mean = 0, sd = 1, nu = 5, lower.tail = TRUE, log.p = FALSE) {
  pstd_(q, mean, sd, nu, as.logical(lower.tail)[1L], as.logical(log.p)[1L])
}

#' @rdname std-distribution
#' @export
qstd <- function(p, mean = 0, sd = 1, nu = 5, lower.tail = TRUE, log.p = FALSE) {
  qstd_(p, mean, sd, nu, as.logical(lower.tail)[1L], as.logical(log.p)[1L])
}

#' @rdname std-distribution
#' @export
rstd <- function(n, mean = 0, sd = 1, nu = 5) {
  n <- univariateML_rng_length(n)
  if (!n || any(lengths(list(mean, sd, nu)) == 0L)) return(numeric())
  mean <- rep_len(mean, n)
  sd <- rep_len(sd, n)
  nu <- rep_len(nu, n)
  rstd_(n, mean, sd, nu)
}
