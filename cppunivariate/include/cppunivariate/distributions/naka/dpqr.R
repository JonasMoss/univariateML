#' naka distribution
#'
#' Density, distribution, quantile and random generation functions.
#' @param x,q Numeric observations.
#' @param p Numeric probabilities.
#' @param n Number of draws; a vector supplies its length.
#' @param shape,scale Positive shape and scale; scale is the mean squared amplitude.
#' @param log,log.p Return or supply logarithmic probabilities.
#' @param lower.tail Use the lower tail if TRUE, the upper tail otherwise.
#' @details Numeric arguments recycle, zero length returns numeric(0), and
#' missing values propagate. Invalid parameters give NaN with a warning.
#' Parameters must be finite. Random generation transforms draws from R's RNG.
#' @return A numeric vector.
#' @name naka-distribution
NULL

#' @rdname naka-distribution
#' @export
dnaka <- function(x, shape, scale, log = FALSE) {
  dnaka_(x, shape, scale, as.logical(log)[1L])
}

#' @rdname naka-distribution
#' @export
pnaka <- function(q, shape, scale, lower.tail = TRUE, log.p = FALSE) {
  pnaka_(q, shape, scale, as.logical(lower.tail)[1L], as.logical(log.p)[1L])
}

#' @rdname naka-distribution
#' @export
qnaka <- function(p, shape, scale, lower.tail = TRUE, log.p = FALSE) {
  qnaka_(p, shape, scale, as.logical(lower.tail)[1L], as.logical(log.p)[1L])
}

#' @rdname naka-distribution
#' @export
rnaka <- function(n, shape, scale) {
  n <- univariateML_rng_length(n)
  if (!n || !length(shape) || !length(scale)) return(numeric())
  shape <- rep_len(shape, n)
  scale <- rep_len(scale, n)
  rnaka_(n, shape, scale)
}

