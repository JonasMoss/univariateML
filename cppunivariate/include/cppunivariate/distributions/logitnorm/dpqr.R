#' logitnorm distribution
#'
#' Density, distribution, quantile and random generation functions.
#' @param x,q Numeric observations.
#' @param p Numeric probabilities.
#' @param n Number of draws; a vector supplies its length.
#' @param mu,sigma Normal location and positive scale on the logit scale.
#' @param log,log.p Return or supply logarithmic probabilities.
#' @param lower.tail Use the lower tail if TRUE, the upper tail otherwise.
#' @details Numeric arguments recycle, zero length returns numeric(0), and
#' missing values propagate. Invalid parameters give NaN with a warning.
#' Parameters must be finite. Random generation transforms draws from R's RNG.
#' @param ... Additional arguments, retained for reference API compatibility.
#' @return A numeric vector.
#' @name logitnorm-distribution
NULL

#' @rdname logitnorm-distribution
#' @export
dlogitnorm <- function(x, mu = 0, sigma = 1, log = FALSE, ...) {
  dlogitnorm_(x, mu, sigma, as.logical(log)[1L])
}

#' @rdname logitnorm-distribution
#' @export
plogitnorm <- function(q, mu = 0, sigma = 1, lower.tail = TRUE, log.p = FALSE, ...) {
  plogitnorm_(q, mu, sigma, as.logical(lower.tail)[1L], as.logical(log.p)[1L])
}

#' @rdname logitnorm-distribution
#' @export
qlogitnorm <- function(p, mu = 0, sigma = 1, lower.tail = TRUE, log.p = FALSE, ...) {
  qlogitnorm_(p, mu, sigma, as.logical(lower.tail)[1L], as.logical(log.p)[1L])
}

#' @rdname logitnorm-distribution
#' @export
rlogitnorm <- function(n, mu = 0, sigma = 1, ...) {
  n <- univariateML_rng_length(n)
  if (!n || !length(mu) || !length(sigma)) return(numeric())
  mu <- rep_len(mu, n)
  sigma <- rep_len(sigma, n)
  rlogitnorm_(n, mu, sigma)
}

