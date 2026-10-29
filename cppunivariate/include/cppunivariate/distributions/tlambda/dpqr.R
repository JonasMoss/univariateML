#' Tukey lambda distribution
#'
#' Density, distribution, quantiles and random generation for
#' Q(u) = mu + sigma * (u^lambda - (1-u)^lambda) / lambda,
#' with the analytic logistic limit at lambda = 0.
#' @param x,q Numeric observations or quantiles.
#' @param p Probabilities.
#' @param n Number of draws; a vector supplies its length.
#' @param mu Finite location.
#' @param sigma Positive finite scale.
#' @param lambda Finite shape (evaluation allows all real shapes).
#' @param log,log.p Return or supply logarithmic probabilities.
#' @param lower.tail If TRUE use P(X <= q), otherwise P(X > q).
#' @details Numeric vectors recycle, missing values propagate, and invalid
#' parameters give NaN with a warning. Inversion solves for the smaller tail
#' in log probability using safeguarded Newton iterations. Random generation
#' uses R's uniform RNG, so set.seed controls reproducibility.
#' @return A numeric vector.
#' @name tlambda-distribution
NULL

#' @rdname tlambda-distribution
#' @export
dtlambda <- function(x, mu = 0, sigma = 1, lambda = 0, log = FALSE) {
  dtlambda_(x, mu, sigma, lambda, as.logical(log)[1L])
}

#' @rdname tlambda-distribution
#' @export
ptlambda <- function(q, mu = 0, sigma = 1, lambda = 0, lower.tail = TRUE, log.p = FALSE) {
  ptlambda_(q, mu, sigma, lambda, as.logical(lower.tail)[1L], as.logical(log.p)[1L])
}

#' @rdname tlambda-distribution
#' @export
qtlambda <- function(p, mu = 0, sigma = 1, lambda = 0, lower.tail = TRUE, log.p = FALSE) {
  qtlambda_(p, mu, sigma, lambda, as.logical(lower.tail)[1L], as.logical(log.p)[1L])
}

#' @rdname tlambda-distribution
#' @export
rtlambda <- function(n, mu = 0, sigma = 1, lambda = 0) {
  n <- univariateML_rng_length(n)
  if (!n || any(lengths(list(mu, sigma, lambda)) == 0L)) return(numeric())
  rtlambda_(n, rep_len(mu, n), rep_len(sigma, n), rep_len(lambda, n))
}
