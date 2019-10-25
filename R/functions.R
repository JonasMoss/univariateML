#' Maximum likelihood estimated distribution
#'
#' Density, distribution function, quantile function and random generation for
#'    a univariate distribution estimated by maximum likelihood.
#'
#'
#' @param x,q vector of quantiles.
#' @param p vector of probabilities.
#' @param n number of observations. If \code{length(n) > 1}, the length is
#'     taken to be the number required.
#' @param obj an \code{univariateML} object.
#' @param log,log.p logical; if \code{TRUE}, the probabilities p are gives as
#'     \code{log(p)}.
#' @param lower.tail logical; if \code{TRUE} (default), the probabilites are
#'     \eqn{P[X \le x]} otherwise, \eqn{P[X > x]}
#' @return \code{dml} gives the density, \code{pml} gives the distribution
#'     function, \code{qml} gives the quantile function, and \code{rml}
#'     generates random deviates.
#' @examples
#'     ##
#' @name MaximumLikelihood
#' @export
dml = function(x, obj, log = FALSE)
  univariateML_to_function(obj, type = "d")(x = x, log = log)

#' @rdname MaximumLikelihood
#' @export
pml = function(q = q, obj, lower.tail = TRUE, log.p = FALSE)
  univariateML_to_function(obj, type = "p")(q = q, lower.tail = lower.tail, log.p = log.p)

#' @rdname MaximumLikelihood
#' @export
qml = function(p = p, obj, lower.tail = TRUE, log.p = FALSE)
  univariateML_to_function(obj, type = "q")(p = p, lower.tail = lower.tail, log.p = log.p)

#' @rdname MaximumLikelihood
#' @export
rml = function(n = n, obj)
  univariateML_to_function(obj, type = "r")(n = n)

