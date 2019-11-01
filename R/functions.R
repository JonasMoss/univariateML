#' Maximum likelihood estimated distribution
#'
#' Density, distribution function, quantile function and random generation for
#'    a univariate distribution estimated by maximum likelihood.
#'
#' \code{dml} is the density, \code{pml} is the distribution function,
#' \code{qml} is the quantile function, and \code{rml} is the random variable
#' generator.
#'
#' These functions work like their counterparts in `stats`, e.g. \link[stats]{Normal}.
#' The \code{univariateML} object contains both maximum likelihood estimates
#' and the identity of the model these estimates were calculated under. These
#' functions are wrappers around underlying density, distribution, quantile and
#' random generation functions where unknown parameters are filled with the
#' maximum likelihood estimates. See the example.
#'
#' @param x,q vector of quantiles.
#' @param p vector of probabilities.
#' @param n number of observations. If \code{length(n) > 1}, the length is
#'     taken to be the number required.
#' @param obj an \code{univariateML} object.
#' @param log,log.p logical; if \code{TRUE}, the probabilities p are gives as
#'     \code{log(p)}.
#' @param lower.tail logical; if \code{TRUE} (default), the probabilities are
#'     \eqn{P[X \le x]} otherwise, \eqn{P[X > x]}
#' @return \code{dml} gives the density, \code{pml} gives the distribution
#'     function, \code{qml} gives the quantile function, and \code{rml}
#'     generates random deviates.
#' @examples
#'   ## Simple example
#'   obj = mlnorm(airquality$Wind)
#'   dml(0.5, obj) == dnorm(0.5, mean = obj[1], sd = obj[2])
#'
#'   ## We study the Beta prime model applied to the airquality data set.
#'   obj = mlbetapr(airquality$Wind)
#'
#'   ## Example copied from 'stats::dnorm'.
#'   par(mfrow = c(2,1))
#'   plot(function(x) dml(x, obj, log = TRUE), from = 0, to = 20,
#'        main = "Logarithm of Density", ylab = NA, lwd = 2)
#'   curve(log(dml(x, obj)), add = TRUE, col = "red", lwd = 2, lty = 2)
#'   mtext("dml(x, obj, log = TRUE)", adj = 0)
#'   mtext("log(dml(x, obj))", col = "red", adj = 1)
#'
#'   plot(function(x) pml(x, obj, log = TRUE), from = 0, to = 20,
#'        main = "Logarithm of Cumulative Probability", ylab = NA, lwd = 2)
#'   curve(log(pml(x, obj)), add = TRUE, col = "red", lwd = 2, lty = 2)
#'   mtext("pml(x, obj, log = TRUE)", adj = 0)
#'   mtext("log(pml(x, obj))", col = "red", adj = 1)
#'
#' @name MaximumLikelihoodDistribution
#' @export
dml = function(x, obj, log = FALSE)
  univariateML_to_function(obj, type = "d")(x = x, log = log)

#' @rdname MaximumLikelihoodDistribution
#' @export
pml = function(q = q, obj, lower.tail = TRUE, log.p = FALSE)
  univariateML_to_function(obj, type = "p")(q = q, lower.tail = lower.tail, log.p = log.p)

#' @rdname MaximumLikelihoodDistribution
#' @export
qml = function(p = p, obj, lower.tail = TRUE, log.p = FALSE)
  univariateML_to_function(obj, type = "q")(p = p, lower.tail = lower.tail, log.p = log.p)

#' @rdname MaximumLikelihoodDistribution
#' @export
rml = function(n = n, obj)
  univariateML_to_function(obj, type = "r")(n = n)

