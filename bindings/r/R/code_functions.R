#' Maximum likelihood estimated distribution
#'
#' Density, distribution function, quantile function and random generation for
#'    a univariate distribution estimated by maximum likelihood.
#'
#' `dml` is the density, `pml` is the distribution function,
#' `qml` is the quantile function, and `rml` is the random variable
#' generator.
#'
#' These functions work like their counterparts in `stats`, e.g.
#' [Normal][stats::Normal]. The `univariateML` object contains both maximum
#' likelihood estimates and the identity of the model these estimates were
#' calculated under. These functions are wrappers around underlying density,
#' distribution, quantile and random generation functions where unknown
#' parameters are filled with the maximum likelihood estimates.
#' They also accept fixed models built by calling a fitter without data,
#' for example `mlgamma(shape = 3)`. Unspecified coefficients then use
#' the defaults listed on that fitter's help page.
#'
#' @details
#' For discrete models, `qml` and `rml` return exact integers up to 2^53
#' (about 9.0e15); beyond that, the smallest double whose distribution
#' function reaches `p`. `NaN` means the evaluation failed.
#'
#' @param x,q vector of quantiles.
#' @param p vector of probabilities.
#' @param n number of observations. If `length(n) > 1`, the length is
#'     taken to be the number required.
#' @param obj an `univariateML` object.
#' @param log,log.p logical; if `TRUE`, the probabilities p are gives as
#'     `log(p)`.
#' @param lower.tail logical; if `TRUE` (default), the probabilities are
#'     \eqn{P[X \le x]} otherwise, \eqn{P[X > x]}
#' @return `dml` gives the density, `pml` gives the distribution
#'     function, `qml` gives the quantile function, and `rml`
#'     generates random deviates.
#' @examples
#' fixed <- mlgamma(shape = 3)
#' rml(5, fixed)
#' dml(c(1, 2), fixed)
#'
#' ## Simple example
#' obj <- mlnorm(airquality$Wind)
#' all.equal(dml(0.5, obj), dnorm(0.5, mean = obj[1], sd = obj[2]),
#'   tolerance = 1e-10)
#'
#' obj <- mlweibull(airquality$Wind)
#'
#' # Plot the logarithm of the Weibull density.
#' plot(function(x) dml(x, obj, log = TRUE),
#'   from = 0, to = 20,
#'   main = "Logarithm of Density", ylab = NA, lwd = 2
#' )
#' @name MaximumLikelihoodDistribution
#' @export
dml <- function(x, obj, log = FALSE) {
  fun <- univariateML_to_function(obj, type = "d")
  if (!("log" %in% names(formals(fun)))) {
    log(fun(x = x))
  } else {
    fun(x = x, log = log)
  }
}

#' @rdname MaximumLikelihoodDistribution
#' @export
pml <- function(q = q, obj, lower.tail = TRUE, log.p = FALSE) {
  fun <- univariateML_to_function(obj, type = "p")
  if (!all(c("log.p", "lower.tail") %in% names(formals(fun)))) {
    p <- fun(q = q)
    if (!lower.tail) p <- 1 - p
    if (log.p) p <- log(p)
    p
  } else {
    fun(q = q, lower.tail = lower.tail, log.p = log.p)
  }
}

#' @rdname MaximumLikelihoodDistribution
#' @export
qml <- function(p = p, obj, lower.tail = TRUE, log.p = FALSE) {
  fun <- univariateML_to_function(obj, type = "q")
  if (!all(c("log.p", "lower.tail") %in% names(formals(fun)))) {
    if (!lower.tail) p <- 1 - p
    if (log.p) p <- exp(p)
    fun(p = p)
  } else {
    fun(p = p, lower.tail = lower.tail, log.p = log.p)
  }
}

#' @rdname MaximumLikelihoodDistribution
#' @export
rml <- function(n = n, obj) {
  # By position: stats::rhyper names the sample size `nn`, and `n` is a parameter.
  univariateML_to_function(obj, type = "r")(n)
}
