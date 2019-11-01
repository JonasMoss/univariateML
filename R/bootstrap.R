#' Parametric Bootstrap on Distributions Fitted with Maximum Likelihood
#'
#' The parametric bootstrap is a resampling technique using random variates from
#'    a known parametric distribution. In this function the distribution of the
#'    random variates is completely determined by the \code{unvariateML} object
#'    \code{object}.
#'
#' For each bootstrap iteration a maximum likelihood estimate is calculated using
#'    the \code{ml***} function specified by \code{object}. The resulting numeric
#'    vector is then passed to \code{map}. The values returned by \code{map} is collected in
#'    an array and the \code{reducer} is called on each row of the array.
#'
#' By default the \code{map} function is the identity and the default \code{reducer} is
#'    the quantile function taking the argument \code{probs}, which defaults to \code{c(0.025, 0.975)}.
#'    This corresponds to a 95\% basic percentile confidence interval and is also reported
#'    by \code{\link{confint}}
#'
#' \emph{Note:} The default confidence intervals are percentile intervals, not empirical intervals.
#'    These confidence intervals will in some cases have poor coverage as they are not studentized,
#'    see e.g. Carpenter, J., & Bithell, J. (2000).
#'
#' @param object A \code{univariateML} object.
#' @param Nreps Positive integer. The number of bootstrap samples.
#' @param map A function of the parameters of the \code{univariateML} object.
#'   Defaults to the identity.
#' @param reducer A reducer function. Defaults to \code{stats::quantile} with
#'   default argument \code{probs = c(0.025, 0.975)}.
#' @param ... Passed to \code{reducer}.
#' @return The transposed map-reduced bootstrap samples.
#' @export
#' @references
#'   Efron, B., & Tibshirani, R. J. (1994). An introduction to the bootstrap. CRC press.
#'
#'   Carpenter, J., & Bithell, J. (2000). Bootstrap confidence intervals: when, which, what? A practical guide for medical statisticians. Statistics in medicine, 19(9), 1141-1164.
#' @seealso \code{\link{confint}} for an application of \code{bootstrapml}.
#' @examples
#' \dontrun{
#'   set.seed(1)
#'   object = mlgamma(mtcars$qsec)
#'
#'   ## Calculate c(0.025, 0.975) confidence interval for the gamma parameters.
#'   bootstrapml(object)
#'
#'   #            2.5%      97.5%
#'   # shape 68.624945 160.841557
#'   # rate   3.896915   9.089194
#'
#'   ## The mean of a gamma distribution is shape/rate. Now we calculate a
#'   ## parametric bootstrap confidence interval for the mean with confidence
#'   ## limits c(0.05, 0.95)
#'
#'   bootstrapml(object, map = function(x) x[1]/x[2], probs = c(0.05, 0.95))
#'
#'   #       5%      95%
#'   # 17.33962 18.31253
#'
#'   ## Print a histogram of the bootstrapped estimates from an exponential.
#'   object = mlexp(mtcars$qsec)
#'   hist(bootstrapml(object, reducer = identity))
#' }

bootstrapml = function(object, Nreps = 1000, map = identity,
                       reducer = stats::quantile, ...) {

  r_fun = univariateML_to_function(object, type = "r")
  ml_fun = univariateML_to_function(object, type = "ml")
  bootstraps = replicate(n = Nreps, expr = ml_fun(r_fun(attr(object, "n"))))

  defaults = list()

  if(identical(reducer, stats::quantile)) defaults = list(probs = c(0.025, 0.975))

  arguments = listmerge(x = defaults, y = list(...))

  if(is.null(dim(bootstraps))) {
    mapped = sapply(bootstraps, map)
  } else {
    mapped = apply(bootstraps, 2, map)
  }

  if(is.null(dim(mapped))) {
    do.call(function(...) reducer(mapped, ...), arguments)
  } else {
    do.call(function(...) t(apply(mapped, 1, reducer, ...)), arguments)
  }

}