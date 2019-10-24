#' Parametric Bootstrap on Distributions Fitted with Maximum Likelihood
#'
#' @param obj A \code{univariateML} object.
#' @param Nreps Positive integer. The number of bootstrap samples.
#' @param map A function of the parameters of the \code{univariateML} object.
#'   Defaults to the identity.
#' @param reducer A reducer function. Defaults to \code{stats::quantile} with
#'   default argument \code{probs = c(0.025, 0.975)}.
#' @param ... Passed to reduce.
#' @return The transposed map-reduced bootstrap samples.
#' @export
#' @examples
#' \dontrun{
#'   set.seed(1)
#'   obj = mlgamma(mtcars$qsec)
#'
#'   ## Calculate c(0.025, 0.975) confidence interval for the gamma parameters.
#'   bootstrapml(obj)
#'
#'   #            2.5%      97.5%
#'   # shape 68.624945 160.841557
#'   # rate   3.896915   9.089194
#'
#'   ## The mean of a gamma distribution is shape/rate. Now we calculate a
#'   ## parametric bootstrap confidence interval for the mean with confidence
#'   ## limits c(0.05, 0.95)
#'
#'   bootstrapml(obj, map = function(x) x[1]/x[2], probs = c(0.05, 0.95))
#'
#'   #       5%      95%
#'   # 17.33962 18.31253
#'
#'   ## Print a histogram of the bootstrapped estimates from an exponential.
#'   obj = mlexp(mtcars$qsec)
#'   hist(bootstrapml(obj, reducer = identity))
#' }

bootstrapml = function(obj, Nreps = 1000, map = identity,
                       reducer = stats::quantile, ...) {

  r_fun = univariateML_to_function(obj, type = "r")
  ml_fun = univariateML_to_function(obj, type = "ml")
  bootstraps = replicate(n = Nreps, expr = ml_fun(r_fun(attr(obj, "n"))))

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