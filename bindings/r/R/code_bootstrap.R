#' Parametric Bootstrap on Distributions Fitted with Maximum Likelihood
#'
#' The parametric bootstrap is a resampling technique using random variates
#'    from a known parametric distribution. In this function the distribution
#'    of the random variates is completely determined by the `unvariateML`
#'    object `object`.
#'
#' For each bootstrap iteration a maximum likelihood estimate is calculated
#'    using the `ml***` function specified by `object`. The resulting
#'    numeric vector is then passed to `map`. The values returned by
#'    `map` is collected in an array and the `reducer` is called on
#'    each row of the array.
#'
#' By default the `map` function is the identity and the default
#'    `reducer` is the quantile function taking the argument `probs`,
#'    which defaults to `c(0.025, 0.975)`. This corresponds to a 95\%
#'    basic percentile confidence interval and is also reported by
#'    [confint()]
#'
#' *Note:* The default confidence intervals are percentile intervals,
#'    not empirical intervals. These confidence intervals will in some cases
#'    have poor coverage as they are not studentized, see e.g. Carpenter,
#'    J., & Bithell, J. (2000).
#'
#' @param object A `univariateML` object.
#' @param reps Positive integer. The number of bootstrap samples.
#' @param map A function of the parameters of the `univariateML` object.
#'   Defaults to the identity.
#' @param reducer A reducer function. Defaults to `stats::quantile` with
#'   default argument `probs = c(0.025, 0.975)`.
#' @param ... Passed to `reducer`.
#' @param type Parametric simulation or nonparametric resampling of retained
#'   observations (whole interval rows for censored data). Censored parametric
#'   bootstrap requires a censoring mechanism and is refused.
#' @return The transposed map-reduced bootstrap samples.
#' @export
#' @references
#'   Efron, B., & Tibshirani, R. J. (1994). An introduction to the bootstrap.
#'   CRC press.
#'
#'   Carpenter, J., & Bithell, J. (2000). Bootstrap confidence intervals:
#'   when, which, what? A practical guide for medical statisticians.
#'   Statistics in medicine, 19(9), 1141-1164.
#'
#' @seealso [confint()] for an application of `bootstrapml`.
#' @examples
#' \donttest{
#' set.seed(1)
#' object <- mlgamma(mtcars$qsec)
#'
#' ## Calculate c(0.025, 0.975) confidence interval for the gamma parameters.
#' bootstrapml(object)
#'
#' #            2.5%      97.5%
#' # shape 68.624945 160.841557
#' # scale  0.110021   0.256613
#'
#' ## The mean of a gamma distribution is shape * scale. Now we calculate a
#' ## parametric bootstrap confidence interval for the mean with confidence
#' ## limits c(0.05, 0.95)
#'
#' bootstrapml(object, map = function(x) x[1] * x[2], probs = c(0.05, 0.95))
#'
#' #       5%      95%
#' # 17.33962 18.31253
#'
#' ## Print a histogram of the bootstrapped estimates from an exponential.
#' object <- mlexp(mtcars$qsec)
#' hist(bootstrapml(object, reducer = identity))
#' }
#'
bootstrapml <- function(
    object, reps = 1000, map = identity,
    reducer = stats::quantile, ..., type = c("parametric", "nonparametric")) {
  type <- match.arg(type)
  if (isTRUE(attr(object,"censored")) && type=="parametric")
    stop("Parametric bootstrap of censored data requires a censoring mechanism")
  ml_require_data(object)
  r_fun <- univariateML_to_function(object, type = "r")
  ml_fun <- univariateML_to_function(object, type = "ml")
  # Refit with the same parameters held fixed as in the original fit.
  fixed <- univariateML_fixed(object)
  held <- stats::setNames(as.list(as.numeric(object[fixed])), fixed)
  required <- univariateML_metadata[[univariateML_name(object)]]$required
  free_required <- setdiff(required, fixed)
  held[free_required] <- rep(list(NULL), length(free_required))
  if (!is.null(attr(object,"zero")) && attr(object,"zero")!="none") held$zero <- attr(object,"zero")
  if (!is.null(attr(object,"truncate"))) {
    held$truncate <- attr(object,"truncate_spec")
    if(is.null(held$truncate))held$truncate<-attr(object,"truncate")
  }
  draw <- function() {
    if(type=="parametric") return(r_fun(attr(object,"n")))
    data <- attr(object,"inference_data")
    if(is.null(data)) stop("Nonparametric bootstrap requires retained observations")
    rows <- sample.int(if(is.matrix(data))nrow(data) else length(data),replace=TRUE)
    if(is.matrix(data)) data[rows,,drop=FALSE] else data[rows]
  }
  bootstraps <- replicate(
    n = reps,
    expr = do.call(ml_fun, c(list(draw()), held))
  )

  defaults <- list()

  if (identical(reducer, stats::quantile)) {
    defaults <- list(probs = c(0.025, 0.975))
  }

  arguments <- listmerge(x = defaults, y = list(...))

  if (is.null(dim(bootstraps))) {
    mapped <- sapply(bootstraps, map)
  } else {
    mapped <- apply(bootstraps, 2, map)
  }

  if (is.null(dim(mapped))) {
    do.call(function(...) reducer(mapped, ...), arguments)
  } else {
    do.call(function(...) t(apply(mapped, 1, reducer, ...)), arguments)
  }
}
