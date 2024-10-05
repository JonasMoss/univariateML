#' Beta prime distribution maximum likelihood estimation
#'
#' This function does not estimate the scale parameter for the `BetaPrime`
#'     distribution. Transforms the data and uses `stat::nlm` to estimate
#'     the parameters of the Beta distribution.
#'
#' For the density function of the Beta prime distribution see
#'     [BetaPrime][extraDistr::BetaPrime].
#'
#' @param x a (non-empty) numeric vector of data values.
#' @param na.rm logical. Should missing values be removed?
#' @param ... passed to [`mlbeta`][mlbeta].
#' @return `mlbetapr` returns an object of [class][base::class] `univariateML`.
#'    This is a named numeric vector with maximum likelihood estimates for
#'    `shape1` and `shape2` and the following attributes:
#'     \item{`model`}{The name of the model.}
#'     \item{`density`}{The density associated with the estimates.}
#'     \item{`logLik`}{The loglikelihood at the maximum.}
#'     \item{`support`}{The support of the density.}
#'     \item{`n`}{The number of observations.}
#'     \item{`call`}{The call as captured my `match.call`}
#' @details For `type`, the option `none` is fastest.
#' @seealso [BetaPrime][extraDistr::BetaPrime] for the Beta prime density,
#'   [nlm][stats::nlm] for the optimizer this function uses, [mlbeta] for
#'   the Beta distribution maximum likelihood estimator.
#' @examples
#' AIC(mlbetapr(USArrests$Rape))
#' @references Johnson, N. L., Kotz, S. and Balakrishnan, N. (1995)
#' Continuous Univariate Distributions, Volume 2, Chapter 25. Wiley, New York.
#' @export

mlbetapr <- function(x, na.rm = FALSE, ...) {
  if (na.rm) x <- x[!is.na(x)] else assertthat::assert_that(!anyNA(x))
  ml_input_checker(x)
  assertthat::assert_that(min(x) > 0)

  # Make a minimal reference to extraDistr package so that R-CMD-CHECK recognizes it
  invisible(extraDistr::dbetapr)

  val1 <- mean(log(x))
  val2 <- mean(log(1 + x))
  object <- mlbeta(x / (x + 1), na.rm = FALSE, ...)
  alpha <- object[1]
  beta <- object[2]
  class(object) <- "univariateML"
  attr(object, "model") <- "BetaPrime"
  attr(object, "density") <- "extraDistr::dbetapr"
  attr(object, "logLik") <- unname(-length(x) * (lbeta(alpha, beta) -
    (alpha - 1) * val1 +
    (alpha + beta) * val2))
  attr(object, "support") <- c(0, Inf)
  attr(object, "n") <- length(x)
  attr(object, "call") <- match.call()
  object
}
