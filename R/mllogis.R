#' Logistic distribution maximum likelihood estimation
#'
#' Calculates the estimates using `nlm` with an exponential transform of the
#'     location parameter.
#'
#' For the density function of the logistic distribution see
#'     [Logistic][stats::Logistic].
#'
#' @param x a (non-empty) numeric vector of data values.
#' @param na.rm logical. Should missing values be removed?
#' @param ... currently affects nothing.
#' @return `mllogis` returns an object of [class][base::class] `univariateML`.
#'    This is a named numeric vector with maximum likelihood estimates for
#'    `location` and `scale` and the following attributes:
#'     \item{`model`}{The name of the model.}
#'     \item{`density`}{The density associated with the estimates.}
#'     \item{`logLik`}{The loglikelihood at the maximum.}
#'     \item{`support`}{The support of the density.}
#'     \item{`n`}{The number of observations.}
#'     \item{`call`}{The call as captured my `match.call`}
#' @examples
#' mllogis(precip)
#' @seealso [Logistic][stats::Logistic] for the Logistic density,
#'   [nlm][stats::nlm] for the optimizer this function uses.
#' @references Johnson, N. L., Kotz, S. and Balakrishnan, N. (1995)
#' Continuous Univariate Distributions, Volume 2, Chapter 23. Wiley, New York.
#' @export
mllogis <- \(x, na.rm = FALSE, ...) {}

univariateML_metadata$mllogis <- list(
  "model" = "Logistic",
  "density" = "stats::dlogis",
  "support" = intervals::Intervals(c(-Inf, Inf), closed = c(FALSE, FALSE)),
  "names" = c("location", "scale"),
  "default" = c(0, 1)
)

mllogis_ <- \(x, ...) {
  m <- stats::median(x)
  mad <- stats::median(abs(x - m))
  f <- \(p) -sum(stats::dlogis(x, p[1], exp(p[2]), log = TRUE))
  values <- suppressWarnings(stats::nlm(f = f, p = c(m, log(mad))))
  list(
    estimates = c(values$estimate[1], exp(values$estimate[2])),
    logLik = -values$minimum,
    i = values$iterations
  )
}
