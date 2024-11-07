#' Cauchy distribution maximum likelihood estimation
#'
#' Calculates the estimates using `nlm` and an exponential transform of the
#'     location parameter. If `n < 5`, an exact solution is reported. In
#'     the edge case where no maximum likelihood estimator exists and error is
#'     thrown.
#'
#' For the density function of the Cauchy distribution see
#'     [Cauchy][stats::Cauchy].
#'
#' @param x a (non-empty) numeric vector of data values.
#' @param na.rm logical. Should missing values be removed?
#' @param ... currently affects nothing.
#' @return `mlcauchy` returns an object of [class][base::class] `univariateML`.
#'    This is a named numeric vector with maximum likelihood estimates for
#'    `location` and `scale` and the following attributes:
#'     \item{`model`}{The name of the model.}
#'     \item{`density`}{The density associated with the estimates.}
#'     \item{`logLik`}{The loglikelihood at the maximum.}
#'     \item{`support`}{The support of the density.}
#'     \item{`n`}{The number of observations.}
#'     \item{`call`}{The call as captured my `match.call`}
#' @seealso [Cauchy][stats::Cauchy] for the Cauchy density, [nlm][stats::nlm]
#'   for the optimizer this function uses.
#' @references Johnson, N. L., Kotz, S. and Balakrishnan, N. (1995)
#' Continuous Univariate Distributions, Volume 1, Chapter 16. Wiley, New York.
#' @examples
#' mlcauchy(airquality$Temp)
#' @export
mlcauchy <- \(x, na.rm = FALSE, ...) {}

univariateML_metadata$mlcauchy <- list(
  "model" = "Cauchy",
  "density" = "stats::dcauchy",
  "support" = intervals::Intervals(c(-Inf, Inf), closed = c(FALSE, FALSE)),
  "names" = c("location", "scale"),
  "default" = c(0, 1)
)

mlcauchy_ <- \(x, ...) {
  m <- stats::median(x)
  mad <- stats::median(abs(x - m))
  start <- c(m, mad)

  f <- \(p) -sum(stats::dcauchy(x, p[1], exp(p[2]), log = TRUE))
  values <- suppressWarnings(stats::nlm(f = f, p = start))

  list(
    estimates = c(values$estimate[1], exp(values$estimate[2])),
    logLik = -values$minimum
  )
}
