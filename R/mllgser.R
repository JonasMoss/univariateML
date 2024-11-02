#' Logarithmic series distribution maximum likelihood estimation
#'
#' For the density function of the Logarithmic series distribution see
#' [Logarithmic series][extraDistr::dlgser]. For an example data set, see [corbet].
#'
#' @param x a (non-empty) numeric vector of data values.
#' @param na.rm logical. Should missing values be removed?
#' @param ... Not in use.
#' @return `mllgser` returns an object of [class][base::class] `univariateML`.
#'    This is a named numeric vector with maximum likelihood estimates for
#'    `theta`.
#'     \item{`model`}{The name of the model.}
#'     \item{`density`}{The density associated with the estimates.}
#'     \item{`logLik`}{The loglikelihood at the maximum.}
#'     \item{`support`}{The support of the density.}
#'     \item{`n`}{The number of observations.}
#'     \item{`call`}{The call as captured my `match.call`}
#' @examples
#' theta_hat <- mllgser(corbet)
#'
#' # The corbert data contains observations from 1 to 24.
#' observed <- table(corbet)
#'
#' # The chi square test evaluated at the maximum likelihood is highly significant.
#' expected <- extraDistr::dlgser(1:24, theta_hat)
#' chisq.test(observed, p = expected / sum(expected))
#'
#' # But chi square test evaluated at 0.997 (used in Corbet) is not.
#' expected <- extraDistr::dlgser(1:24, 0.997)
#' chisq.test(observed, p = expected / sum(expected))
#'
#' # The chi square for `dzipf` is similar.
#' expected <- sads::dzipf(1:24, mlzipf(corbet)[1], mlzipf(corbet)[2]) * length(corbet)
#' chisq.test(observed, p = expected / sum(expected))
#' @references
#' Fisher, R. A., Corbet, A. S., & Williams, C. B. (1943). The relation between the number of species and the number of individuals in a random sample of an animal population. The Journal of Animal Ecology, 12(1), 42. https://doi.org/10.2307/1411
#'
#' Johnson, N. L., Kemp, A. W., & Kotz, S. (2005). Univariate Discrete Distributions (3rd ed.). Wiley-Blackwell.
#' @seealso [Logarithmic series][extraDistr::dlgser] for the density.
#' @export

mllgser <- \(x, na.rm = FALSE, ...) {}

metadata$mllgser <- list(
  "model" = "Logarithmic series",
  "density" = "extraDistr::dlgser",
  "support" = intervals::Intervals(c(1, Inf), closed = c(TRUE, FALSE), type = "Z"),
  "names" = c("theta"),
  "default" = 0.9
)

mllgser_ <- \(x, ...) {
  if (sum(x) == length(x)) {
    stop("All observations equals 1; the maximum likelihood estimator does not exist.")
  }
  x_bar <- mean(x)
  xi <- -x_bar * pracma::lambertWn(-exp(-1 / x_bar) / x_bar) - 1
  theta <- xi / (xi + 1)
  logLik <- sum(extraDistr::dlgser(x, theta, log = TRUE))

  list(estimates = theta, logLik = logLik)
}
