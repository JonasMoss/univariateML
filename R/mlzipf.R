#' Zipfdistribution maximum likelihood estimation
#'
#' For the density function of the Zipfdistribution see
#' [Zipf][stats::dzipf].
#'
#' @param x a (non-empty) numeric vector of data values.
#' @param na.rm logical. Should missing values be removed?
#' @param ... Not currently in use.
#' @return `mlzipf` returns an object of [class][base::class] `univariateML`.
#'    This is a named numeric vector with maximum likelihood estimates for
#'    `mean` and `sd` and the following attributes:
#'     \item{`model`}{The name of the model.}
#'     \item{`density`}{The density associated with the estimates.}
#'     \item{`logLik`}{The loglikelihood at the maximum.}
#'     \item{`support`}{The support of the density.}
#'     \item{`n`}{The number of observations.}
#'     \item{`call`}{The call as captured my `match.call`}
#' @examples
#' mlpois(precip)
#' @seealso [Zipf][stats::dzipf] for the density.
#' @export
mlzipf <- \(x, na.rm = FALSE, ...) {}

metadata$mlzipf <- list(
  "model" = "Zipf",
  "density" = "sads::dzipf",
  "support" = stats::setNames(intervals::Intervals(c(1, Inf), closed = c(TRUE, FALSE), type = "Z"), c("1", "N")),
  "names" = c("N", "shape")
)

mlzipf_ <- \(x, ...) {
  n <- length(x)
  dots <- dots(...)
  if(is.null(dots$N)) N <- max(x)
  N <- max(x)

  h <- \(N, shape) sum(1/seq(1, N)^shape)
  f <- \(N, shape) -shape * sum(log(x)) - n * log(h(N, shape))


  res <- sapply(73:10, \(N) optimize(\(shape) -f(N, shape), c(0, 10)))

  list(estimates = c(N, shape), logLik = logLik)
}
