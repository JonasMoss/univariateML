#' Beta-binomial distribution maximum likelihood estimation
#'
#' For the density function of the Beta-binomial distribution see
#' [Beta-binomial][extraDistr::dbbinom].
#'
#' @param x a (non-empty) numeric vector of data values.
#' @param na.rm logical. Should missing values be removed?
#' @param ... The arguments `size` can be specified to only return the ml of `prob`.
#'    `reltol` is the relative accuracy requested,
#'    defaults to `.Machine$double.eps^0.25`. `iterlim` is a positive integer
#'    specifying the maximum number of iterations to be performed before the
#'    program is terminated (defaults to `100`).
#' @return `mlbbinom` returns an object of [class][base::class] `univariateML`.
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
#' @seealso [Beta-binomial][extraDistr::dbbinom] for the density.
#' @export
mlbbinom <- \(x, na.rm = FALSE, ...) {}

metadata$mlbbinom <- list(
  "model" = "Beta-binomial",
  "density" = "extraDistr::dbbinom",
  "support" = intervals::Intervals(c(0, Inf), closed = c(TRUE, FALSE), type = "Z"),
  "names" = c("size", "alpha", "beta")
)

mlbbinom_ <- \(x, ...) {
  n <- length(x)
  x_bar <- mean(x)
  freqs <- Rfast::Table(x)
  values <- as.numeric(names(freqs))



  list(estimates = c(size, prob), logLik = logLik)
}
