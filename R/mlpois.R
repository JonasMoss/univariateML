#' Poisson distribution maximum likelihood estimation
#'
#' The maximum likelihood estimate of `lambda` is the empirical mean.
#'
#' For the density function of the Poisson distribution see
#' [Poisson][stats::Poisson].
#'
#' @param x a (non-empty) numeric vector of data values.
#' @param na.rm logical. Should missing values be removed?
#' @param ... currently affects nothing.
#' @return `mlpois` returns an object of [class][base::class] `univariateML`.
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
#' @seealso [Poisson][stats::Poisson] for the Poisson density.
#' @export
mlgumbel <- \(x, na.rm = FALSE, ...) {}

mlgumbel <- decorator("mlgumbel")

metadata$mlgumbel <- list(
  "model" = "Gumbel",
  "density" = "extraDistr::dgumbel",
  "support" = intervals::Intervals(c(-Inf, Inf), closed = c(FALSE, FALSE)),
  "continuous" = TRUE,
  "names" = c("mu", "sigma"),
  "class" = "mlfun"
)

mlpois <- \(x, na.rm = FALSE, ...) {
  if (na.rm) x <- x[!is.na(x)] else assertthat::assert_that(!anyNA(x))
  ml_input_checker(x)
  n <- length(x)

  lambda <- mean(x)

  object <- c(lambda = lambda)
  class(object) <- "univariateML"
  attr(object, "model") <- "Poisson"
  attr(object, "continuous") <- FALSE
  attr(object, "density") <- "stats::dpois"
  attr(object, "logLik") <- -n * lambda + sum(x) * log(lambda) - sum(lgamma(x + 1))
  attr(object, "support") <- c(0, Inf)
  attr(object, "n") <- length(x)
  attr(object, "call") <- match.call()
  object
}
