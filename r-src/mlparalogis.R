#' Paralogistic distribution maximum likelihood estimation
#'
#' This function estimates the only the shape parameters of the Paralogistic distribution.
#' The rate is set to 1.
#'
#' For the density function of the Paralogistic distribution see [Paralogistic][actuar::dparalogis].
#'
#' @param x a (non-empty) numeric vector of data values.
#' @param na.rm logical. Should missing values be removed?
#' @param ... currently affects nothing.
#' @return `mlparalogis` returns an object of [class][base::class] `univariateML`.
#'    This is a named numeric vector with maximum likelihood estimates for
#'    `shape` and the following attributes:
#'     \item{`model`}{The name of the model.}
#'     \item{`density`}{The density associated with the estimates.}
#'     \item{`logLik`}{The loglikelihood at the maximum.}
#'     \item{`support`}{The support of the density.}
#'     \item{`n`}{The number of observations.}
#'     \item{`call`}{The call as captured my `match.call`}
#' @examples
#' mlparalogis(abalone$length)
#' @seealso [Paralogistic][actuar::dparalogis] for the paralogistic density.
#' @references Kleiber, C. and Kotz, S. (2003), Statistical Size Distributions in Economics and Actuarial Sciences, Wiley.
#' Klugman, S. A., Panjer, H. H. and Willmot, G. E. (2012), Loss Models, From Data to Decisions, Fourth Edition, Wiley.
#' @export
mlparalogis <- function(x, na.rm = FALSE, ...) {}

univariateML_metadata$mlparalogis <- list(
  "model" = "Paralogistic",
  "density" = "actuar::dparalogis",
  "support" = intervals::Intervals(c(0, Inf), closed = c(FALSE, FALSE)),
  "names" = c("shape"),
  "default" = c(1)
)

mlparalogis_ <- function(x, ...) {
  n <- length(x)
  log_x <- log(x)
  log_x2 <- log_x^2
  s <- sum(log_x) / n
  n <- length(x)

  f_over_df <- function(shape) {
    x_shape <- x^shape
    x_div <- x_shape / (1 + x_shape)
    r <- sum(log(1 + x_shape)) / n
    r_grad <- sum(x_div * log_x) / n
    r_hessian <- sum(x_div * log_x2) / n - sum(x_div * x_div * log_x2) / n

    f <- 2 / shape + s - r - (shape + 1) * r_grad
    df <- -2 / shape^2 - 2 * r_grad - (shape + 1) * r_hessian
    f / df
  }

  # Approximate method of moments solution
  shape0 <- max(3 / (1 - mean(x)), 0.1)
  dots <- list(...)
  if (!is.null(dots$shape0)) shape0 <- dots$shape0
  shape <- newton_raphson_1d(f_over_df, shape0)
  rn <- function(shape) sum(log(1 + x^shape))
  loglik <- 2 * n * log(shape) + (shape - 1) * n * s - (shape + 1) * rn(shape)

  list(estimates = c(shape), logLik = loglik)
}
