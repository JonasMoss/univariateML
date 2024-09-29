#' Logit-Normal distribution maximum likelihood estimation
#'
#' The maximum likelihood estimate of `mu` is the empirical mean of the
#'     logit transformed data and the maximum likelihood estimate of
#'     `sigma` is the square root of the logit transformed
#'     biased sample variance.
#'
#' For the density function of the logit-normal distribution see
#'    [dlogitnorm][logitnorm::dlogitnorm].
#'
#' @param x a (non-empty) numeric vector of data values.
#' @param na.rm logical. Should missing values be removed?
#' @param ... currently affects nothing.
#' @return `mllogitnorm` returns an object of [class][base::class]
#'    `univariateML`. This is a named numeric vector with maximum likelihood
#'    estimates for `mu` and `sigma` and the following attributes:
#'     \item{`model`}{The name of the model.}
#'     \item{`density`}{The density associated with the estimates.}
#'     \item{`logLik`}{The loglikelihood at the maximum.}
#'     \item{`support`}{The support of the density.}
#'     \item{`n`}{The number of observations.}
#'     \item{`call`}{The call as captured my `match.call`}
#' @examples
#' AIC(mllogitnorm(USArrests$Rape / 100))
#' @seealso [Normal][stats::dnorm] for the normal density.
#' @references Atchison, J., & Shen, S. M. (1980). Logistic-normal
#' distributions: Some properties and uses. Biometrika, 67(2), 261-272.
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

mllogitnorm <- \(x, na.rm = FALSE, ...) {
  if (na.rm) x <- x[!is.na(x)] else assertthat::assert_that(!anyNA(x))
  ml_input_checker(x)

  assertthat::assert_that(min(x) > 0)
  assertthat::assert_that(max(x) < 1)

  n <- length(x)
  y <- stats::qlogis(x)
  mu <- mean(y)
  sigma <- sqrt(stats::var(y) * (n - 1) / n)

  H <- mean(log(x))
  G <- mean(log(1 - x))
  object <- c(mu = mu, sigma = sigma)
  class(object) <- c("univariateML")
  attr(object, "model") <- "LogitNormal"
  attr(object, "density") <- "logitnorm::dlogitnorm"
  attr(object, "logLik") <-
    -n / 2 * (1 + log(2 * pi) + 2 * log(sigma) + 2 * H + 2 * G)
  attr(object, "support") <- c(0, 1)
  attr(object, "n") <- length(x)
  attr(object, "call") <- match.call()
  object
}
