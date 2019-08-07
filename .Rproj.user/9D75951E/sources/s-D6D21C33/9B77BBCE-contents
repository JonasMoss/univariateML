#' Estimates of the underlying mean and standard deviation of a truncated normal
#'
#' Calculates the maximum likelihood estimates of \code{mean} and \code{sd} of
#'    a truncated normal with known lower truncation \code{a} and upper t
#'    truncation \code{b}.
#'
#' @param x The data from which the estimate is to be computed.
#' @param a,b lower and upper truncation points, \code{a < x <= b}.
#' @param na.rm logical. Should missing values be removed?
#' @return A named numeric vector with maximum likelihood estimates for
#' \code{location} and \code{scale}.
#' @export

mltnorm = function(x, a = -Inf, b = Inf, na.rm = FALSE) {
  if(na.rm) x = x[!is.na(x)] else assertthat::assert_that(!anyNA(x))
  if(a == -Inf & b == Inf) return(mlnorm(x))

  assertthat::assert_that(min(x) > a)
  assertthat::assert_that(max(x) <= b)

  start = c(mean(x), stats::sd(x))
  S = mean(x^2)
  M = mean(x)
  n = length(x)

  f = function(p) {
    mu = p[1]
    sigma = p[2]
    1/(2*sigma^2)*(S - 2*mu*M + 1/n*mu^2) + log(sigma) +
      log(stats::pnorm((b - mu)/sigma) - stats::pnorm((a - mu)/sigma))

  }

  values = stats::nlm(f = f,
                      p = start)

  object = c(location = values[1], scale = exp(values[2]))
  class(object) = c("univariateML", "numeric")
  attr(object, "model") = "TruncNormal"
  attr(object, "parameters") = c(a = a, b = b)
  object

}
