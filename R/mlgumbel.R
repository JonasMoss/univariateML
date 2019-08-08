#' Estimates the parameter of a Gumbel distribution by maximum likelihood
#'
#' Uses Newton-Raphson to estimate the parameters of the Gumbel distribution.
#'
#' @param x The data from which the estimate is to be computed.
#' @param na.rm logical. Should missing values be removed?
#' @param sigma0 An optional starting value for the \code{sigma} parameter.
#' @param rel.tol Relative accuracy requested.
#' @param iterlim A positive integer specifying the maximum number of
#' iterations to be performed before the program is terminated.
#'
#' @return A named numeric vector with maximum likelihood estimates for
#' \code{shape} and \code{sigma}.
#' @export

mlgumbel = function(x, na.rm = FALSE, sigma0 = 1, rel.tol = .Machine$double.eps^0.25,
                    iterlim = 100) {

  if(na.rm) x = x[!is.na(x)] else assertthat::assert_that(!anyNA(x))

  rel.tol_str = deparse(substitute(rel.tol))
  mean_x = mean(x)

  for(i in 1:iterlim) {

    A = sum(x*exp(-x/sigma0))
    B = sum(exp(-x/sigma0))
    C = sum(x^2*exp(-x/sigma0))

    top = mean_x - sigma0 - A/B
    bottom = -1 - 1/sigma0^2*(C/B - (A/B)^2)

    sigma = sigma0 - top/bottom

    if(abs((sigma0 - sigma)/sigma0) < rel.tol) break

    sigma0 = sigma
  }

  if(i == iterlim) {
    warning(paste0("The iteration limit (iterlim = ", iterlim, ") was reached",
                   " before the relative tolerance requirement (rel.tol = ",
                   rel.tol_str, ")."))
  }

  ## Given the sigma, the mu is easy to compute.
  mu = -sigma*log(mean(exp(-x/sigma)))
  S = mean(exp(-(x - mu)/sigma))


  object = c(mu = mu, sigma = sigma)
  class(object) = "univariateML"
  attr(object, "model") = "Gumbel"
  attr(object, "density") = "extraDistr::dgumbel"
  attr(object, "logLik") = -length(x)*(log(sigma) + 1/sigma*(mean_x - mu) + S)
  attr(object, "support") = c(-Inf, Inf)
  attr(object, "n") = length(x)
  object

}
