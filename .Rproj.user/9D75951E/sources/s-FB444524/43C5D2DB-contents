#' Estimates the parameter of a Gumbel distribution by maximum likelihood
#'
#' Uses Newton-Raphson to estimate the parameters of the Gumbel distribution.
#'
#' @param x The data from which the estimate is to be computed.
#' @param scale0 An optional starting value for the \code{scale} parameter.
#' @param rel.tol Relative accuracy requested.
#' @param iterlim A positive integer specifying the maximum number of
#' iterations to be performed before the program is terminated.
#'
#' @return A named numeric vector with maximum likelihood estimates for
#' \code{shape} and \code{scale}.
#' @export

mlgumbel = function(x, scale0 = 1, rel.tol = .Machine$double.eps^0.25,
                    iterlim = 100) {

  rel.tol_str = deparse(substitute(rel.tol))
  mean_x = mean(x)

  for(i in 1:iterlim) {

    A = sum(x*exp(-x/scale0))
    B = sum(exp(-x/scale0))
    C = sum(x^2*exp(-x/scale0))

    top = mean_x - scale0 - A/B
    bottom = -1 - 1/scale0^2*(C/B - (A/B)^2)

    scale = scale0 - top/bottom

    if(abs((scale0 - scale)/scale0) < rel.tol) break

    scale0 = scale
  }

  if(i == iterlim) {
    warning(paste0("The iteration limit (iterlim = ", iterlim, ") was reached",
                   " before the relative tolerance requirement (rel.tol = ",
                   rel.tol_str, ")."))
  }

  ## Given the shape, the scale is easy to compute.
  loc = -scale*log(mean(exp(-x/scale)))

  object = c(loc = loc, scale = scale)
  class(object) = "univariateML"
  attr(object, "density") = "Gumbel"
  object

}
