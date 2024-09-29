#' Kumaraswamy distribution maximum likelihood estimation
#'
#' Uses Newton-Raphson to estimate the parameters of the Kumaraswamy
#'     distribution.
#'
#' For the density function of the Kumaraswamy distribution see
#'     [Kumaraswamy][extraDistr::Kumaraswamy].
#'
#' @param x a (non-empty) numeric vector of data values.
#' @param na.rm logical. Should missing values be removed?
#' @param ... `a0` is an optional starting value for the `a` parameter.
#'     `rel.tol` is the relative accuracy requested, defaults
#'     to `.Machine$double.eps^0.25`. `iterlim` is a positive integer
#'     specifying the maximum number of iterations to be performed before the
#'     program is terminated (defaults to `100`).
#'
#' @return `mlkumar` returns an object of [class][base::class] `univariateML`.
#'    This is a named numeric vector with maximum likelihood estimates for `a`
#'    and `b` and the following attributes:
#'     \item{`model`}{The name of the model.}
#'     \item{`density`}{The density associated with the estimates.}
#'     \item{`logLik`}{The loglikelihood at the maximum.}
#'     \item{`support`}{The support of the density.}
#'     \item{`n`}{The number of observations.}
#'     \item{`call`}{The call as captured my `match.call`}
#' @seealso [Kumaraswamy][extraDistr::Kumaraswamy] for the Kumaraswamy density.
#' @examples
#' AIC(mlkumar(USArrests$Rape / 100))
#' @references Jones, M. C. "Kumaraswamy's distribution: A beta-type
#' distribution with some tractability advantages." Statistical Methodology
#' 6.1 (2009): 70-81.
#'
#' Kumaraswamy, Ponnambalam. "A generalized probability density function
#' for double-bounded random processes." Journal of Hydrology 46.1-2 (1980):
#' 79-88.
#' @export
mlkumar <- \(x, na.rm = FALSE, ...) {}

mlkumar <- decorator("mlkumar")

metadata$mlkumar <- list(
  "model" = "Kumaraswamy",
  "density" = "extraDistr::dkumar",
  "support" = intervals::Intervals(c(0, 1), closed = c(FALSE, FALSE)),
  "continuous" = TRUE,
  "names" = c("a", "b"),
  "class" = "mlfun"
)

mlkumar_ <- \(x, ...) {
  dots <- list(...)

  a0 <- if (!is.null(dots$a0)) dots$a0 else 1

  rel.tol <- if (!is.null(dots$rel.tol)) {
    dots$rel.tol
  } else {
    .Machine$double.eps^0.25
  }

  iterlim <- if (!is.null(dots$iterlim)) dots$iterlim else 100

  logs <- log(x)

  for (i in 1:iterlim) {
    xa <- x^a0
    T1 <- a0 * mean(logs / (1 - xa))
    T2 <- a0 * mean(logs * (xa / (1 - xa)))
    T3 <- mean(log(1 - xa))
    f <- 1 / a0 * (1 + T1 + T2 / T3)

    C <- mean(logs^2 * (xa / (1 - xa)^2))
    D <- mean(logs^2 * (xa / (1 - xa)))

    T1diff <- 1 / a0 * T1 + a0 * C
    T2diff <- 1 / a0 * T2 + 1 / a0 * T2^2 + a0 * D

    fdiff <- -1 / a0^2 * f + 1 / a0 *
      (T1diff + T2diff / T3 + 1 / a0 * (T2 / T3)^2)

    a <- a0 - f / fdiff
    if (abs((a0 - a) / a0) < rel.tol) break
    a0 <- a
  }

  if (i == iterlim) {
    warning(paste0(
      "The iteration limit (iterlim = ", iterlim, ") was reached",
      " before the relative tolerance requirement (rel.tol = ",
      rel.tol, ")."
    ))
  }

  ## Given the shape, the scale is easy to compute.
  b <- -1 / mean(log(1 - x^a))

  estimates <- c(a = a, b = b)
  logLik <- length(x) * (log(a) + log(b) + (a - 1) * mean(log(x)) + -1 + 1 / b)
  list(estimates = estimates, logLik = logLik)
}
