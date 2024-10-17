#' Zipf distribution maximum likelihood estimation
#'
#' For the density function of the Zipf distribution see
#' [Zipf][sads::dzipf].
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
#' mlzipf(precip)
#' @seealso [Zipf][sads::dzipf] for the density.
#' @export
mlzipf <- \(x, na.rm = FALSE, ...) {}

metadata$mlzipf <- list(
  "model" = "Zipf",
  "density" = "sads::dzipf",
  "support" = stats::setNames(intervals::Intervals(c(1, Inf), closed = c(TRUE, FALSE), type = "Z"), c("1", "N")),
  "names" = c("N", "s")
)

mlzipf_ <- \(x, ...) {

  dots <- list(...)
  N <- max(x)
  n <- length(x)
  sum_lx <- sum(log(x))
  log_seq <- log(seq(N))
  log_seq2 <- log(seq(N))^2

  if(sum_lx == 0) {
    warning("All observations are equal to 1. The maximum likelihood estimator is not unique.")
    return(list(estimates = c(N, 1), logLik = 0))
  }

  rel.tol <- if (!is.null(dots$rel.tol)) {
    dots$rel.tol
  } else {
    .Machine$double.eps^0.25
  }

  iterlim <- if (!is.null(dots$iterlim)) {
    dots$iterlim
  } else {
    100
  }

  shape0 <- 1

  for (i in 1:iterlim) {
    sum_shape <- sum(1/seq(1, N)^shape0)
    sum_log_hs <- sum(1/seq(1, N)^shape0 * log_seq)
    sum_log2_hs <- sum(1/seq(1, N)^shape0 * log_seq2)
    top = sum_log_hs / sum_shape
    bottom = -n*sum_log2_hs / sum_shape + n*top^2
    shape <- shape0 - (n*top - sum_lx)/bottom
    if (abs((shape - shape0) / shape0) < rel.tol) break
    shape0 <- shape
  }

  if (i == iterlim) {
    warning(paste0(
      "The iteration limit (iterlim = ", iterlim, ") was reached",
      " before the relative tolerance requirement (rel.tol = ",
      rel.tol, ")."
    ))
  }


  h <- \(shape) sum(1/seq(1, N)^shape)
  f <- \(shape) -shape * sum_lx - n * log(h(shape))

  logLik = f(shape)
  list(estimates = c(N, shape), logLik = logLik)
}
