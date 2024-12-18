#' Zipf distribution maximum likelihood estimation
#'
#' For the density function of the Zipf distribution see
#' [Zipf][sads::dzipf].
#'
#' This function follows the same format as every other function in the package,
#' but most applications of Zipf's law use rank-abundance data. See, e.g., [sads::fitzipf]
#' for estimation of this sort of data.
#'
#' @param x a (non-empty) numeric vector of data values.
#' @param na.rm logical. Should missing values be removed?
#' @param ... Not currently in use.
#' @return `mlzipf` returns an object of [class][base::class] `univariateML`.
#'    This is a named numeric vector with maximum likelihood estimates for
#'    `N` and `s` and the following attributes:
#'     \item{`model`}{The name of the model.}
#'     \item{`density`}{The density associated with the estimates.}
#'     \item{`logLik`}{The loglikelihood at the maximum.}
#'     \item{`support`}{The support of the density.}
#'     \item{`n`}{The number of observations.}
#'     \item{`call`}{The call as captured my `match.call`}
#' @examples
#' AIC(mlzipf(corbet)) # 2729.536
#' AIC(mllgser(corbet)) # 2835.883
#' @seealso [Zipf][sads::dzipf] for the density.
#' @references
#' Johnson, N. L., Kemp, A. W., & Kotz, S. (2005). Univariate Discrete Distributions (3rd ed.). Wiley-Blackwell.
#' @export
mlzipf <- \(x, na.rm = FALSE, ...) {}

univariateML_metadata$mlzipf <- list(
  "model" = "Zipf",
  "density" = "sads::dzipf",
  "support" = stats::setNames(intervals::Intervals(c(1, Inf), closed = c(TRUE, FALSE), type = "Z"), c("1", "N")),
  "names" = c("N", "s"),
  "default" = c(3, 0.5)
)

mlzipf_ <- \(x, ...) {
  N <- max(x)
  n <- length(x)
  sum_lx <- sum(log(x))
  log_seq <- log(seq(N))
  log_seq2 <- log(seq(N))^2
  div_seq <- 1 / seq(1, N)

  if (sum_lx == 0) {
    warning("All observations are equal to 1. The maximum likelihood estimator is not unique.")
    return(list(estimates = c(N, 1), logLik = 0))
  }

  f_over_df <- \(shape0) {
    div_seq_shape0 <- div_seq^shape0
    sum_shape <- sum(div_seq_shape0)
    sum_log_hs <- sum(div_seq_shape0 * log_seq)
    sum_log2_hs <- sum(div_seq_shape0 * log_seq2)
    top <- sum_log_hs / sum_shape
    bottom <- -n * sum_log2_hs / sum_shape + n * top^2
    (n * top - sum_lx) / bottom
  }

  shape0 <- 1
  shape <- newton_raphson_1d(f_over_df, shape0, ...)

  if (shape < 0) {
    stop("Optimal shape parameter is less than 0. The maximum likelihood estimator does not exist.")
  }

  logLik <- -shape * sum_lx - n * log(sum(1 / seq(1, N)^shape))
  list(estimates = c(N, shape), logLik = logLik)
}
