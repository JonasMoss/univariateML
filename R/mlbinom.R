#' Binomial distribution maximum likelihood estimation
#'
#' For the density function of the Binomial distribution see
#' [Binomial][stats::dbinom].
#'
#' The estimator computes both the `size` and `prob` parameter by default. Be aware
#'   that the likelihood will often be unbounded. According to Olkin et al. (1981),
#'   the likelihood is unbounded when \eqn{\hat{\mu}/\hat{\sigma}^2 \leq 1},
#'   where \eqn{\hat{\sigma}^2} is the biased sample variance. When the likelihood
#'   is unbounded,the maximum likelihood estimator can be regarded as a [Poisson][stats::dpois]
#'   with `lambda` parameter equal to the mean of the observation.
#'
#' When \eqn{\hat{\mu}/\hat{\sigma}^2 \leq 1} and `size` is not supplied by the user,
#'   an error is cast. If `size` is provided and `size < max(x)`, an error is cast.
#'
#' The maximum likelihood estimator of `size` is unstable, and improvements exist.
#'   See, e.g., Carroll and Lomard (1985) and DasGupta and Rubin (2005).
#'
#' @param x a (non-empty) numeric vector of data values.
#' @param na.rm logical. Should missing values be removed?
#' @param ... The arguments `size` can be specified to only return the ml of `prob`.
#'    `reltol` is the relative accuracy requested,
#'    defaults to `.Machine$double.eps^0.25`. `iterlim` is a positive integer
#'    specifying the maximum number of iterations to be performed before the
#'    program is terminated (defaults to `100`).
#' @return `mlbinom` returns an object of [class][base::class] `univariateML`.
#'    This is a named numeric vector with maximum likelihood estimates for
#'    `size` and `prob` and the following attributes:
#'     \item{`model`}{The name of the model.}
#'     \item{`density`}{The density associated with the estimates.}
#'     \item{`logLik`}{The loglikelihood at the maximum.}
#'     \item{`support`}{The support of the density.}
#'     \item{`n`}{The number of observations.}
#'     \item{`call`}{The call as captured my `match.call`}
#' @examples
#' # The likelihood will often be unbounded.
#' \dontrun{
#' mlbinom(ChickWeight$weight)
#' }
#' # Provide a size
#' mlbinom(ChickWeight$weight, size = 400)
#'
#' # Or use mlpoiss, the limiting likelihood of the binomial.
#' mlpois(ChickWeight$weight)
#' @seealso [Binomial][stats::dbinom] for the density.
#' @export
#' @references
#' Olkin, I., Petkau, A. J., & Zidek, J. V. (1981). A comparison of n Estimators for the binomial distribution. Journal of the American Statistical Association, 76(375), 637-642. https://doi.org/10.1080/01621459.1981.10477697
#'
#' Johnson, N. L., Kemp, A. W., & Kotz, S. (2005). Univariate Discrete Distributions (3rd ed.). Wiley-Blackwell.
#'
#' Carroll, R. J., & Lombard, F. (1985). A Note on N Estimators for the Binomial Distribution. Journal of the American Statistical Association, 80(390), 423-426. https://doi.org/10.1080/01621459.1985.10478134
#'
#' DasGupta, A., & Rubin, H. (2005). Estimation of binomial parameters when both n,p are unknown. Journal of Statistical Planning and Inference, 130(1-2), 391-404. https://doi.org/10.1016/j.jspi.2004.02.019


mlbinom <- \(x, na.rm = FALSE, ...) {}

univariateML_metadata$mlbinom <- list(
  "model" = "Binomial",
  "density" = "stats::dbinom",
  "support" = intervals::Intervals(c(0, Inf), closed = c(TRUE, FALSE), type = "Z"),
  "names" = c("size", "prob"),
  "default" = c(10, 0.5)
)

mlbinom_ <- \(x, ...) {
  dots <- list(...)

  n <- length(x)
  x_bar <- mean(x)
  n <- length(x)
  counts <- Rfast::Table(x)
  uniques <- as.numeric(names(counts))

  log_likelihood <- \(size) {
    n * x_bar * log(x_bar) +
      n * (size - x_bar) * log(size - x_bar) -
      n * size * log(size) +
      sum(counts * lchoose(size, uniques))
  }

  if (!is.null(dots$size)) {
    size <- dots$size
    if (size < max(x)) stop("`size` is smaller than `max(x)`.")
    prob <- x_bar / size
    return(list(estimates = c(size, prob), logLik = log_likelihood(size)))
  }

  if (mean(x) / (stats::var(x) * (n - 1) / n) <= 1) {
    stop("The maximum likelihood estimator does not exist. Use `mlpois` to fit a Poisson or supply a `size` argument.")
  }

  f_over_df <- \(size) {
    size_m_x <- size - x_bar
    f <- n * (log(size_m_x) - log(size)) +
      n * digamma(size + 1) -
      sum(counts * digamma(size - uniques + 1))

    df <- n * x_bar / (size * size_m_x) +
      n * trigamma(size + 1) -
      sum(counts * trigamma(size - uniques + 1))

    f/df
  }

  size0 <- max(x)
  size_cands <- size0
  if(size0 != 1) {
    if(f_over_df(size0) > 0) {
      size_cand <- newton_raphson_1d(f_over_df, size0)
      size_cands <- c(size0, floor(size_cand), ceiling(size_cand))
    }
  }

  log_liks <- sapply(size_cands, log_likelihood)
  size <- size_cands[which.max(log_liks)]

  list(estimates = c(size = size, prob = x_bar / size),
       logLik = max(log_liks))
}
