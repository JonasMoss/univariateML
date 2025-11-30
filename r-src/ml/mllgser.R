mllgser_ <- function(x, ...) {
  if (sum(x) == length(x)) {
    stop("All observations equals 1; the maximum likelihood estimator does not exist.")
  }
  x_bar <- mean(x)
  xi <- -x_bar * pracma::lambertWn(-exp(-1 / x_bar) / x_bar) - 1
  theta <- xi / (xi + 1)
  logLik <- sum(extraDistr::dlgser(x, theta, log = TRUE))

  list(estimates = theta, logLik = logLik)
}
