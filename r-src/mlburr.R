mlburr <- function(x, na.rm = FALSE, ...) {}



mlburr_ <- function(x, ...) {
  if (all(x >= 1)) {
    stop("The Burr maximum likelihood estimator does not exist when all observations >=1. Use `mlpareto` to fit a Pareto distribution.")
  }

  n <- length(x)
  log_x <- log(x)
  lx_sum <- sum(log_x)
  f_over_df <- function(c) {
    x_c <- x^c
    x_c_div <- x_c / (x_c + 1)
    g <- sum(log(1 + x_c))
    dg <- sum(log_x * x_c_div)
    d2g <- sum(log_x^2 * x_c_div) - sum((log_x * x_c_div)^2)
    f <- n / c - n * dg / g + lx_sum - dg
    df <- -n / c^2 - n * (g * d2g - dg^2) / g^2 - d2g
    f / df
  }

  mu <- lx_sum / n
  sigma2 <- sum(log_x^2) / n - mu^2
  c0 <- burr_lmom(mu, sigma2)[2]
  c <- newton_raphson_1d(f_over_df, c0, ...)
  k <- n / sum(log(1 + x^c))

  list(
    estimates = c(k, c),
    logLik = sum(actuar::dburr(x, k, c, log = TRUE))
  )
}
