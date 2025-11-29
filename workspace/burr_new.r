mlburr2 <- function(x) {
  if (all(x >= 1)) {
    stop("The Burr maximum likelihood estimator does not exist when all observations >=1. Use `mlburr` to fit a Pareto distribution.")
  }

  n <- length(x)
  log_x <- log(x)
  lx_sum <- sum(log_x)
  f_over_df <- function(c) {
    x_c <- exp(log_x*c)
    x_c_div <- x_c / (x_c + 1)
    g <- sum(log(1 + x_c))
    dg <- sum(log_x * x_c_div)
    d2g <- sum(log_x^2 * x_c_div) - sum((log_x * x_c_div)^2)
    f <- n / c - n * dg / g + lx_sum - dg
    df <- -n / c^2 - n * (g * d2g - dg^2) / g^2 - d2g
    f / df
  }

  mu <- lx_sum / n
  sigma2 <- sum(log_x^2)/n - mu^2
  c0 <- if (mu^2 > sigma2 && mu > 0) {
    pi / sqrt(3 * sigma2)
  } else {
    logmom_burr_c(mu, sigma2)
  }
  c <- univariateML:::newton_raphson_1d(f_over_df, c0)
  k <- n / sum(log(1 + x^c))

  c("shape1" = k, "shape2" = c)
}

set.seed(123)
shape1 <- 0.1
shape2 <- 0.01
x <- actuar::rburr(100, shape1 = shape1, shape2 = shape2, scale = 1)
x <- x[is.finite(x)]

microbenchmark::microbenchmark(mlburr2(x), mlburr_cpp(x))