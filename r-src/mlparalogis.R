mlparalogis_ <- function(x, ...) {
  n <- length(x)
  log_x <- log(x)
  log_x2 <- log_x^2
  s <- sum(log_x) / n
  n <- length(x)

  f_over_df <- function(shape) {
    x_shape <- x^shape
    x_div <- x_shape / (1 + x_shape)
    r <- sum(log(1 + x_shape)) / n
    r_grad <- sum(x_div * log_x) / n
    r_hessian <- sum(x_div * log_x2) / n - sum(x_div * x_div * log_x2) / n

    f <- 2 / shape + s - r - (shape + 1) * r_grad
    df <- -2 / shape^2 - 2 * r_grad - (shape + 1) * r_hessian
    f / df
  }

  # Approximate method of moments solution
  shape0 <- max(3 / (1 - mean(x)), 0.1)
  dots <- list(...)
  if (!is.null(dots$shape0)) shape0 <- dots$shape0
  shape <- newton_raphson_1d(f_over_df, shape0)
  rn <- function(shape) sum(log(1 + x^shape))
  loglik <- 2 * n * log(shape) + (shape - 1) * n * s - (shape + 1) * rn(shape)

  list(estimates = c(shape), logLik = loglik)
}
