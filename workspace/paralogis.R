mlparalogis_ <- \(x, ...) {
  log_x <- log(x)
  s <- mean(log_x)
  n <- length(x)

  r <- \(shape) mean(log(1 + x^shape))
  r_grad <- \(shape) mean(x^shape * log_x / (x^shape + 1))
  r_hessian <- \(shape) mean(x^shape * log_x^2 / (x^shape + 1)) -
    mean((x^shape * log_x / (x^shape + 1))^2)

  f_over_df <- \(shape) {
    r_grad_shape <- r_grad(shape)
    f <- 2 / shape + s - r(shape) - (shape + 1) * r_grad_shape
    df <- -2 / shape^2 - 2 * r_grad_shape - (shape + 1) * r_hessian(shape)
    f / df
  }

  # Approximate method of moments solution
  shape0 <- max(3 / (1 - mean(x)), 0.1)
  dots <- list(...)
  if(!is.null(dots$shape0)) shape0 <- dots$shape0
  shape <- newton_raphson_1d(f_over_df, shape0)
  loglik <- 2 * n * log(shape) + (shape - 1) * n * s - (shape + 1) * n * r(shape)

  list(estimates = c(shape), loglik = loglik)
}

x <- actuar::rparalogis(1000, 0.01)
x <- x[x < Inf]
mlparalogis_(x)

log_x <- log(x)
s <- mean(log_x)
n <- length(x)
r <- \(shape) mean(log(1 + x^shape))
loglik <- \(shape) 2 * n * log(shape) + (shape - 1) * n * s - (shape + 1) * n * r(shape)
alphas <- seq(0.001, 1, by = 0.0011)
plot(alphas, sapply(alphas, loglik), type = "l")


r <- \(alpha) mean(log(1 + x^alpha))
r_grad <- \(alpha) mean(x^alpha * log(x) / (x^alpha + 1))

f <- \(alpha) {
  2 * n / alpha + n * s - n * r(alpha) - n * (alpha + 1) * r_grad(alpha)
}

plot(alphas, sapply(alphas, f), type = "l", log = "x")
