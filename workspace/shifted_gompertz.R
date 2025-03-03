x <- extraDistr::rsgomp(1000, 20, 2)

1 / mean(x)

(mean(log(x)^2) - mean(log(x))^2) / var(x)

mlsgomp_(x)
# microbenchmark::microbenchmark(mlsgomp_(x, grad = FALSE),
#                               mlsgomp_(x, grad = TRUE))

mlsgomp_(x, grad = FALSE)
mlsgomp_ <- function(x, ...) {
  n <- length(x)
  x_sum <- sum(x)
  f <- function(p, grad = FALSE) {
    b <- exp(p[1])
    eta <- exp(p[2])
    exp_b <- exp(-b * x)
    eta_exp_b <- 1 + eta * (1 - exp_b)
    log_mean <- sum(log(eta_exp_b))
    exp_mean <- sum(exp_b)
    loglik <- n * log(b) - b * x_sum - eta * exp_mean + log_mean
    if (grad) {
      x_exp_b <- x * exp_b
      r <- sum((1 - exp_b) / eta_exp_b)
      s <- x_sum - eta * sum(x_exp_b)
      t <- sum(x_exp_b / eta_exp_b)
      deta <- -exp_mean + r
      db <- n / b - s + eta * t
      attr(loglik, "gradient") <- -c(deta, db)
    }
    -loglik
  }
  dots <- list(...)
  grad <- if (!is.null(dots$grad)) dots$grad else FALSE
  fit <- nlm(f, p = c(1, 1), grad = grad)
  estimates <- exp(fit$estimate)
  # estimates[1] <- exp(estimates[1])
  list(estimates = estimates, logLik = -length(x) * fit$minimum, rest = fit)
}
