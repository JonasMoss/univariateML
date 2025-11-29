mlpower <- function(x, na.rm = FALSE, ...) {}


mlpower_ <- function(x, ...) {
  dots <- list(...)
  epsilon <- if (!is.null(dots$epsilon)) {
    dots$epsilon
  } else {
    .Machine$double.eps^0.5
  }

  m <- mean(log(x))
  alpha <- max(x) + epsilon
  beta <- 1 / (log(alpha) - m)

  estimates <- c(alpha, beta)
  logLik <-
    length(x) * (log(beta) - beta * log(alpha) + (beta - 1) * m)
  list(estimates = estimates, logLik = logLik)
}
