mllaplace <- function(x, na.rm = FALSE, ...) {}


mllaplace_ <- function(x, ...) {
  mu <- stats::median(x)
  sigma <- mean(abs(x - mu))
  estimates <- c(mu = mu, sigma = sigma)
  logLik <- -length(x) * (1 + log(2 * sigma))
  list(estimates = estimates, logLik = logLik)
}
