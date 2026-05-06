mlzip_ <- function(x, ...) {
  r_0 <- mean(x == 0)
  x_bar <- mean(x)

  if (r_0 == 1) {
    pi <- 1
    lambda <- 1
    warning("All observations equal to 0; the maximum likelihood estimator is not unique.")
  } else if (r_0 > exp(-x_bar)) {
    x_bar <- mean(x)
    gamma <- x_bar / (1 - r_0)
    lambda <- pracma::lambertWp(-gamma * exp(-gamma)) + gamma
    pi <- (r_0 - exp(-lambda)) / (1 - exp(-lambda))
  } else {
    pi <- 0
    lambda <- x_bar
  }

  logLik <- sum(extraDistr::dzip(x, lambda, pi, log = TRUE))

  list(estimates = c(lambda, pi), logLik = logLik)
}
