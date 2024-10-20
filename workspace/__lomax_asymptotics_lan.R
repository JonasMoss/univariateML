mu <- 2
h_mu <- 0
h_r <- 1

log_sum_n <- \(x) {
  n <- length(x)
  mu_n = (mu + h_mu / n^(1/2))
  r_n = (h_r / sqrt(n))
  sum(extraDistr::dlomax(x, mu_n * r_n, 1 / r_n, log = TRUE))
}

log_sum_0 <- \(x) {
  n <- length(x)
  mu_n = (mu + h_mu / sqrt(n))
  sum(dexp(x, mu_n, log = TRUE))
}

n_reps <- 10000
nn <- (1:10)*2000
results <- sapply(nn, \(n)
  replicate(n_reps, {
    x <- rexp(n)
    log_sum_n(x) - log_sum_0(x)
  }))

plot(nn, apply(results, 2, var))
plot(nn, colMeans(results))
abline(a = 0, b = 1)

## Mean appears to be -1. Is it -lambda in general?