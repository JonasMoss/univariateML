log_moment_kumar <- function(mu, sigma2) {
  neuler <- digamma(1) # Negative Euler-Mascheroni constant
  basel <- pi^2 / 6 # trigamma(1)

  f_over_df <- function(a) {
    b <- inverse_digamma(neuler - a * mu)
    tri <- trigamma(b)
    tetra <- psigamma(b, deriv = 2)

    f <- a * tri * (basel - tri - a^2 * sigma2)
    df <- a * tetra * mu - 2 * tri * (basel + tri)
    f / df
  }

  a <- univariateML:::newton_raphson_1d(f_over_df, sqrt(basel / sigma2))
  #b <- inverse_digamma(neuler - a * mu) - 1
  #c(a = a, b = b)
  a
}

mlkumar2 <- function(x) {
  n <- length(x)
  log_x <- log(x)
  n_div <- 1 / n

  f_over_df <- function(a) {
    xa <- x^a
    xa_div <- xa / (1 - xa) * log_x

    psi <- sum(xa_div^2) * n_div
    phi <- sum(xa_div * log_x) * n_div

    t1 <- sum(log_x / (1 - xa)) * n_div
    t2 <- sum(xa_div) * n_div
    t3 <- sum(log(1 - xa)) * n_div

    s1 <- t2 / t3
    s2 <- ((t2^2 + phi) / t3 + psi)
    s3 <- t1 + s1

    f <-  a * s3 + 1
    df <- a * (s1^2 + s2) + s3 - f / a

    f / df
  }

  ## Starting value for Newton--Raphson
  mu <- sum(log_x) * n_div
  sigma2 <- sum(log_x^2) * n_div - mu^2
  a0 <- log_moment_kumar(mu, sigma2)

  ## Newton--Raphson for a.
  reltol <- .Machine$double.eps^0.25
  iterlim <- 100
  for (i in seq(iterlim)) {
    a <- a0 - f_over_df(a0)
    if (abs(a0 - a) < abs(a0)*reltol) break
    a0 <- a
  }
  a <- a0

  b <- -n / sum(log(1 - x^a))
  estimates <- c(a = a, b = b)
  logLik <- n * (log(a) + log(b) + (a - 1) * mu + 1 / b - 1)
  list(estimates = estimates, logLik = logLik, iterations = i)
}


set.seed(1)
a <- 10
b <- 22.257825
x <- extraDistr::rkumar(1000, a, b)

mlkumar_(x)
mlkumar2(x)

microbenchmark::microbenchmark(mlkumar_(x), mlkumar2(x), times = 1000)


logmedian <- function(mu, M_log_hat) {

  # Necessary constants
  psi1 <- -0.5772156649015329 # digamma(1) is -Euler-Mascheroni constant
  log2 <- 0.6931471805599453

  # Function defining the Newton-Raphson step f/(df/da)
  f_over_df <- function(a) {
    c_val <- inverse_digamma(psi1 - a * mu)
    b_val <- c_val - 1

    inv_b <- 1 / b_val
    term_half_pow_inv_b <- exp(-log2 * inv_b) # (1/2)^(1/b)
    log_arg <- 1 - term_half_pow_inv_b
    L_a <- log(log_arg)

    p_a <- L_a / a - M_log_hat
    trig_c <- trigamma(c_val)
    L_prime_a <- (1 / log_arg) * (log2 * term_half_pow_inv_b) * (mu / (b_val^2 * trig_c))
    p_prime_a <- (a * L_prime_a - L_a) / (a^2)
    p_a / p_prime_a
  }

  a0 <- -log2 / M_log_hat
  univariateML:::newton_raphson_1d(f_over_df, a0)

}

mlkumar2(x)[1]
logmedian(mean(log(x)), median(log(x)))[1]
log_moment_kumar(mean(log(x)), var(log(x)))

# Example Usage (requires sample data 'x' and inverse_digamma)
# x <- rkumar(...) # Generate sample data
# mu_hat <- mean(log(x))
# M_log_hat <- median(log(x))
#
# # Estimate 'a' using this method
# a_start_val <- hybrid_logmedian_nr_core(mu_hat, M_log_hat, your_inverse_digamma_function)
# print(a_start_val)
#
# # Then use this a_start_val in your mlkumar2 function
# # ml_fit <- mlkumar2(x, start_value_override = a_start_val) # Need to adapt mlkumar2 slightly
# to accept an override

# Or modify mlkumar2 to call this function internally:
# mlkumar2 <- function(x, start_method = "hybridlogmedian_nr", ...) {
#    # ... setup ...
#    if (start_method == "hybridlogmedian_nr") {
#       # ... calculate mu, M_log_hat ...
#       # Ensure inv_digamma_func is available from ... or globally
#       a0 <- hybrid_logmedian_nr_core(mu, M_log_hat, inv_digamma_func)
#    } else if ...
#    # ... rest of MLE ...
# }

#
#
# a <- runif(1, 0, 100)
# b <- runif(1, 0, 100)
# mu <- 1 / a * (digamma(1) - digamma(1 + b))
# sigma2 <- 1 / a^2 * (trigamma(1) - trigamma(1 + b))
#
# logmom1(mu, sigma2)
# print(c(a, b))
#
# microbenchmark::microbenchmark(logmom1(mu, sigma2), times = 1000)
#
#
# a <- 2.171274e-03
# b <- 22.257825
# mu <- 1 / a * (digamma(1) - digamma(1 + b))
# sigma2 <- 1 / a^2 * (trigamma(1) - trigamma(1 + b))
#
# logmom1(mu, sigma2)
#
#
# a <- 1
# b <- 22.257825
# x <- extraDistr::rkumar(100, a, b)
#
# mlkumar(x)
#
#
#
# failed_params <- list()
#
# set.seed(313)
# results <- replicate(100000, {
#   a <- runif(1, 0, 1)
#   b <- runif(1, 0, 100)
#   x <- extraDistr::rkumar(100, a, b)
#
#   # avoid log(0)
#   if (any(x <= 0)) {
#     failed_params[[length(failed_params) + 1]] <<- c(NA_real_, NA_real_)
#     return(NA_real_)
#   }
#
#   log_x <- log(x)
#   mu <- mean(log_x)
#   sigma2 <- var(log_x)
#
#   tryCatch(
#     logmom1(mu, sigma2),
#     error = function(e) {
#       failed_params[[length(failed_params) + 1]] <<- c(mu, sigma2)
#       NA_real_
#     }
#   )
# })
#
# failed_matrix <- do.call(rbind, failed_params)
# sum(is.na(failed_matrix))
# length(failed_matrix)
