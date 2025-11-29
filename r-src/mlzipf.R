mlzipf_ <- function(x, ...) {
  N <- max(x)
  n <- length(x)
  sum_lx <- sum(log(x))
  log_seq <- log(seq(N))
  log_seq2 <- log(seq(N))^2
  div_seq <- 1 / seq(1, N)

  if (sum_lx == 0) {
    warning("All observations are equal to 1. The maximum likelihood estimator is not unique.")
    return(list(estimates = c(N, 1), logLik = 0))
  }

  f_over_df <- function(shape0) {
    div_seq_shape0 <- div_seq^shape0
    sum_shape <- sum(div_seq_shape0)
    sum_log_hs <- sum(div_seq_shape0 * log_seq)
    sum_log2_hs <- sum(div_seq_shape0 * log_seq2)
    top <- sum_log_hs / sum_shape
    bottom <- -n * sum_log2_hs / sum_shape + n * top^2
    (n * top - sum_lx) / bottom
  }

  shape0 <- 1
  shape <- newton_raphson_1d(f_over_df, shape0, ...)

  if (shape < 0) {
    stop("Optimal shape parameter is less than 0. The maximum likelihood estimator does not exist.")
  }

  logLik <- -shape * sum_lx - n * log(sum(1 / seq(1, N)^shape))
  list(estimates = c(N, shape), logLik = logLik)
}
