mlbinom_ <- function(x, ...) {
  dots <- list(...)

  n <- length(x)
  x_bar <- mean(x)
  n <- length(x)
  counts <- Rfast::Table(x)
  uniques <- as.numeric(names(counts))

  log_likelihood <- function(size) {
    n * x_bar * log(x_bar) +
      n * (size - x_bar) * log(size - x_bar) -
      n * size * log(size) +
      sum(counts * lchoose(size, uniques))
  }

  if (!is.null(dots$size)) {
    size <- dots$size
    if (size < max(x)) stop("`size` is smaller than `max(x)`.")
    prob <- x_bar / size
    return(list(estimates = c(size, prob), logLik = log_likelihood(size)))
  }

  if (mean(x) / (stats::var(x) * (n - 1) / n) <= 1) {
    stop("The maximum likelihood estimator does not exist. Use `mlpois` to fit a Poisson or supply a `size` argument.")
  }

  f_over_df <- function(size) {
    size_m_x <- size - x_bar
    f <- n * (log(size_m_x) - log(size)) +
      n * digamma(size + 1) -
      sum(counts * digamma(size - uniques + 1))

    df <- n * x_bar / (size * size_m_x) +
      n * trigamma(size + 1) -
      sum(counts * trigamma(size - uniques + 1))

    f / df
  }

  size0 <- max(x)
  size_cands <- size0
  if (size0 != 1) {
    if (f_over_df(size0) > 0) {
      size_cand <- newton_raphson_1d(f_over_df, size0)
      size_cands <- c(size0, floor(size_cand), ceiling(size_cand))
    }
  }

  log_liks <- sapply(size_cands, log_likelihood)
  size <- size_cands[which.max(log_liks)]

  list(
    estimates = c(size = size, prob = x_bar / size),
    logLik = max(log_liks)
  )
}
