mlcauchy <- function(x, na.rm = FALSE, ...) {}


mlcauchy_ <- function(x, ...) {
  m <- stats::median(x)
  mad <- stats::median(abs(x - m))
  start <- c(m, mad)

  f <- function(p) -sum(stats::dcauchy(x, p[1], exp(p[2]), log = TRUE))
  values <- suppressWarnings(stats::nlm(f = f, p = start))

  list(
    estimates = c(values$estimate[1], exp(values$estimate[2])),
    logLik = -values$minimum
  )
}
