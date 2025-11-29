mllogis <- function(x, na.rm = FALSE, ...) {}


mllogis_ <- function(x, ...) {
  m <- stats::median(x)
  mad <- stats::median(abs(x - m))
  f <- function(p) -sum(stats::dlogis(x, p[1], exp(p[2]), log = TRUE))
  values <- suppressWarnings(stats::nlm(f = f, p = c(m, log(mad))))
  list(
    estimates = c(values$estimate[1], exp(values$estimate[2])),
    logLik = -values$minimum,
    i = values$iterations
  )
}
