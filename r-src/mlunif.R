mlunif <- function(x, na.rm = FALSE, ...) {}


mlunif_ <- function(x, ...) {
  max_ <- max(x)
  min_ <- min(x)
  list(estimates = c(min_, max_), logLik = -length(x) * log(max_ - min_))
}
