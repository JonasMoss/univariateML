mllgser_ <- function(x, ...) {
  res <- mllgser_rcpp(as.integer(x))
  list(estimates = unname(res$estimates), logLik = res$logLik)
}
