mllogis_ <- function(x, ...) {
  fit <- mllogis_fast(x, tol = 1e-12, maxit = 1000)
  start <- unname(fit$estimates)
  negloglik <- function(par) -sum(stats::dlogis(x, par[1], par[2], log = TRUE))
  opt <- stats::optim(start, negloglik, method = "BFGS", control = list(reltol = 1e-12, maxit = 100))
  estimates <- opt$par
  logLik <- -opt$value
  list(
    estimates = estimates,
    logLik = logLik,
    iterations = fit$iterations + opt$counts["function"]
  )
}
