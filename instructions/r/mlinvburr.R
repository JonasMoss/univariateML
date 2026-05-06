mlinvburr_ <- function(x, ...) {
  params <- mlburr_(1/x)$estimates
  list(
    estimates = params,
    logLik = sum(actuar::dinvburr(x, params[1], params[2], log = TRUE))
  )
}
