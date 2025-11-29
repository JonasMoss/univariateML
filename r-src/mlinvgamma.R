mlinvgamma <- function(x, na.rm = FALSE, ...) {}


mlinvgamma_ <- function(x, ...) {
  gamma_ml <- mlgamma_(1 / x, ...)
  alpha <- gamma_ml$estimates[1]
  beta <- gamma_ml$estimates[2]

  L <- mean(log(x))
  M <- mean(1 / x)
  logLik <- length(x) * (alpha * log(beta) - log(gamma(alpha)) - (alpha + 1) * L - beta * M)

  list(estimates = gamma_ml$estimates, logLik = unname(logLik))
}
