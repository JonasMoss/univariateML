mlgumbel <- function(x, na.rm = FALSE, ...) {}


mlgumbel_ <- function(x, ...) {
  x_bar <- sum(x) / length(x)
  estimates <- mlgumbel_estimate(x - x_bar, ...)
  mu <- estimates[1] + x_bar
  sigma <- estimates[2]
  logLik <- sum(extraDistr::dgumbel(x, mu, sigma, log = TRUE))
  list(estimates = c(mu = mu, sigma = sigma), logLik = logLik, iterations = estimates[3])
}

mlgumbel_estimate <- function(x, ...) {
  x2 <- x^2
  dots <- list(...)
  reltol <- if (!is.null(dots$reltol)) dots$reltol else .Machine$double.eps^0.25
  iterlim <- if (!is.null(dots$iterlim)) dots$iterlim else 100

  f_over_df <- function(sigma0) {
    neg_sigma_inv <- -1 / sigma0
    exps <- exp(x * neg_sigma_inv)
    psi0 <- sum(exps)
    psi1 <- sum(x * exps) / psi0
    psi2 <- sum(x2 * exps) / psi0 - psi1^2
    f <- sigma0 + psi1
    df <- 1 + neg_sigma_inv^2 * psi2
    f / df
  }

  sigma0 <- sqrt(sum(x2) / length(x)) * 2.45 / pi
  sigma <- sigma0

  for (i in seq_len(iterlim)) {
    sigma_new <- sigma - f_over_df(sigma)
    if (is.nan(sigma_new)) stop("NaN in Newton--Raphson iteration.")
    if (abs((sigma_new - sigma) / sigma_new) < reltol) {
      sigma <- sigma_new
      break
    }
    sigma <- sigma_new
  }

  if (i == iterlim) {
    warning(paste0(
      "The iteration limit (iterlim = ", iterlim, ") was reached",
      " before the relative tolerance requirement (reltol = ",
      reltol, ")."
    ))
  }

  neg_sigma_inv <- -1 / sigma
  psi0 <- sum(exp(x * neg_sigma_inv))
  mu <- -sigma * log(psi0 / length(x))
  c(mu, sigma, iterations = i)
}
