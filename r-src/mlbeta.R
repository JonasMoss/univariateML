mlbeta <- function(x, na.rm = FALSE, ...) {}


mlbeta_ <- function(x, ...) {
  n <- length(x)
  s <- sum(log(x)) / n
  r <- sum(log1p(-x)) / n

  reltol <- .Machine$double.eps^0.5
  iterlim <- 100

  m <- sum(x) / n
  v <- sum(x^2) / n - m^2
  nu <- m * (1 - m) / v - 1
  b <- log((1 - m) * nu)

  for (i in seq(iterlim)) {
    exp_b <- exp(b)
    digamma_b_r <- digamma(exp_b) - r
    trigamma_b <- trigamma(exp_b)

    # Inverts digamma(s-r+digamma(exp(b)))
    y <- s + digamma_b_r
    a <- 1 / log(1 + exp(-y))
    a <- a + (y - digamma(a)) / trigamma(a)
    a <- a + (y - digamma(a)) / trigamma(a)
    a <- a + (y - digamma(a)) / trigamma(a)

    # Construct derivative and hessian
    f <- exp_b * (digamma_b_r - digamma(a + exp_b))
    df <- f + exp_b^2 * (trigamma_b - trigamma(a + exp_b) * (trigamma_b / trigamma(a) + 1))

    b0 <- b - f / df
    if (abs((b0 - b) / b0) < reltol) break
    b <- b0
  }

  list(
    estimates = c(a, exp_b),
    logLik = n * ((a - 1) * s + (exp_b - 1) * r - lbeta(a, exp_b)),
    i = i
  )
}

