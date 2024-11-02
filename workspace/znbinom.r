
a <- 1.5
p <- 0.9
numDeriv::grad(\(a) pbeta(p, a, 1, log.p=TRUE), a)
log(p)
numDeriv::hessian(\(a) pbeta(p, a, 1, log.p=TRUE), a)
0

numDeriv::grad(\(p) pbeta(p, a, 1, log.p=TRUE), p)
a/p
numDeriv::hessian(\(p) pbeta(p, a, 1, log.p=TRUE), p)
-a/p^2


mlnbinom_ <- \(x, ...) {
  dots <- list(...)
  reltol <- get_reltol(dots)
  iterlim <- get_iterlim(dots)

  n <- length(x)
  x_bar <- mean(x)
  get_prob <- \(size) size / (x_bar + size)

  if (is.null(dots$size)) {
    dget_prob <- \(size) x_bar / (size^2 + x_bar * size)




    f <- \(size) sum(digamma(x + size)) - n * digamma(size) + n * log(get_prob(size))
    df <- \(size) sum(trigamma(x + size)) - n * trigamma(size) + n * dget_prob(size)
    size0 <- x_bar / 2

    for (i in seq(iterlim)) {
      size <- size0 - f(size0) / df(size0)
      if (abs((size0 - size) / size0) < reltol) break

      size0 <- size
    }
  } else {
    size <- dots$size
  }

  prob <- get_prob(size)
  print(i)

  logLik <- sum(lgamma(x + size)) - sum(lfactorial(x)) - n * lgamma(size) +
    n * size * log(prob) + n * log(1 - prob) * x_bar

  list(estimates = c(size, prob), logLik = logLik)
}
