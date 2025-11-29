f_inv <- function(p, tol=.Machine$double.eps^0.5) {
  # Inverse CDF of F(x)=1-f(x) on (0,1).
  # p : numeric vector in [0,1]
  # maxiter : maximum Newton steps per evaluation

  if (any(p < 0 | p > 1)) stop("All p must lie in [0,1]")

  dig1 <- digamma(1.0)      # = -gamma
  tri1 <- trigamma(1.0)     # = pi^2/6

  alpha0 <- 1.78784229
  beta0  <- 2.82868897
  x0 <- qbeta(p, alpha0, beta0)

  # Prepare output
  q <- numeric(length(p))
  iter <- numeric(length(p))
  eps <- .Machine$double.eps

  for (i in seq_along(p)) {
    pi <- p[i]
    if (pi <= tol)      { q[i] <- 0; next }
    if (1 - pi <= tol)  { q[i] <- 1; next }

    # target T = 1-p: we solve f(x) = T
    Tgt <- 1 - pi
    xi  <- x0[i]
    xi  <- pmin(pmax(xi, eps), 1 - eps)

    # Newton iterations
    for (it in seq_len(10)) {
      A   <- digamma(xi) - dig1
      D   <- tri1 + trigamma(xi)
      fxi <- A*A / D
      err <- fxi - Tgt
      if (abs(err) < tol) break

      d1     <- trigamma(xi)
      d2     <- psigamma(xi, 2)   # tetragamma
      fprime <- (2*A*d1*D - A*A*d2) / (D*D)

      xi <- xi - err/fprime
      # clamp back into (0,1)
      xi <- pmin(pmax(xi, eps), 1 - eps)
    }

    q[i] <- xi
    iter[i] <- it
  }

  c(k = q, iter = iter)
}


f <- \(k) 1 - (digamma(k) - digamma(1))^2 / (trigamma(1) + trigamma(k))


x <- exp(seq(log(1e-7), log(1-1e-7), length.out = 1000))
#x <- exp(seq(log(1e-8), log(1-1e-8), length.out = 1000))


x <- plogis(seq(-12, 12, length.out = 1000))
y <- f(x)



results <- as.data.frame(cbind(x = x, cv = y, t(sapply(y, f_inv))))
sum((results$x - results$k)^2)
max(abs(results$x - results$k))

mean(results$iter)
table(results$iter)


plot(qlogis(results$x), abs(results$x - results$k), log = "y")

#plot(results$x, abs(results$x - results$k) / results$x)

#plot(results$x, results$k, type = "l")

options(digits = 5)
microbenchmark::microbenchmark(f_inv(0.01))