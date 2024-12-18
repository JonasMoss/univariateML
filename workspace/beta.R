a <- 9
b <- 16

x <- rbeta(100, a, b)


old <- \(s, r, n) {
  g1 <- exp(s)
  g2 <- exp(r)

  denom <- 1 / 2 * (1 / (1 - g1 - g2))
  start <- 1/2 + c(g1, g2) * denom

  objective <- \(p) lbeta(p[1], p[2]) - (p[1] - 1) * s - (p[2] - 1) * r
  fit <- stats::nlm(objective, p = start, typsize = start)
  list(estimates = fit$estimate,
       logLik = -n * fit$minimum,
       i = fit$iterations)
}

new <- \(s, r, n) {
  g1 <- exp(s)
  g2 <- exp(r)

  b <- 0.5 + 0.5 * g2 / (1 - (g1 + g2))

  reltol <- .Machine$double.eps^0.25
  iterlim <- 100

  for (i in seq(iterlim)) {
    digamma_b_r <- digamma(b) - r
    trigamma_b <- trigamma(b)

    y <- s+digamma_b_r
    a <- 1/log(1+exp(-y))
    a <- a + (y - digamma(a)) / trigamma(a)
    a <- a + (y - digamma(a)) / trigamma(a)
    a <- a + (y - digamma(a)) / trigamma(a)

    f <- digamma_b_r - digamma(a+b)
    df <- trigamma_b - trigamma(a+b) * (trigamma_b/trigamma(a)+1)
    b0 <- b - f/df
    if (abs((b0 - b) / b0) < reltol) break
    b <- b0
  }

  list(estimates = c(a, b),
       logLik = n*((a-1)*s + (b-1)*r - lbeta(a,b)),
       i = i)
}


new2 <- \(s, r, n) {
  g1 <- exp(s); g2 <- exp(r)
  b <- log(0.5 + 0.5 * g2 / (1 - (g1 + g2)))

  reltol <- .Machine$double.eps^0.25
  iterlim <- 100

  for (i in seq(iterlim)) {
    exp_b <- exp(b)
    digamma_b_r <- digamma(exp_b) - r
    trigamma_b <- trigamma(exp_b)

    # Inverts digamma(s-r+psi(exp(b)))
    y <- s+digamma_b_r
    a <- 1/log(1+exp(-y))
    a <- a + (y - digamma(a)) / trigamma(a)
    a <- a + (y - digamma(a)) / trigamma(a)
    a <- a + (y - digamma(a)) / trigamma(a)

    # Construct derivative and hessian
    f <- exp_b * (digamma_b_r - digamma(a+exp_b))
    df <- f + exp_b^2 * (trigamma_b - trigamma(a+exp_b) * (trigamma_b/trigamma(a)+1))

    b0 <- b - f/df
    if (abs((b0 - b) / b0) < reltol) break
    b <- b0
  }

  list(estimates = c(a, exp_b),
       logLik = n*((a-1)*s + (exp_b-1)*r - lbeta(a,exp_b)),
       i = i)
}


a <- 2
b <- 32
x <- extraDistr::rkumar(10, a, b)

n <- length(x)

s <- mean(log(x))
r <- mean(log(1 - x))
#s <- -1
#r <- -5

a <- 2
b <- 32
x <- rbeta(10, a, b)
s <- mean(log(x))
r <- mean(log(1 - x))

delta <- 0.01
xi <- 0.02
s <- log(delta) + log(xi)
r <- log(1-delta) +log(1-xi)
microbenchmark::microbenchmark(new(s,r,n), new2(s,r,n),old(s,r,n))

new2(s, r, n)
old(s, r, n)

f <- \(b) {
  #b <- exp(b)
  digamma_b_r <- digamma(b) - r
  y <- s+digamma_b_r
  a <- 1/log(1+exp(-y))
  a <- a + (y - digamma(a)) / trigamma(a)
  a <- a + (y - digamma(a)) / trigamma(a)
  a <- a + (y - digamma(a)) / trigamma(a)

  n*((a-1)*s + (b-1)*r - lbeta(a,b))
}

bb <- seq(0.0001,0.1, by = 0.001)
plot(bb, sapply(bb, f), type = "l")
abline(v = 0.03902641)

g <- \(b) {
  digamma_b_r <- digamma(b) - r
  trigamma_b <- trigamma(b)

  y <- s+digamma_b_r
  a <- 1/log(1+exp(-y))
  a <- a + (y - digamma(a)) / trigamma(a)
  a <- a + (y - digamma(a)) / trigamma(a)
  a <- a + (y - digamma(a)) / trigamma(a)
  n*((a-1)*s + (b-1)*r - lbeta(a,b))
}


plot(xx, sapply(xx, \(x) numDeriv::grad(f, x)))

old(x)$logLik - new(x)$logLik


x<-1
microbenchmark::microbenchmark(trigamma(x), digamma(x), exp(x), log(x), x*x, if(TRUE) x, psigamma(x, c(1,2)), times =10000)

microbenchmark::microbenchmark(inverse_digamma(x))

set.seed(1)
mean(replicate(10000, {x <- rnorm(1000, sd = 10)
shapiro.test(x)$p.value < 0.05}))

mean(replicate(10000, {x <- ceiling(rnorm(1000, sd = 10))
shapiro.test(x)$p.value < 0.05}))



mlbeta(x)

g <- \(b) {
  a <- inverse_digamma(s-r+digamma(b))
  digamma(b) - r - digamma(a+b)
}

numDeriv::grad(g, b)

a <- inverse_digamma(s-r+digamma(b))
f <- digamma(b) - r - digamma(a+b)
trigamma(b) - trigamma(a+b)*(trigamma(b)/trigamma(a)+1)