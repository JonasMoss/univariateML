#set.seed(313)
a <- 4
b <- 5
n <- 100
x <- extraDistr::rgompertz(n, a, b)

q <- 0.25
r <- 0.75

c <- log(q)
d <- log(r)

a <- exp(quantile(x, 1-q))
b <- exp(quantile(x, 1-r))
(c - d) / (b*c - a*d)


# Here we have a new kind of problem I think. Negative values of `b` will
# *also* yield a density, but not with the same support.
# Here the density class is incomplete in the sense that there are densities,
# on the same form, that aren't covered by the class.
# Thi class isn't even closed in my previous sense, since b->0 converges to the
# exponential pointwise.


mlgompertz_ <- \(x, ...) {
  n <- length(x)
  x_sum <- sum(x)
  f_over_df <- \(b) {
    exp_bx <- exp(b*x)
    sum_exp_bx <- sum(exp_bx)
    sum_exp_bx_x <- sum(x*exp_bx)
    sum_exp_bx_x2 <- sum(x^2*exp_bx)
    sum_minus <- sum_exp_bx - n
    sum_exp_bx_mod <- b * sum_exp_bx_x - sum_minus

    a <- b*n / sum_minus
    da <- -n * sum_exp_bx_mod / sum_minus^2
    d2a <- n/sum_minus^2*(b*(2*sum_exp_bx_x^2 / sum_minus - sum_exp_bx_x2)
                          - 2*sum_exp_bx_x)

    f <- x_sum - a/b^2*sum_exp_bx_mod + n*da/a - da / b * sum_minus
    df <- n * (a * d2a - da^2) / a^2
    f/df
  }

  b <- newton_raphson_1d(f_over_df, 1)
  a <- b*n / (sum(exp(b*x)) - n)
  c(a,b)
}

mlgompertz_(x)
mle2 <- nlm(\(p) {
  -mean(extraDistr::dgompertz(x, p[1], p[2], log = TRUE))
}, p =c(50, 50))

f <- \(b) {
  x_sum <- sum(x)
  exp_bx <- exp(b*x)
  sum_exp_bx <- sum(exp_bx)
  sum_exp_bx_x <- sum(x*exp_bx)
  sum_exp_bx_x2 <- sum(x^2*exp_bx)
  sum_minus <- sum_exp_bx - n
  sum_exp_bx_mod <- b * sum_exp_bx_x - sum_minus

  a <- b*n / (sum_exp_bx - n)
  da <- -n * (sum_exp_bx_mod) / (sum_exp_bx - n)^2
  d2a <- n/sum_minus^2*(b*(2*sum_exp_bx_x^2 / sum_minus - sum_exp_bx_x2)
                        - 2*sum_exp_bx_x)

  f <- x_sum - a/b^2*(sum_exp_bx_mod) + n*da/a - da / b * sum_minus
  df <- n * (a * d2a - da^2) / a^2
  f
}

bb <- seq(-2,5, by = 0.01)
plot(bb, sapply(bb, f))




a_hat <- \(b) {
  exp_bx <- exp(b*x)
  b*n / (sum(exp_bx) - n)
}

da <- \(b) {
  bx_m_1 <- b*x - 1
  exp_bx <- exp(b*x)
  -n * (sum(exp_bx*bx_m_1) + n) / (sum(exp_bx) - n)^2
}

numDeriv::grad(a_hat, b)
da(b)


#mlgompertz_(x)



grad <- \(b) {
  x_sum <- sum(x)
  bx_m_1 <- b*x - 1
  exp_bx <- exp(b*x)
  sum_exp_bx <- sum(exp(b*x))
  sum_exp_bx_m_1 <- sum(exp_bx*bx_m_1)
  a <- b*n / (sum_exp_bx  - n)
  da <- -n * (sum_exp_bx_m_1 + n) / (sum_exp_bx - n)^2
  x_sum - a/b^2*(sum_exp_bx_m_1  + n) + n*da/a - da / b * (sum_exp_bx  - n)
}

f_over_df <- \(b) {
  x_sum <- sum(x)
  exp_bx <- exp(b*x)
  sum_exp_bx <- sum(exp(b*x))
  sum_exp_bx_m_1 <- sum(exp_bx*(b*x - 1))
  sum_exp_bx_x <- sum(x*exp_bx)
  sum_exp_bx_x2 <- sum(x^2*exp_bx)
  sum_minus <- sum_exp_bx - n

  a <- b*n / (sum_exp_bx - n)
  da <- -n * (sum_exp_bx_m_1 + n) / (sum_exp_bx - n)^2
  d2a <- n/sum_minus^2*(b*(2*sum_exp_bx_x^2 / sum_minus - sum_exp_bx_x2)
              - 2*sum_exp_bx_x)

  f <- x_sum - a/b^2*(sum_exp_bx_m_1 + n) + n*da/a - da / b * sum_minus
  df <- n * (a * d2a - da^2) / a^2
  c(f, df)
}


true <- \(b) {
  c(numDeriv::grad(\(b) sum(extraDistr::dgompertz(x, a_hat(b), b, log = TRUE)), b),
    numDeriv::hessian(\(b) sum(extraDistr::dgompertz(x, a_hat(b), b, log = TRUE)), b))
}

c(f_over_df(b), true(b))



f <- \(b) {
  numDeriv::grad(\(b) sum(extraDistr::dgompertz(x, a_hat(b), b, log = TRUE)), b)
}

c(grad(b), f(b))


hess_a <- \(b) {
  n <- length(x)
  x_sum <- sum(x)
  bx_m_1 <- b*x - 1
  exp_bx <- exp(b*x)
  sum_exp_bx <- sum(exp(b*x))
  sum_exp_bx_m_1 <- sum(exp_bx*bx_m_1)
  sum_minus <- sum_exp_bx - n

  n*(b*(2*sum(x*exp_bx)^2 / sum_minus^3 - sum(x^2*exp_bx) / sum_minus^2)
       - 2*sum(x*exp_bx)/sum_minus^2)
}

hess_a <- \(b) {
  x_sum <- sum(x)
  bx_m_1 <- b*x - 1
  exp_bx <- exp(b*x)
  sum_exp_bx <- sum(exp(b*x))
  sum_exp_bx_m_1 <- sum(exp_bx*bx_m_1)
  sum_exp_bx_x <- sum(x*exp_bx)
  sum_exp_bx_x2 <- sum(x^2*exp_bx)
  sum_minus <- sum_exp_bx - n

  a <- b*n / (sum_exp_bx  - n)
  da <- -n * (sum_exp_bx_m_1 + n) / (sum_exp_bx - n)^2
  d2a <- n*(b*(2*sum_exp_bx_x^2 / sum_minus^3 - sum(x^2*exp_bx) / sum_minus^2)
            - 2*sum_exp_bx_x/sum_minus^2)
  (a * d2a - da^2) / a^2
}

a_hat <- \(b) {
  exp_bx <- exp(b*x)
  b*n / (sum(exp_bx) - n)
}
f <- \(b) {
  numDeriv::hessian(\(b) log(a_hat(b)), b)
}

f(b)
hess_a(b)



bb <- seq(0.1, 19, by = 0.1)
sapply(bb, f) - sapply(bb, grad)

bb <- seq(0.1, 19, by = 0.1)
lines(bb, sapply(bb, f), type = "l")




p <- c(a, b)
mle2 <- nlm(\(p) {
  -mean(extraDistr::dgompertz(x, p[1], p[2], log = TRUE))
}, p =c(1, 1))


microbenchmark::microbenchmark(mlgompertz_(x), nlm(\(p) {
  -mean(extraDistr::dgompertz(x, p[1], p[2], log = TRUE))
}, p =c(1, 1)))

x1 <- mle2$minimum
x2 <- -mean(extraDistr::dgompertz(x, a, b, log = TRUE))



a_hat <- \(b) {
  bx_m_1 <- b*x - 1
  b*n / (sum(exp(b*x)) - n)
}

da <- \(b) {
  n * (sum(exp_bx*bx_m_1) + n)

}



g <- \(b) sum(extraDistr::dgompertz(x, a, b, log = TRUE))

c(numDeriv::grad(g, b), f)
c(numDeriv::hessian(g, b), df)

g <- \(p) -sum(extraDistr::dgompertz(x, p[1], p[2], log = TRUE))




# b = exp(eta) * ei(eta) / mean(x)

p <- c(a, b)
mle2 <- nlm(\(p) {
  -mean(dweibull(exp(-x), p[1], p[2], log = TRUE))
}, p = c(3, 7))$estimate


p <- c(a, b)
mle2 <- nlm(\(p) {
  -mean(extraDistr::dgompertz(x, p[1], p[2], log = TRUE))
}, p =c(a, b))$estimate

q <- 0.1
r <- 0.5

c <- log(q)
d <- log(r)

a <- exp(quantile(x, c(1-q)))
b <- exp(quantile(x, c(1-r)))
log(-(c - d) / (b*c - a*d))


(1 - b/a) / (d - c*b/a)
(a/b - 1) / (a/b*d - c)

a1 = 0.01882586
b1 = 0.02074556

a1/b1

(c - d) / (b*c - a*d)
