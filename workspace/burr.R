x <- actuar::rburr(100, 1, 1)

#x <- egypt$age
#x[1] <- 0.9

x[x < 1] = 1
#x[1] = 0.99

#x = c(1,1,1,1,1,1,1, 1)
#x <- x * 0.5
###
### Verify loglikelihood.
###

loglik <- \(beta) {
  alpha <- n / (beta * sum(log(1+x^(1/beta))))
  h <- mean(log(1+x^(1/beta)))
  s <- mean(log(x))
  n * log(abs(alpha)) + (1/beta - 1) * s * n - n*(abs(alpha) * beta + 1) * h
}

loglik2 <- \(beta) {
  c <- 1/beta
  k <- \(c) n / sum(log(1+x^c))
  sum(actuar::dburr(egypt$age, k(c), c, log = TRUE))
}

beta <- 5
loglik(beta)
loglik2(beta)

###
### Plot likelihood
###

betas <- seq(0.0000000001, 0.1, by = 0.0001)
plot(betas, sapply(betas, loglik), type = "l")
plot(betas, sapply(betas, r))

betas <- seq(0.01, 3, by = 0.001)
plot(betas, sapply(betas, alpha))
###
### Verify derivative of alpha
###

r <- \(beta) mean(x^(1/beta) * log(x) / (x^(1/beta) + 1))
alpha <- \(beta) n / (beta * sum(log(1+x^(1/beta))))
numDeriv::grad(alpha, beta)
alpha(beta) / beta * (alpha(beta)*r(beta) - 1)

###
### Verify derivative of r
###
dr <- \(beta) {
  1/beta^2 * (mean(((x^(1/beta) * log(x)) / (x^(1/beta) + 1))^2) - mean((x^(1/beta) * log(x)^2) / (x^(1/beta) + 1)))
}
numDeriv::grad(r, beta)
dr(beta)

###
### Verify log-derivative of alpha
###

log_alpha <- \(beta) log(n / (beta * sum(log(1+x^(1/beta)))))
numDeriv::grad(log_alpha, beta)
1 / beta * (alpha(beta)*r(beta) - 1)

###
### Verify derivative of 1/(beta*alpha)
###

beta_alpha <- \(beta) (sum(log(1+x^(1/beta)))) / n
numDeriv::grad(beta_alpha, beta)
-r(beta) / beta^2

###
### Verify derivative of loglikelihood
###

beta <- 0.02
numDeriv::grad(loglik, beta)
s <- mean(log(x))
f <- n / beta^2 * (alpha(beta) * r(beta) * beta - beta - s + r(beta))
f

###
### Verify second derivative of loglikelihood
###

numDeriv::hessian(loglik, beta)
-2/beta * f + n / beta^2 *(alpha(beta)^2 * r(beta)^2 + beta*alpha(beta) * dr(beta) - 1 + dr(beta))




mlburr_ <- \(x, ...) {
  n <- length(x)
  log_x <- log(x)
  lx_sum <- sum(log_x)
  f_over_df <- \(c) {
    x_c <- x^c
    x_c_div <- x_c / (x_c + 1)
    g <- sum(log(1+x_c))
    dg <- sum(log_x * x_c_div)
    d2g <- sum(log_x^2 * x_c_div) - sum((log_x*x_c_div)^2)
    f <- n/c - n * dg / g + lx_sum - dg
    df <- -n/c^2 - n * (g * d2g - dg^2)/g^2 - d2g
    f / df
  }

  c <- newton_raphson_1d(f_over_df, 1, ...)
  k <- n / sum(log(1+x^c))

  list(estimates = c(k, c),
       logLik = sum(actuar::dburr(x, k, c, log = TRUE)))
}


n <- 100
shape1 <- 9
shape2 <- 8
x <- actuar::rburr(n, shape1, shape2)

n / sum(log(1+x^shape2))

#x <- egypt$age
mlburr_(x)
mle2 <- nlm(\(p) {
  -mean(actuar::dburr(egypt$age, p[1], p[2], log = TRUE))
}, p =c(1, 1), iterlim = 1000)


x <- egypt$age
n <- length(x)


k <- \(c) {
  n / sum(log(1+x^c))
}

cc <- seq(1,120, by = 1)
f <- \(c) {
  k <- n / sum(log(1+x^c))
  mean(actuar::dburr(egypt$age, k, c, log = TRUE))
}

plot(cc, sapply(cc, f))

cc <- seq(1,400, by = 1)
plot(cc, sapply(cc, f))

xx <- seq(0, 10, by = 0.01)
c <- 120
g <- \(c) n / sum(log(1+x^c))
plot(xx, actuar::dburr(xx, g(c), c), type = "l", col = "red")
lines(xx, extraDistr::dpareto(xx, 1/3, 1))


f_over_df <- \(c) {
  lx_sum <- sum(log(x))
  g <- sum(log(1+x^c))
  dg <- sum(x^c*log(x) / (x^c+1))
  d2g <- sum(x^c*log(x)^2 / (x^c+1)) - sum((x^c*log(x))^2 / (x^c+1)^2)
  f <- n/c - n * dg / g + lx_sum - dg
  df <- -n/c^2 - n * (g * d2g - dg^2)/g^2 - d2g
  c(f, df)
}

f_over_df(0.5)
k <- \(c) n / sum(log(1+x^c))

true <- \(c) {
  c(numDeriv::grad(\(c) sum(actuar::dburr(x, k(c), c, log = TRUE)), c),
    numDeriv::hessian(\(c) sum(actuar::dburr(x, k(c), c, log = TRUE)), c))
}

f_over_df(0.5)
true(0.5)
