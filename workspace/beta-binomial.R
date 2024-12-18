x <- extraDistr::rbbinom(10000, 10, 40, 40)
x <- rbinom(1000, 10, 0.3)

size <- 10
grad <- \(p) {
  alpha <- p[1]
  beta <- p[2]
  tab <- Rfast::Table(x)
  uniques <- as.integer(names(tab))
  n <- length(x)

  digammas <- digamma(alpha + beta) - digamma(alpha + beta + size)
  digamma_alpha <- digammas - digamma(alpha)
  digamma_beta <- digammas - digamma(beta)
  sum_alpha = sum(digamma(alpha + uniques) * tab)
  sum_beta = sum(digamma(beta + size - uniques) * tab)

  -c(n * digamma_alpha + sum_alpha, n * digamma_beta + sum_beta)
}

tab <- Rfast::Table(x)
uniques <- as.integer(names(tab))
n <- length(x)
f <- \(p) {
  alpha <- p[1]
  beta <- p[2]
  sum_lbeta= sum(lbeta(uniques + alpha, beta + size - uniques) * tab)
  out <- -sum_lbeta + n*lbeta(alpha, beta)
  attr(out, "gradient") <- {
    digammas <- digamma(alpha + beta) - digamma(alpha + beta + size)
    digamma_alpha <- digammas - digamma(alpha)
    digamma_beta <- digammas - digamma(beta)
    sum_alpha = sum(Rfast::Digamma(alpha + uniques) * tab)
    sum_beta = sum(Rfast::Digamma(beta + size - uniques) * tab)

    -c(n * digamma_alpha + sum_alpha, n * digamma_beta + sum_beta)
  }
  out
}

g <- \(p) {
  alpha <- p[1]
  beta <- p[2]
  tab <- Rfast::Table(x)
  uniques <- as.integer(names(tab))
  n <- length(x)

  sum_lbeta= sum(lbeta(uniques + alpha, beta + size - uniques) * tab)
  -sum_lbeta + n*lbeta(alpha, beta)
}

numDeriv::grad(f, c(1,1))
grad(c(1,1))

beta <- 40
h <- \(alpha) {
  tab <- Rfast::Table(x)
  uniques <- as.integer(names(tab))
  n <- length(x)

  digammas <- digamma(alpha + beta) - digamma(alpha + beta + size)
  digamma_alpha <- digammas - digamma(alpha)
  sum_alpha = sum(digamma(alpha + uniques) * tab)

  trigammas <- trigamma(alpha + beta) - trigamma(alpha + beta + size)
  trigamma_alpha <- trigammas - trigamma(alpha)
  sum_trigamma <- sum(trigamma(alpha + uniques) * tab)

  (n * digamma_alpha + sum_alpha) / (n* trigamma_alpha + sum_trigamma)
}

x_bar <- mean(x)
beta <- 40
beta * x_bar / (size - x_bar)
newton_raphson_1d(h, beta * x_bar / (size - x_bar))


m1 <- mean(x)
m2 <- mean(x^2)

size <- 11
alpha <- (size * m1 - m2) / (size*(m2/m1 - m1 - 1) + m1)
beta <- (size - m1) * (size - m2/m1) / (size * (m2/m1 - m1 - 1) + m1)
start <- c(alpha, beta)

microbenchmark::microbenchmark(nlm(\(p) f(p), c(alpha,beta)),
                               nlm(\(p) g(p), c(alpha,beta)))

nlm(\(p) f(p), c(alpha,beta))
nlm(\(p) g(p), c(alpha,beta))
nlm(\(p) sum(grad(p)*grad(p)), c(50,50))






