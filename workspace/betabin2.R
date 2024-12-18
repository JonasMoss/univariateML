mlbbinom_ <- \(x, size, ...) {
  tab <- Rfast::Table(x)
  uniques <- as.integer(names(tab))
  n <- length(x)

  f <- \(p) {
    alpha <- p[1]
    beta <- p[2]
    sum_lbeta <- sum(lbeta(uniques + alpha, beta + size - uniques) * tab)
    out <- -sum_lbeta + n * lbeta(alpha, beta)
    attr(out, "gradient") <- {
      digammas <- digamma(alpha + beta) - digamma(alpha + beta + size)
      digamma_alpha <- digammas - digamma(alpha)
      digamma_beta <- digammas - digamma(beta)
      sum_alpha <- sum(Rfast::Digamma(alpha + uniques) * tab)
      sum_beta <- sum(Rfast::Digamma(beta + size - uniques) * tab)
      -c(n * digamma_alpha + sum_alpha, n * digamma_beta + sum_beta)
    }
    out
  }

  m1 <- sum(tab * uniques) / n
  m2 <- sum(tab * uniques^2) / n

  alpha <- (size * m1 - m2) / (size * (m2 / m1 - m1 - 1) + m1)
  beta <- (size - m1) * (size - m2 / m1) / (size * (m2 / m1 - m1 - 1) + m1)
  start <- c(alpha, beta)
  nlm(f, c(alpha, beta))
}

x <- extraDistr::rbbinom(10, 10, 40, 40)
mlbbinom_(x, size = 10)
