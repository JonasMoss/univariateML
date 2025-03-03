f <- function(n) {
  g <- Vectorize(function(i) (-1)^(i + 1) * factorial(i))
  values <- g(seq(n))
  h <- function(m) kStatistics::e_eBellPol(n = n, m = m, values)
  sapply(seq(n), h)
}

f(8)
