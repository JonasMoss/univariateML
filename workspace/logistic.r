x <- rlogis(2000, 1, 2)

microbenchmark::microbenchmark(mllogis_(x), mllogis2_(x))

p <- c(1,2)

sum_x <- sum(x)

f <- \(p) {
  n<-length(x)
  mu <- p[1]
  s <- p[2]
  -(sum_x - n*mu)/s - n*log(s) - 2*sum(log(1+exp(-(x-mu)/s)))
}

mllogis2_ <- \(x, ...) {
  m <- stats::median(x)
  mad <- stats::median(abs(x - m))
  n <- length(x)
  sum_x <- sum(x)
  f <- \(p) {
    mu <- p[1]
    s <- p[2]^2
    (sum_x - n*mu)/s + n*log(s) + 2*sum(log1p(exp(-(x-mu)/s)))
  }
  values <- suppressWarnings(stats::nlm(f = f,p = c(m, mad^2)))
  list(estimates = c(values$estimate[1], exp(values$estimate[2])),
       logLik = -values$minimum,
       i = values$iterations)
}

microbenchmark::microbenchmark(mllogis_(x), mllogis2_(x))