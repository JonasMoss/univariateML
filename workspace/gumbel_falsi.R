newton_secant <- \(f_df, x0) {
  reltol <- .Machine$double.eps^0.25
  iterlim <- 100
  fdf0 <- f_df(x0)
  x1 <- x0 - fdf0[1] / fdf0[2]
  print(x1)
  f0 <- fdf0[1] / sqrt(fdf0[2])
  for (i in seq(iterlim)) {
    fdf1 <- f_df(x1)
    f1 <- fdf1[1] / sqrt(fdf1[2])
    secant <- (f1 - f0) / (x1 - x0)
    tmp <- x1 - f1/secant
    x0 <- x1
    x1 <- tmp
    f0 <- f1
    print(x1)
    if (is.nan(x1)) stop("NaN in secant iteration.")
    if (abs((x0 - x1) / x0) < reltol) break
  }

  if (i == iterlim) {
    warning(paste0(
      "The iteration limit (iterlim = ", iterlim, ") was reached",
      " before the relative tolerance requirement (reltol = ",
      reltol, ")."
    ))
  }
  print(i+1)
  x1
}

x <- extraDistr::rgumbel(100, 9, 0.1)
x <- rgamma(100, 1, 1)

n <- length(x)
mean_hat <- sum(x) / n
lx_bar <- sum(log(x)) / n
s <- log(mean_hat) - lx_bar

f_df <- \(shape0) {
  c(-(log(shape0) - digamma(shape0) - s), -(1 / shape0 - trigamma(shape0)))
}

f_over_df <- \(shape0) {
  (log(shape0) - digamma(shape0) - s) / (1 / shape0 - trigamma(shape0))
}

x0 <- 1 / (12 * s) * (3 - s + sqrt((s - 3)^2 + 24 * s))
x0 <- 1.5
newton_secant(f_df, x0)
newton_raphson_1d(f_over_df, x0)

n <- length(x)
log_x <- log(x)
lx_sum <- sum(log_x)
f_over_df <- \(c) {
  x_c <- x^c
  x_c_div <- x_c / (x_c + 1)
  g <- sum(log(1 + x_c))
  dg <- sum(log_x * x_c_div)
  d2g <- sum(log_x^2 * x_c_div) - sum((log_x * x_c_div)^2)
  f <- n / c - n * dg / g + lx_sum - dg
  df <- -n / c^2 - n * (g * d2g - dg^2) / g^2 - d2g
  f / df
}


x<-actuar::rburr(100, 10, 10)

n <- length(x)
log_x <- log(x)
lx_sum <- sum(log_x)

f_over_df <- \(c) {
  x_c <- x^c
  x_c_div <- x_c / (x_c + 1)
  g <- sum(log(1 + x_c))
  dg <- sum(log_x * x_c_div)
  d2g <- sum(log_x^2 * x_c_div) - sum((log_x * x_c_div)^2)
  f <- n / c - n * dg / g + lx_sum - dg
  df <- -n / c^2 - n * (g * d2g - dg^2) / g^2 - d2g
  f / df
}

f_df <- \(c) {
  x_c <- x^c
  x_c_div <- x_c / (x_c + 1)
  g <- sum(log(1 + x_c))
  dg <- sum(log_x * x_c_div)
  d2g <- sum(log_x^2 * x_c_div) - sum((log_x * x_c_div)^2)
  f <- n / c - n * dg / g + lx_sum - dg
  df <- -n / c^2 - n * (g * d2g - dg^2) / g^2 - d2g
  -c(f, df)
}


#x0 <- 20
y <- seq(0.001, 2, by = 0.01)
#plot(y, sapply(y, \(y) -f_df(y)[1]), type = "l")
plot(y, sapply(y, \(y) f_df(y)[1]/sqrt(f_df(y)[2])), type = "l", col = "red")
abline(h = 0,col = "grey")
abline(v = x0)
x0 <- 4.953032*(sum(log_x * log_x) / n)^(-0.46)
newton_raphson_1d(f_over_df, x0)
newton_secant(f_df, x0)

x0 <- 0.385/(sum(log_x * log_x) / n)^(1/3)
microbenchmark::microbenchmark(newton_secant(f_df, x0),
                               newton_raphson_1d(f_over_df, x0))

# x <- x - mean(x)
# x2 <- x^2
#
# f_df <- \(sigma0) {
#   neg_sigma_inv <- -1 / sigma0
#   exps <- exp(x * neg_sigma_inv)
#   psi0 <- sum(exps)
#   psi1 <- sum(x * exps) / psi0
#   psi2 <- sum(x2 * exps) / psi0 - psi1^2
#   f <- sigma0 + psi1
#   df <- 1 + neg_sigma_inv^2 * psi2
#   c(f, df)
# }
# f_over_df <- \(sigma0) {
#   fdf0 <- f_df(sigma0)
#   fdf0[1] / fdf0[2]
# }
#
# x0 <- sqrt(sum(x^2) / length(x)) * 2.45 / pi * (1 - 1 / length(x))
# newton_secant(f_df, x0)
# newton_raphson_1d(f_over_df, x0)



f_df <- \(x) c(x^2 - 2, 2*x)
x0 <- 2

newton_secant(f_df, x0)