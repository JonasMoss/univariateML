halley <- \(fs, x0) {
  reltol <- .Machine$double.eps^0.25
  iterlim <- 100
  for (i in seq(iterlim)) {
    fs_ <- fs(x0)
    x1 <- x0 - fs_[1] / (fs_[2] - 0.5 * fs_[1] * fs_[3]/fs_[2])
    if (is.nan(x1)) stop("NaN in secant iteration.")
    if (abs((x0 - x1) / x0) < reltol) break
    x0 <- x1
  }

  if (i == iterlim) {
    warning(paste0(
      "The iteration limit (iterlim = ", iterlim, ") was reached",
      " before the relative tolerance requirement (reltol = ",
      reltol, ")."
    ))
  }
  print(i)
  x1
}


x <- rgamma(100, 1, 1)

n <- length(x)
mean_hat <- sum(x) / n
lx_bar <- sum(log(x)) / n
s <- log(mean_hat) - lx_bar

x0 <- 1 / (12 * s) * (3 - s + sqrt((s - 3)^2 + 24 * s)) * 0.9989 + 0.0102
newton_raphson_1d(\(x) fs(x)[1] / fs(x)[2], x0)
x0 <- 1 / (12 * s) * (3 - s + sqrt((s - 3)^2 + 24 * s))
newton_raphson_1d(\(x) fs(x)[1] / fs(x)[2], x0)

fs <- \(shape0) {
  -c((log(shape0) - digamma(shape0) - s),
    (1 / shape0 - trigamma(shape0)),
    -1 / shape0^2 - psigamma(shape0, deriv =2))
}

x0 <- 1 / (12 * s) * (3 - s + sqrt((s - 3)^2 + 24 * s)) * 0.9989 + 0.0102
#x0 <- 1 / (12 * s) * (3 - s + sqrt((s - 3)^2 + 24 * s))
halley(fs, x0)
newton_raphson_1d(\(x) fs(x)[1] / fs(x)[2], x0)

x0s <- seq(0.5, 2, by = 0.01)
plot(x0s, sapply(x0s, \(x) fs(x)[1]), type = "l")
lines(x0s, sapply(x0s, \(x) fs(x)[1]/sqrt(fs(x)[2])), type = "l")


results <- t(replicate(10000, {
  n <- sample(5:10000, 1)
  x <- rgamma(n, rexp(1)+10, rexp(1)+10)
n <- length(x)
mean_hat <- sum(x) / n
lx_bar <- sum(log(x)) / n
s <- log(mean_hat) - lx_bar
x0 <- 1 / (12 * s) * (3 - s + sqrt((s - 3)^2 + 24 * s))
x1 <- unname(mlgamma(x)[1])
c(x0, x1, n)}))

