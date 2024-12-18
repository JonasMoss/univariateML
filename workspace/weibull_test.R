#set.seed(1)
x <- rweibull(100000, 6, 30)
microbenchmark::microbenchmark(mlweibull(x), mlgamma(x))

mlweibull(x)
mlgumbel(-log(x))

-mlgumbel(-log(x))[1]
log(mlweibull(x)[2])

mlgumbel(-log(x))[2]
1/mlweibull(x)[1]


exp(-mlgumbel(-log(x))[1])
mlweibull(x)[2]

1/mlgumbel(-log(x))[2]
mlweibull(x)[1]



mlweibull_(x)
#a <- mlweibull1_(x)
#b <- mlweibull2_(x)
#x <- egypt$age

y <- rbeta(10000, 2, 7)
microbenchmark::microbenchmark(mlweibull_(x), mlgumbel_(-log(x)),mlbeta(y), mlgamma(x))
#microbenchmark::microbenchmark(mlweibull_(x), mlweibull1_(x), mlweibull2_(x), times = 100)

microbenchmark::microbenchmark(mlweibull1_(x))


mlweibull1_ <- \(x, ...) {
  n <- length(x)
  log_x <- log(x)
  l_hat <- mean(log_x)

  iterate <- \(shape0) {
    #lshape0 <- log(shape0)
    #x_shape0 <- exp(log_x*lshape0)
    x_shape0 <- x^shape0

    psi0 <- mean(x_shape0)
    psi1 <- mean(x_shape0 * log_x)
    psi2 <- mean(x_shape0 * log_x^2)

    lpsi1 <- psi1 / psi0
    lpsi2 <- psi2 / psi0 - lpsi1^2

    f1 <- 1 / shape0 + l_hat - lpsi1
    f2 <- -1 / shape0^2 - lpsi2

    f1 / f2
  }
  newton_raphson_1d(iterate, 1.2825 / sd(log_x))
}

mlweibull2_ <- \(x, ...) {
  n <- length(x)
  log_x <- log(x)
  log_xsq <- log_x^2
  log_xcube <- log_x^3
  l_hat <- mean(log_x)

  iterate <- \(shape0) {
    x_shape0 <- x^shape0

    psi0 <- mean(x_shape0)
    psi1 <- mean(x_shape0 * log_x)
    psi2 <- mean(x_shape0 * log_xsq)

    lpsi1 <- psi1 / psi0
    lpsi2 <- psi2 / psi0 - lpsi1^2

    f1 <- 1 / shape0 + l_hat - lpsi1
    f2 <- -1 / shape0^2 - lpsi2

    h <- f1 / f2

    \(step) {
      if (step) {
        psi3 <- mean(x_shape0 * log_xcube)
        lpsi3 <- psi3 / psi0 - 3*lpsi2*lpsi1 - lpsi1^3
        f3 <- 2/shape0^3 - lpsi3
        #return(h / (1 - 0.5 * f3/f2 *h))
        return(f2/f3 + sqrt(f2^2 - 2*f1*f3) / f3)
      }
      h
    }
  }
  iter(iterate, 1.2825 / sd(log_x))
}

iter <- \(iterate, param0) {
  reltol <- .Machine$double.eps^0.25
  iterlim <- 100

  for (i in seq(iterlim)) {
    f <- iterate(param0)
    param <- param0 - f(FALSE)
    if (is.nan(param)) stop("NaN in Newton--Raphson iteration.")
    if (abs((param0 - param) / param0) < reltol) break
    param <- param0 - f(TRUE)
    if (is.nan(param)) stop("NaN in Newton--Raphson iteration.")
    if (abs((param0 - param) / param0) < reltol) break
    param0 <- param
  }

  if (i == iterlim) {
    warning(paste0(
      "The iteration limit (iterlim = ", iterlim, ") was reached",
      " before the relative tolerance requirement (reltol = ",
      reltol, ")."
    ))
  }

  #attr(param, "iter") <- i
  #print(i)
  param
}



mlweibull_(x)
mlweibull1_(x)
mlweibull2_(x)

microbenchmark::microbenchmark(mlweibull1_(x), mlweibull2_(x))


## Concept.

### First time: Calculate Newton-Raphson.
### Second time: Do secant method. If convergence. Stop. If not, another Newton-Raphson.

### In general, do kth order Householder first time.
### Second time: Do secant method (or, perhaps, a method using the previous derivative!) to test convergence.
### If not,




f_over_df <- \(shape0) {
  x_shape0 <- x^shape0
  shape0_lsum <- sum(x_shape0 * log_x)
  shape0_lsum_sqr <- sum(x_shape0 * log_x^2)
  shape0_sum <- sum(x_shape0)
  a <- shape0_lsum / shape0_sum
  b <- shape0_lsum_sqr / shape0_sum
  f <- 1 / shape0 + l_hat - a
  df <- -1 / shape0^2 + - b + a^2
  f / df
}

dots <- list(...)
shape0 <- if (!is.null(dots$shape0)) dots$shape0 else 1.2825 / sd(log_x)

shape <- newton_raphson_1d(f_over_df, shape0, ...)

shape_mean <- mean(x^shape)
scale <- shape_mean^(1 / shape)

estimates <- c(shape, scale)
logLik <- n * (log(shape) - log(scale) +
                 (shape - 1) * (l_hat - log(scale)) - scale^-shape * shape_mean)
list(estimates = estimates, logLik = logLik)
}
