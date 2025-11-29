f_inv <- function(cv, location = -0.3136848, scale = 0.6394021) { # cv = (m1)^2 / m2
  if (cv == 0) {
    return(c(k = 1, iter = 0))
  }

  neuler <- digamma(1)
  basel <- trigamma(1)
  tetra <- psigamma(1, deriv = 2)

  if (cv > 1 - 1e-12) {
    tmp <- (1 - cv) / (cv * basel)
    if(tmp < 0 ) return(c(1e-15, iter = 0))
    return(c(k = max(1e-15, sqrt(tmp)), iter = 0))
  }

  threshold_cv_low <- 0.1 # Used in startin value comp.
  threshold_cv_high <- 0.95

  f_over_df <- function(x) {
    k <- plogis(x)

    di <- digamma(k)
    tri <- trigamma(k)
    tetra <- psigamma(k, deriv = 2)
    psi_diff <- di - neuler

    psi_diff2 <- psi_diff^2
    bpt <- basel + tri

    f <- psi_diff2 / bpt - cv
    df <- (2 * psi_diff * tri * bpt - psi_diff2 * tetra) / bpt^2 * dlogis(x)
    f / df
  }

  x0 <- qlogis(if (cv < threshold_cv_low) {
      1 - sqrt(2 * cv / basel)
    } else if (cv > threshold_cv_high) {
      sqrt((1 - cv) / (cv * basel))
    } else {
      y_approx <- 1 - cv
      x_approx <- qlogis(y_approx, location = location, scale = scale)
      plogis(x_approx)
    })

  reltol <- .Machine$double.eps^0.7
  iterlim <- 10

  for (i in seq(iterlim)) {
    x <- x0 - f_over_df(x0)
    if (abs(x0 - x) < abs(x0) * reltol + reltol * 1e-3) break
    x0 <- x
  }
  c(k = plogis(x0), iter = i)
}

f <- \(k) (digamma(k) - digamma(1))^2 / (trigamma(1) + trigamma(k))

x <- exp(seq(log(1e-4), log(1-1e-4), length.out = 1000))
y <- f(x)


f_inv(f(x[5]))

x <- exp(seq(log(1e-6), log(1-1e-6), length.out = 1000))
y <- f(x)
results <- as.data.frame(cbind(x = x, cv = y, t(sapply(y, f_inv))))
sum((results$x - results$k)^2)
mean(results$iter)
table(results$iter)

plot(results$x, abs(results$x - results$k))

#plot(results$x, abs(results$x - results$k) / results$x)

location <- -0.5495872
scale <- 0.6103216

results <- as.data.frame(cbind(x = x, cv = y, t(sapply(y, f_inv, location, scale))))
sum((results$x - results$k)^2)
mean(results$iter)
table(results$iter)



microbenchmark::microbenchmark(f_inv(0.51))
