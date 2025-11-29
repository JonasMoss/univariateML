dlogburr <- function(x, mu = 0, sigma = 1, k = 1, log = FALSE) {
  z <- (x - mu) / sigma
  logdens <- log(k) - log(sigma) + z - (k + 1) * log1p(exp(z))
  if (log) return(logdens)
  exp(logdens)
}


sigma <- 1/200
k <- sigma
xx <- seq(-1,10, by = 0.01)
plot(xx, dlogburr(xx, mu = 0, sigma = sigma, k = k), type = "l", lwd = 2)
lines(xx, dexp(xx), col = "red")