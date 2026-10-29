context("mlhuber")
set.seed(782)
x <- extraDistr::rhuber(120, mu = 2, sigma = 3, epsilon = 1.345)
for (e in c(.5, 1.345, 3, 10)) {
  for (fm in c(FALSE, TRUE)) for (fs in c(FALSE, TRUE)) {
    args <- list(x = x, epsilon = e)
    if (fm) args$mu <- 1
    if (fs) args$sigma <- 2
    fit <- do.call(mlhuber, args)
    fixed <- c("epsilon", if (fm) "mu", if (fs) "sigma")
    expect_setequal(attr(fit, "fixed"), fixed)
    expect_equal(attr(logLik(fit), "df"), 2L - fm - fs)
    if (fm) expect_equal(unname(fit["mu"]), 1)
    if (fs) expect_equal(unname(fit["sigma"]), 2)
    obj <- function(p) {
      m <- if (fm) 1 else p[1]
      s <- if (fs) 2 else exp(p[length(p)])
      -sum(extraDistr::dhuber(x, m, s, e, log = TRUE))
    }
    if (!(fm && fs)) {
      vals <- sapply(c(-2, 0, 2), function(shift) {
        start <- c(if (!fm) median(x)+shift, if (!fs) log(sd(x))+shift/3)
        optim(start, obj, method = "BFGS", control = list(reltol = 1e-12))$value
      })
      expect_equal(-as.numeric(logLik(fit)), min(vals), tolerance = 1e-7)
    }
    expect_equal(as.numeric(logLik(fit)), sum(extraDistr::dhuber(x, fit[1], fit[2], e, log=TRUE)), tolerance=1e-10)
  }
}
# Direct clipped-score Huber location iteration at a held scale.
m <- median(x)
for (i in 1:1000) {
  z <- (x-m)/2
  w <- pmin(1, 1.345/abs(z))
  next_m <- sum(w*x)/sum(w)
  if (abs(next_m-m)<1e-12) break
  m <- next_m
}
expect_equal(unname(mlhuber(x, sigma=2)[1]), m, tolerance=1e-9)
expect_equal(unname(mlhuber(c(-10,10), sigma=1)[1]), 0)
expect_error(mlhuber(rep(1,5)), "unbounded")
expect_error(mlhuber(rep(1,5), mu=1), "unbounded")
expect_true(is.finite(as.numeric(logLik(mlhuber(rep(1,5), mu=0)))))
expect_true(is.finite(as.numeric(logLik(mlhuber(rep(1,5), sigma=2)))))
for (bad in c(0,-1,Inf,NaN)) {
  expect_error(mlhuber(x, sigma=bad), "sigma")
  expect_error(mlhuber(x, epsilon=bad), "epsilon")
}
expect_error(mlhuber(x, mu=Inf), "mu")
expect_equal(as.numeric(mlhuber(x, epsilon=10)[1:2]), as.numeric(mlnorm(x)), tolerance=1e-7)
expect_error(mlhuber(c(1, Inf)), "support")
expect_error(mlhuber(x, gradtol=0), "gradtol")
expect_error(mlhuber(x, iterlim=0), "iterlim")
expect_error(mlhuber(x, iterlim=1, gradtol=1e-15), "converge")
# Tied, nonconstant samples remain bounded and fit every free subset.
for (args in list(list(), list(mu=0), list(sigma=1), list(mu=0, sigma=1))) {
  fit <- do.call(mlhuber, c(list(x=c(0,0,0,1,1)), args))
  expect_true(is.finite(as.numeric(logLik(fit))))
}
