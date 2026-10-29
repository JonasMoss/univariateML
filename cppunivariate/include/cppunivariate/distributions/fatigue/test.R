test_that("mlfatigue agrees with its reference density", {
  set.seed(313)
  x <- simulate_default("mlfatigue", 200)
  fit <- mlfatigue(x)
  density <- getFromNamespace("dfatigue", "extraDistr")
  oracle <- optim(log(c(2, 1)), function(p) {
    -sum(extraDistr::dfatigue(x, alpha = exp(p[1]), beta = exp(p[2]), mu = 0, log = TRUE))
  }, method = "BFGS", control = list(reltol = 1e-12, maxit = 1000))
  expect_equal(unname(coef(fit)[1:2]), exp(oracle$par), tolerance = 1e-4)
  expect_equal(unname(coef(fit)[["mu"]]), 0)
  expect_equal(attr(fit, "logLik"), sum(do.call(density, c(list(x = x, log = TRUE), as.list(coef(fit))))), tolerance = 1e-7)
})

test_that("the three-parameter likelihood with free mu is unbounded (why mu is fixed)", {
  # beta = eps, alpha^2 = s / eps, mu just below min(x): logL grows like -2 log(eps).
  set.seed(1)
  x <- extraDistr::rfatigue(50, alpha = 0.5, beta = 2, mu = 1)
  s <- mean(x - min(x))
  path <- sapply(10^-(4:7), function(eps) {
    sum(log(extraDistr::dfatigue(x, alpha = sqrt(s / eps), beta = eps, mu = min(x) - 10 * eps^2 / s)))
  })
  expect_true(all(diff(path) > 0))
  expect_equal(diff(path), rep(2 * log(10), 3), tolerance = 0.01)
})

test_that("fatigue handles ties and extreme scales without invalid estimates", {
  expect_error(mlfatigue(rep(1,5)),"does not exist")
  f <- mlfatigue(1+(0:4)*.Machine$double.eps)
  expect_gt(f[["alpha"]],0)
  expect_true(is.finite(attr(f,"logLik")))
  x <- c(.4,.8,1,2,4); base <- mlfatigue(x)
  for(s in c(1e-300,1e300)) {
    g <- mlfatigue(x*s)
    expect_equal(attr(g,"logLik")+length(x)*log(s),attr(base,"logLik"),tolerance=1e-6)
  }
})
