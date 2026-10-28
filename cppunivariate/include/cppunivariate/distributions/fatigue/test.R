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
