test_that("Poisson agrees with its reference likelihood", {
  x <- c(0, 1, 2, 4)
  fit <- mlpois(x)
  expect_equal(unname(coef(fit)), mean(x))
  expect_equal(attr(fit, "logLik"), sum(dpois(x, mean(x), log = TRUE)))
})
