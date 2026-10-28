test_that("dunif agrees with its reference likelihood", {
  x <- c(0, 1, 2, 4)
  fit <- mldunif(x)
  expect_equal(unname(coef(fit)), c(min(x), max(x)))
  expect_equal(attr(fit, "logLik"), sum(extraDistr::ddunif(x, fit[[1]], fit[[2]], log = TRUE)))
})
