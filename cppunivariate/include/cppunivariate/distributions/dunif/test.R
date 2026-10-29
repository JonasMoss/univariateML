test_that("dunif agrees with its reference likelihood", {
  x <- c(0, 1, 2, 4)
  fit <- mldunif(x)
  expect_equal(unname(coef(fit)), c(min(x), max(x)))
  expect_equal(attr(fit, "logLik"), sum(extraDistr::ddunif(x, fit[[1]], fit[[2]], log = TRUE)))
})

test_that("discrete uniform width cannot overflow an int", {
  x <- c(-.Machine$integer.max, .Machine$integer.max)
  expect_equal(attr(mldunif(x), "logLik"), -2 * log(2*.Machine$integer.max + 1))
})
