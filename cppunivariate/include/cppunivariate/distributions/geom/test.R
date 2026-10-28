test_that("geom agrees with its reference likelihood", {
  x <- c(0, 1, 2, 4)
  fit <- mlgeom(x)
  expect_equal(unname(coef(fit)), 1/(mean(x)+1))
  expect_equal(attr(fit, "logLik"), sum(dgeom(x, fit[[1]], log = TRUE)))
})
