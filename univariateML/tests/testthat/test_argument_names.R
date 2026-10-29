# Wrapper arguments must match adapter arguments exactly: partial matching
# turned `mlweibull(x, shape = 2)` into a start value (`shape0`) and
# estimated the shape the caller meant to hold (P0 finding, 2026-10-29).
test_that("unsupported holds and unknown arguments stop instead of matching partially", {
  set.seed(1)
  x <- rweibull(50, 2, 3)
  expect_error(mlweibull(x, shape = 2), "`mlweibull` cannot hold `shape`")
  expect_error(mlweibull(x, shap = 2), "`mlweibull` has no argument `shap`")
  expect_error(mlkumar(rbeta(50, 2, 3), a = 2), "`mlkumar` cannot hold `a`")
  expect_error(mlnorm(rnorm(10), foo = 1), "has no argument `foo`")
  expect_s3_class(mlweibull(x, shape0 = 2), "univariateML")
  expect_equal(unname(mlbetapr(x, scale = 1)["scale"]), 1)
})
