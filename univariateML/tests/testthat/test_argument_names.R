# Wrapper arguments must match adapter arguments exactly: partial matching
# turned `mlweibull(x, shape = 2)` into a start value (`shape0`) and
# estimated the shape the caller meant to hold (P0 finding, 2026-10-29).
test_that("held coefficients are matched exactly, never to start values", {
  set.seed(1)
  x <- rweibull(50, 2, 3)
  f <- mlweibull(x, shape = 2)
  expect_identical(unname(f["shape"]), 2)
  expect_true("shape" %in% attr(f, "fixed"))
  expect_error(mlweibull(x, shap = 2), "`mlweibull` has no argument `shap`")
  expect_error(mlnorm(rnorm(10), foo = 1), "has no argument `foo`")
  expect_s3_class(mlweibull(x, shape0 = 2), "univariateML")
})

test_that("a coefficient the adapter does not accept is named as an unsupported hold", {
  adapter <- function(x, reltol = 1e-8) NULL
  expect_error(univariateML:::ml_check_args(adapter, list(mean = 1), "mlnorm"),
               "`mlnorm` cannot hold `mean` \\(not supported yet\\)")
  expect_null(univariateML:::ml_check_args(adapter, list(reltol = 1e-6), "mlnorm"))
})
