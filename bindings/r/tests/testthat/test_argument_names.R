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
  expect_error(mlweibull(x, shape0 = 2), "`mlweibull` has no argument `shape0`")
})

test_that("a coefficient the adapter does not accept is named as an unsupported hold", {
  adapter <- function(x, tol = 1e-8) NULL
  expect_error(univariateML:::ml_check_args(adapter, list(mean = 1), "mlnorm"),
               "`mlnorm` cannot hold `mean` \\(not supported yet\\)")
  expect_null(univariateML:::ml_check_args(adapter, list(tol = 1e-6), "mlnorm"))
})

test_that("ml_check_args help title has no terminal period", {
  rd <- umvl_rd("ml_check_args.Rd")
  title <- rd[[which(vapply(rd, function(x) identical(attr(x, "Rd_tag"), "\\title"), logical(1)))]]
  expect_false(grepl("\\.$", trimws(as.character(title[[1]]))))
})

test_that("modifiers are exact-name formals on eligible fitters", {
  expect_identical(names(formals(args(mlpois))), c("x", "na.rm", "...", "zero", "truncate"))
  expect_identical(formals(mlpois)$zero, "none")
  expect_null(formals(mlpois)$truncate)
  expect_identical(names(formals(mlnorm)), c("x", "na.rm", "...", "truncate"))
  expect_true("zero" %in% names(formals(mllgser)))
  expect_error(mlnorm(1:4, zero = "none"), "has no argument `zero`", fixed = TRUE)
  expect_error(mlnorm(1:4, zero = "truncated", truncate = c(0, 5)),
               "has no argument `zero`", fixed = TRUE)
  expect_error(mlpois(1:4, zer = "truncated"), "has no argument `zer`", fixed = TRUE)
  expect_error(mlpois(1:4, zero = NULL), "`zero` must be one of", fixed = TRUE)
  expect_error(mlpois(1:4, FALSE, 2, zero = "modified"),
               "Modifier holds and controls must be named", fixed = TRUE)
  expect_equal(coef(mlpois(c(1:4, NA), TRUE)), coef(mlpois(1:4)))
})
