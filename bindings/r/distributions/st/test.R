test_that("st evaluates the symmetric Student-t member", {
  x <- c(-2, 0, 1, 4)
  model <- mlst(xi=0, omega=1, alpha=0, nu=4)
  expect_equal(dml(x, model), dt(x, 4), tolerance=1e-10)
})
test_that("st diagnoses scale collapse", {
  expect_error(mlst(c(0,0,0,1,2)), "unbounded")
})

test_that("st flags a fit on the df restriction", {
  x <- c(-1000, -10, -1, 0, 1, 10, 1000)
  expect_warning(f <- mlst(x, alpha = 0), class = "univariateML_restricted")
  expect_identical(attr(f, "restricted"), c(nu = "lower"))
})
