context("mlgompertz errors")

test_that("mlgompertz errors on overdispersed data", {
  expect_error(
    mlgompertz(c(0.1, 0.2, 5)),
    "Use `mlexp`"
  )
})

test_that("gompertz fitting respects extreme scale changes", {
  set.seed(1)
  x <- simulate_default("mlgompertz", 50)
  f <- mlgompertz(x)
  for (s in c(1e-300,1e300)) {
    g <- mlgompertz(x*s)
    expect_equal(attr(g,"logLik")+length(x)*log(s),attr(f,"logLik"),tolerance=1e-6)
  }
})

test_that("Gompertz moment equality selects the exponential face", {
  expect_error(mlgompertz(c(1,1,4,12)), "Use `mlexp`")
})

test_that("constant positive Gompertz data have an unbounded likelihood", {
  expect_error(mlgompertz(rep(1,5)),"does not exist")
})
