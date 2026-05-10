context("mlburr errors")

test_that("mlburr errors when all observations >= 1", {
  expect_error(
    mlburr(rep(2, 5)),
    "The Burr maximum likelihood estimator does not exist when all observations >=1\\. Use `mlpareto` to fit a Pareto distribution\\."
  )
})
