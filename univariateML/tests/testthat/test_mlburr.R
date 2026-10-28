context("mlburr errors")

test_that("mlburr errors when all observations >= 1", {
  expect_error(
    mlburr(rep(2, 5)),
    "The Burr maximum likelihood estimator does not exist when all observations >=1\\. Use `mlpareto` to fit a Pareto distribution\\."
  )
})

test_that("mostly Pareto inputs fail cleanly or return finite fits", {
  set.seed(1)
  x1 <- c(1 + rexp(199), 0.99)
  set.seed(2)
  x2 <- c(1 + rexp(49), 0.5)
  for (x in list(x1, x2)) {
    result <- tryCatch(mlburr(x), error = identity)
    if (inherits(result, "error")) {
      expect_match(conditionMessage(result), "valid initial c|Burr MLE failed")
    } else {
      expect_true(all(is.finite(result)))
      expect_true(is.finite(attr(result, "logLik")))
    }
  }
})
