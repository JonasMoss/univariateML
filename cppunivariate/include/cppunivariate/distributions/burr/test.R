context("mlburr errors")

test_that("mlburr errors when all observations >= 1", {
  expect_error(
    mlburr(rep(2, 5)),
    "The Burr maximum likelihood estimator does not exist when all observations >=1\\. Use `mlpareto` to fit a Pareto distribution\\."
  )
})

test_that("mostly Pareto inputs recover finite MLEs", {
  set.seed(1)
  x1 <- c(1 + rexp(199), 0.99)
  set.seed(2)
  x2 <- c(1 + rexp(49), 0.5)
  oracles <- list(c(0.0166659, 97.413, -227.81258048920921),
                  c(0.194673, 8.00904, -67.74383957580861))
  for (i in seq_along(oracles)) {
    fit <- mlburr(list(x1, x2)[[i]])
    oracle <- oracles[[i]]
    expect_true(all(abs(as.numeric(fit) / oracle[1:2] - 1) < 1e-3))
    expect_gte(attr(fit, "logLik"), oracle[3] - 1e-6)
  }
})

test_that("explicit large c starts remain supported", {
  set.seed(1)
  x <- c(1 + rexp(199), 0.99)
  fit <- mlburr(x, init = 1000)
  expect_equal(as.numeric(fit), as.numeric(mlburr(x)), tolerance = 1e-9)
  expect_equal(attr(fit, "logLik"), attr(mlburr(x), "logLik"), tolerance = 1e-9)
})
