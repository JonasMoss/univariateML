test_that("inverse Burr power limits name mlpower", {
  set.seed(1)
  x1 <- runif(200)^(1/2)
  set.seed(1)
  x2 <- runif(50)
  for (x in list(x1, x2, rep(1, 5), c(0.5, 1))) {
    expect_error(mlinvburr(x), "Use `mlpower`")
  }
})

test_that("inverse Burr with points above one recovers a finite MLE", {
  set.seed(3)
  x <- c(runif(199)^(1/2), 1.02)
  fit <- mlinvburr(x)
  expect_true(all(abs(as.numeric(fit) / c(0.0224871, 91.5277) - 1) < 1e-3))
  expect_gte(attr(fit, "logLik"), 38.4194 - 1e-6)
})
