test_that("inverse Burr power limits name mlpower", {
  set.seed(1)
  x1 <- runif(200)^(1/2)
  set.seed(1)
  x2 <- runif(50)
  for (x in list(x1, x2, rep(1, 5), c(0.5, 1))) {
    expect_error(mlinvburr(x), "Use `mlpower`")
  }
})

test_that("inverse Burr with points above one fails cleanly or fits", {
  set.seed(1)
  x <- 1 / c(1 + rexp(199), 0.99)
  result <- tryCatch(mlinvburr(x), error = identity)
  if (inherits(result, "error")) {
    expect_match(conditionMessage(result), "Inverse Burr MLE failed")
  } else {
    expect_true(all(is.finite(result)))
    expect_true(is.finite(attr(result, "logLik")))
  }
})
