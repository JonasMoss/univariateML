context("mlzip")

## Warning when ml is not unique
expect_warning(mlzip(c(0, 0, 0)))

set.seed(1)
expect_equal(unname(mlzip(rpois(10, 1))[2]), 0)

test_that("ordinary zero-inflated samples attain the oracle likelihood", {
  samples <- list(
    c(0, 1, 2, 1, 1, 2, 0, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 3),
    c(0, 0, 4)
  )
  lambda <- c(1.423265, 3.920695)
  pi <- c(0.4730435, 0.6599249)
  oracle_ll <- c(-23.0097617, -3.52318909)
  for (i in seq_along(samples)) {
    fit <- mlzip(samples[[i]])
    expect_lt(abs(unname(fit["lambda"]) / lambda[i] - 1), 1e-4)
    expect_lt(abs(unname(fit["pi"]) / pi[i] - 1), 1e-4)
    expect_gte(attr(fit, "logLik"), oracle_ll[i] - 1e-6)
    expect_true(all(is.finite(fit)))
    expect_equal(attr(fit, "logLik"),
                 sum(extraDistr::dzip(samples[[i]], lambda = fit["lambda"],
                                      pi = fit["pi"], log = TRUE)),
                 tolerance = 1e-10)
  }
})

test_that("ZIP boundary fits are finite and represent their distributions", {
  expect_warning(zero <- mlzip(c(0, 0, 0)), "all observations are zero")
  expect_equal(as.numeric(zero), c(1, 1))
  expect_equal(attr(zero, "logLik"), 0)
  for (x in list(c(1, 2, 3), c(0, 1, 1, 2))) {
    fit <- mlzip(x)
    expect_equal(unname(fit["pi"]), 0)
    expect_equal(unname(fit["lambda"]), mean(x))
    expect_equal(attr(fit, "logLik"), sum(dpois(x, mean(x), log = TRUE)))
  }
  expect_error(mlzip_(integer()), "requires nonempty nonnegative count data")
  expect_error(mlzip_(c(-1L, 0L)), "requires nonempty nonnegative count data")
})

test_that("ZIP Poisson face has an accurate likelihood at the count ceiling", {
  x<-c(.Machine$integer.max-1,.Machine$integer.max)
  f<-mlzip(x)
  expect_equal(f[["pi"]],0)
  expect_equal(attr(f,"logLik"),sum(dpois(x,mean(x),log=TRUE)),tolerance=1e-10)
})
