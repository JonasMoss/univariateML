context("mlnbinom")

## Error when data is Poisson.
expect_error(
  mlnbinom(c(1, 1, 1, 1)),
  "The maximum likelihood estimator does not exist for underdispersed data, but converges to a Poisson. Use `mlpois` instead\\."
)

## Non-unique maximum likelihood.
expect_warning(mlnbinom(c(0, 0, 0)))


## Size argument works
expect_equal(unname(mlnbinom(seq(20), size = 20))[1], 20)

test_that("fractional fixed size is preserved", {
  x <- c(0, 1, 2, 5)
  fit <- mlnbinom(x, size = 2.5)
  expect_equal(fit[["size"]], 2.5)
  expect_equal(attr(fit, "logLik"), sum(dnbinom(x, size = 2.5, prob = fit[["prob"]], log = TRUE)))
})

test_that("variance equal to the mean escapes to the Poisson", {
  # Aragon, Eberly and Eberly (1992): a finite MLE exists iff the
  # population-form variance exceeds the mean. Each sample has equality.
  for (x in list(c(0, 2), c(0, 1, 2, 3, 4), c(1, 1, 2, 4, 5, 5), rep(0:4, 1000))) {
    expect_equal(mean(x^2) - mean(x)^2, mean(x))
    expect_error(mlnbinom(x), "Use `mlpois`")
  }
})

test_that("overdispersed data where Newton leaves the domain still fit", {
  x <- c(rep(0, 9), 1000)
  fit <- mlnbinom(x)
  profile <- function(size) sum(dnbinom(x, size = size, mu = mean(x), log = TRUE))
  opt <- optimize(function(t) profile(exp(t)), c(-20, 5), maximum = TRUE, tol = 1e-12)
  expect_equal(fit[["size"]], exp(opt$maximum), tolerance = 1e-6)
  expect_equal(attr(fit, "logLik"), opt$objective, tolerance = 1e-10)
  # Variance barely above the mean: the MLE is finite and large.
  y <- c(rep(0:4, 999), 0, 4)
  expect_gt(mean(y^2) - mean(y)^2, mean(y))
  fit <- mlnbinom(y)
  expect_gt(fit[["size"]], 1e3)
  expect_equal(attr(fit, "logLik"), sum(dnbinom(y, fit[["size"]], fit[["prob"]], log = TRUE)))
})

test_that("a held size must be positive and finite", {
  for (size in list(0, -1, Inf, NaN, NA_real_)) {
    expect_error(mlnbinom(c(1, 2, 5), size = size), "positive finite")
  }
})

test_that("unrepresentable held-size probability is diagnosed", {
  expect_error(mlnbinom(c(1,2,5),size=1e300), "rounds to one")
})
