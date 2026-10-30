test_that("confint uses the requested number of bootstrap draws", {
  fit <- mlnorm(c(-2, -1, 0, 1, 3))

  set.seed(103)
  expected <- bootstrapml(fit, reps = 20)
  expected_rng <- .Random.seed
  set.seed(103)
  actual <- confint(fit, Nreps = 20)
  expect_true(identical(.Random.seed, expected_rng))
  expect_equal(actual, expected)

  # Inspect the draws directly, without relying on percentile differences.
  draws <- confint(fit, Nreps = 20, reducer = function(x, ...) x)
  expect_equal(dim(draws), c(2L, 20L))
})

test_that("the named reps alias takes precedence over Nreps", {
  fit <- mlnorm(c(-2, -1, 0, 1, 3))
  set.seed(103)
  expected <- bootstrapml(fit, reps = 7)
  set.seed(103)
  expect_equal(confint(fit, reps = 7), expected)
  set.seed(103)
  expect_equal(confint(fit, Nreps = 20, reps = 7), expected)
})
