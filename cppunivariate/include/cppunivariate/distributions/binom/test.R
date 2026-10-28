context("mlbinom")

## Expect error when fed with overdispersed data
expect_error(
  mlbinom(seq(20)),
  "The maximum likelihood estimator does not exist. Use `mlpois` to fit a Poisson or supply a `size` argument\\."
)

## Size argument works
expect_equal(unname(mlbinom(seq(20), size = 20))[1], 20)

## Error when size > max
expect_error(mlbinom(seq(21), size = 20))

## Underdispersed / invalid inputs
expect_error(
  mlbinom(rep(1, 5), size = 0),
  "`size` is smaller than `max\\(x\\)`\\."
)

test_that("binomial probability boundaries have log likelihood zero", {
  for (fit in list(mlbinom(c(5L, 5L, 5L)),
                   mlbinom(c(5L, 5L, 5L), size = 5))) {
    expect_equal(as.numeric(fit), c(5, 1))
    expect_equal(attr(fit, "logLik"), 0)
    expect_equal(attr(fit, "logLik"), sum(dbinom(rep(5, 3), 5, 1, log = TRUE)))
  }
  for (size in c(0, 5)) {
    fit <- mlbinom(c(0L, 0L, 0L), size = size)
    expect_equal(as.numeric(fit), c(size, 0))
    expect_equal(attr(fit, "logLik"), 0)
  }
  fit <- mlbinom(c(0L, 0L, 0L))
  expect_equal(as.numeric(fit), c(0, 0))
  expect_equal(attr(fit, "logLik"), 0)
})

test_that("other discrete zero-data fits have the point-mass likelihood", {
  expect_equal(attr(mlpois(c(0, 0, 0)), "logLik"), 0)
  expect_equal(attr(mlgeom(c(0, 0, 0)), "logLik"), 0)
  expect_warning(fit <- mlnbinom(c(0, 0, 0)), "All observations are 0")
  expect_equal(as.numeric(fit), c(1, 1))
  expect_equal(attr(fit, "logLik"), 0)
  expect_warning(fixed <- mlnbinom(c(0, 0, 0), size = 3), "All observations are 0")
  expect_equal(attr(fixed, "logLik"), 0)
})
