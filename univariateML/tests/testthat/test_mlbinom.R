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
