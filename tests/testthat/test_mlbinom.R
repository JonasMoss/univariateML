context("mlbinom")

## Expect error when fed with overdispersed data
expect_error(mlbinom(seq(20)))

## Size argument works
expect_equal(unname(mlbinom(seq(20), size = 20))[1], 20)

## Error when size > max
expect_error(mlbinom(seq(21), size = 20))
