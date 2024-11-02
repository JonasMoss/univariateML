context("mlnbinom")

## Error when data is Poisson.
expect_error(mlnbinom(c(1, 1, 1, 1)))

## Non-unique maximum likelihood.
expect_warning(mlnbinom(c(0, 0, 0)))


## Size argument works
expect_equal(unname(mlnbinom(seq(20), size = 20))[1], 20)
