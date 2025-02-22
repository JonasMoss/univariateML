context("mlbeta")

## Data generation.
set.seed(313)
small_data <- runif(100)
tiny_data <- rbeta(10, 2, 7)

## Checks if the ML is correct.
mle1 <- nlm(\(p) {
  -mean(dbeta(small_data, p[1], p[2], log = TRUE))
}, p = c(1, 1))$estimate

mle2 <- nlm(\(p) {
  -mean(dbeta(tiny_data, p[1], p[2], log = TRUE))
}, p = c(1, 1))$estimate

expect_equal(mle1, as.numeric(mlbeta(small_data)), tolerance = 1e-5)
expect_equal(mle2, as.numeric(mlbeta(tiny_data)), tolerance = 1e-5)

## Checks the control parameters.
expect_equal(coef(mlbeta(tiny_data, type = "gradient")),
  coef(mlbeta(tiny_data)),
  tolerance = 1e-5
)
expect_equal(coef(mlbeta(small_data, type = "gradient")),
  coef(mlbeta(small_data, type = "hessian")),
  tolerance = 1e-5
)

## Finds errors with na and data out of bounds.
expect_error(mlbeta(c(0, tiny_data)))
expect_error(mlbeta(c(tiny_data, 1)))
expect_error(mlbeta(c(tiny_data, NA)))
