context("mlbetapr")

## Data generation.
set.seed(313)
tiny_data <- extraDistr::rbetapr(10, 0.5, 10)
small_data <- extraDistr::rbetapr(100, 3, 8)
medium_data <- extraDistr::rbetapr(1000, 11, 0.3)

## Checks if the ML is correct.
mle_tiny <- suppressWarnings(nlm(\(p) {
  -sum(extraDistr::dbetapr(tiny_data, p[1], p[2], log = TRUE))
}, p = c(0.5, 10)))

mle_small <- nlm(\(p) {
  -sum(extraDistr::dbetapr(small_data, p[1], p[2], log = TRUE))
}, p = c(3, 8))

mle_medium <- suppressWarnings(nlm(\(p) {
  -sum(extraDistr::dbetapr(medium_data, p[1], p[2], log = TRUE))
}, p = c(11, 0.3)))

expect_equal(mle_tiny$estimate, as.numeric(mlbetapr(tiny_data)),
  tolerance = 1e-5
)
expect_equal(mle_small$estimate, as.numeric(mlbetapr(small_data)),
  tolerance = 1e-5
)
expect_equal(mle_medium$estimate, as.numeric(mlbetapr(medium_data)),
  tolerance = 1e-5
)

## Checks logLiks.
expect_equal(-mle_tiny$minimum, attr(mlbetapr(tiny_data), "logLik"),
  tolerance = 1e-5
)
expect_equal(-mle_small$minimum, attr(mlbetapr(small_data), "logLik"),
  tolerance = 1e-5
)
expect_equal(-mle_medium$minimum, attr(mlbetapr(medium_data), "logLik"),
  tolerance = 1e-5
)

## Checks the control parameters.
expect_equal(coef(mlbetapr(tiny_data, type = "gradient")),
  coef(mlbetapr(tiny_data)),
  tolerance = 1e-4
)
expect_equal(coef(mlbetapr(small_data, type = "gradient")),
  coef(mlbetapr(small_data, type = "hessian")),
  tolerance = 1e-4
)

## Finds errors with na and data out of bounds.
expect_error(mlbetapr(c(0, tiny_data)))
expect_error(mlbetapr(c(tiny_data, NA)))

## Checks that na.rm works as intended.
expect_equal(
  coef(mlbetapr(small_data)),
  coef(mlbetapr(c(small_data, NA), na.rm = TRUE))
)

## Check class.
est <- mlbetapr(tiny_data)
expect_equal(attr(est, "model"), "BetaPrime")
expect_equal(class(est), "univariateML")


## Check support.
expect_equal(class(attr(est, "support"))[[1]], "Intervals")
