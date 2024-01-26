context("mlunif")

## Data generation.
set.seed(313)
tiny_data <- stats::runif(10, 1, 7)
small_data <- stats::runif(100, 3, 9)
medium_data <- stats::runif(1000, 1 / 2, 2)
large_data <- stats::runif(10000, 13, 20)

## Checks logLiks.
expect_equal(sum(dunif(tiny_data,
  min = min(tiny_data),
  max = max(tiny_data), log = TRUE
)),
attr(mlunif(tiny_data), "logLik"),
tolerance = 1e-5
)
expect_equal(sum(dunif(small_data,
  min = min(small_data),
  max = max(small_data), log = TRUE
)),
attr(mlunif(small_data), "logLik"),
tolerance = 1e-5
)
expect_equal(sum(dunif(medium_data,
  min = min(medium_data),
  max = max(medium_data), log = TRUE
)),
attr(mlunif(medium_data), "logLik"),
tolerance = 1e-5
)
expect_equal(sum(dunif(large_data,
  min = min(large_data),
  max = max(large_data), log = TRUE
)),
attr(mlunif(large_data), "logLik"),
tolerance = 1e-5
)

## Finds errors with na and data out of bounds.
expect_error(mlunif(c(tiny_data, NA)))

## Checks that na.rm works as intended.
expect_equal(
  coef(mlunif(small_data)),
  coef(mlunif(c(small_data, NA), na.rm = TRUE))
)

## Check class.
est <- mlunif(small_data, na.rm = TRUE)
expect_equal(attr(est, "model"), "Uniform")
expect_equal(class(est), "univariateML")

## Check support.
expect_equal(class(attr(est, "support")), "numeric")

