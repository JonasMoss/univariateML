context("mlpareto")

## Data generation.
set.seed(313)
tiny_data <- extraDistr::rpareto(10, 1, 7)
small_data <- extraDistr::rpareto(100, 3, 9)
medium_data <- extraDistr::rpareto(1000, 1 / 2, 2)
large_data <- extraDistr::rpareto(10000, 13, 20)

## Checks logLiks.
expect_equal(
  sum(extraDistr::dpareto(tiny_data,
    mlpareto(tiny_data)[1],
    mlpareto(tiny_data)[2],
    log = TRUE
  )),
  attr(mlpareto(tiny_data), "logLik"),
  tolerance = 1e-5
)
expect_equal(
  sum(extraDistr::dpareto(small_data,
    mlpareto(small_data)[1],
    mlpareto(small_data)[2],
    log = TRUE
  )),
  attr(mlpareto(small_data), "logLik"),
  tolerance = 1e-5
)
expect_equal(
  sum(extraDistr::dpareto(medium_data,
    mlpareto(medium_data)[1],
    mlpareto(medium_data)[2],
    log = TRUE
  )),
  attr(mlpareto(medium_data), "logLik"),
  tolerance = 1e-5
)
expect_equal(
  sum(extraDistr::dpareto(large_data,
    mlpareto(large_data)[1],
    mlpareto(large_data)[2],
    log = TRUE
  )),
  attr(mlpareto(large_data), "logLik"),
  tolerance = 1e-5
)

## Finds errors with na and data out of bounds.
expect_error(mlpareto(c(tiny_data, -1)))
expect_error(mlpareto(c(tiny_data, NA)))

## Checks that na.rm works as intended.
expect_equal(
  coef(mlpareto(small_data)),
  coef(mlpareto(c(small_data, NA), na.rm = TRUE))
)

## Check class.
est <- mlpareto(small_data, na.rm = TRUE)
expect_equal(attr(est, "model"), "Pareto")
expect_equal(class(est), "univariateML")


## Check support.
expect_equal(class(attr(est, "support"))[[1]], "numeric")
