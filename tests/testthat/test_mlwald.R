context("mlcuachy")

## Data generation.
set.seed(313)
tiny_data = extraDistr::rwald(10, 1, 7)
small_data = extraDistr::rwald(100, 10, 3)
medium_data = extraDistr::rwald(1000, 1/2, 2)
large_data = extraDistr::rwald(10000, 20, 13)

## Checks if the ML is correct.

mle1 = suppressWarnings(nlm(function(p) {
  -sum(extraDistr::dwald(tiny_data, p[1], p[2], log = TRUE))
}, p = c(1, 7)))

mle2 = suppressWarnings(nlm(function(p) {
  -sum(extraDistr::dwald(small_data, p[1], p[2], log = TRUE))
}, p = c(10, 3)))

mle3 = suppressWarnings(nlm(function(p) {
  -sum(extraDistr::dwald(medium_data, p[1], p[2], log = TRUE))
}, p = c(1/2, 2)))

mle4 = suppressWarnings(nlm(function(p) {
  -sum(extraDistr::dwald(large_data, p[1], p[2], log = TRUE))
}, p = c(20, 13)))

## Checks estimates.
expect_equal(mle1$estimate, as.numeric(mlwald(tiny_data)), tolerance = 1e-5)
expect_equal(mle2$estimate, as.numeric(mlwald(small_data)), tolerance = 1e-5)
expect_equal(mle3$estimate, as.numeric(mlwald(medium_data)), tolerance = 1e-5)
expect_equal(mle4$estimate, as.numeric(mlwald(large_data)), tolerance = 1e-5)

## Checks logLiks.
expect_equal(-mle1$minimum, attr(mlwald(tiny_data), "logLik"), tolerance = 1e-5)
expect_equal(-mle2$minimum, attr(mlwald(small_data), "logLik"), tolerance = 1e-5)
expect_equal(-mle3$minimum, attr(mlwald(medium_data), "logLik"), tolerance = 1e-5)
expect_equal(-mle4$minimum, attr(mlwald(large_data), "logLik"), tolerance = 1e-5)

## Finds errors with na and data out of bounds.
expect_error(mlwald(c(tiny_data, 0)))
expect_error(mlwald(c(tiny_data, NA)))

## Checks that na.rm works as intended.
expect_equal(mlwald(small_data),
             mlwald(c(small_data, NA), na.rm = TRUE))

## Check class.
est = mlwald(small_data, na.rm = TRUE)
expect_equal(attr(est, "model"), "Wald")
expect_equal(class(est), "univariateML")