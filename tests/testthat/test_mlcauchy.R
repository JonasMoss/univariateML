context("mlcauchy")

## Data generation.
set.seed(313)
tiny_data = stats::rcauchy(10, 1, 7)
small_data = stats::rcauchy(100, 10, 3)
medium_data = stats::rcauchy(1000, 1/2, 2)
large_data = stats::rcauchy(10000, 20, 13)

## Checks if the ML is correct.

mle1 = suppressWarnings(nlm(function(p) {
  -sum(stats::dcauchy(tiny_data, p[1], p[2], log = TRUE))
}, p = c(1, 1)))

mle2 = suppressWarnings(nlm(function(p) {
  -sum(stats::dcauchy(small_data, p[1], p[2], log = TRUE))
}, p = c(1, 1)))

mle3 = suppressWarnings(nlm(function(p) {
  -sum(stats::dcauchy(medium_data, p[1], p[2], log = TRUE))
}, p = c(1, 1)))

mle4 = suppressWarnings(nlm(function(p) {
  -sum(stats::dcauchy(large_data, p[1], p[2], log = TRUE))
}, p = c(stats::median(large_data),
         stats::median(abs(large_data - stats::median(large_data))))))

## Checks estimates.
expect_equal(mle1$estimate, as.numeric(mlcauchy(tiny_data)), tolerance = 1e-5)
expect_equal(mle2$estimate, as.numeric(mlcauchy(small_data)), tolerance = 1e-5)
expect_equal(mle3$estimate, as.numeric(mlcauchy(medium_data)), tolerance = 1e-5)
expect_equal(mle4$estimate, as.numeric(mlcauchy(large_data)), tolerance = 1e-5)

## Checks logLiks.
expect_equal(-mle1$minimum, attr(mlcauchy(tiny_data), "logLik"), tolerance = 1e-5)
expect_equal(-mle2$minimum, attr(mlcauchy(small_data), "logLik"), tolerance = 1e-5)
expect_equal(-mle3$minimum, attr(mlcauchy(medium_data), "logLik"), tolerance = 1e-5)
expect_equal(-mle4$minimum, attr(mlcauchy(large_data), "logLik"), tolerance = 1e-5)

## Finds errors with na and data out of bounds.
expect_error(mlcauchy(c(tiny_data, NA)))

## Checks that na.rm works as intended.
expect_equal(mlcauchy(small_data),
             mlcauchy(c(small_data, NA), na.rm = TRUE))

## Check class.
est = mlcauchy(small_data, na.rm = TRUE)
expect_equal(attr(est, "model"), "Cauchy")
expect_equal(class(est), "univariateML")