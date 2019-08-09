context("mllogis")

## Data generation.
set.seed(313)
tiny_data = stats::rlogis(10, 1, 7)
small_data = stats::rlogis(100, 10, 3)
medium_data = stats::rlogis(1000, 1/2, 2)
large_data = stats::rlogis(10000, 20, 13)

## Checks if the ML is correct.

m = stats::median(tiny_data)
mad = stats::median(abs(tiny_data - m))
start = c(m, log(mad))

mle1 = suppressWarnings(nlm(function(p) {
  -sum(stats::dlogis(tiny_data, p[1], exp(p[2]), log = TRUE))
}, p = start))


m = stats::median(small_data)
mad = stats::median(abs(small_data - m))
start = c(m, log(mad))

mle2 = suppressWarnings(nlm(function(p) {
  -sum(stats::dlogis(small_data, p[1], exp(p[2]), log = TRUE))
}, p = start))


m = stats::median(medium_data)
mad = stats::median(abs(medium_data - m))
start = c(m, log(mad))

mle3 = suppressWarnings(nlm(function(p) {
  -sum(stats::dlogis(medium_data, p[1], exp(p[2]), log = TRUE))
}, p = start))

m = stats::median(large_data)
mad = stats::median(abs(large_data - m))
start = c(m, log(mad))

mle4 = suppressWarnings(nlm(function(p) {
  -sum(stats::dlogis(large_data, p[1], exp(p[2]), log = TRUE))
}, p = start))

## Checks estimates.
expect_equal(c(mle1$estimate[1], exp(mle1$estimate[2])),
             as.numeric(mllogis(tiny_data)), tolerance = 1e-5)
expect_equal(c(mle2$estimate[1], exp(mle2$estimate[2])),
             as.numeric(mllogis(small_data)), tolerance = 1e-5)
expect_equal(c(mle3$estimate[1], exp(mle3$estimate[2])),
             as.numeric(mllogis(medium_data)), tolerance = 1e-5)
expect_equal(c(mle4$estimate[1], exp(mle4$estimate[2])),
             as.numeric(mllogis(large_data)), tolerance = 1e-5)

## Checks logLiks.
expect_equal(-mle1$minimum, attr(mllogis(tiny_data), "logLik"), tolerance = 1e-5)
expect_equal(-mle2$minimum, attr(mllogis(small_data), "logLik"), tolerance = 1e-5)
expect_equal(-mle3$minimum, attr(mllogis(medium_data), "logLik"), tolerance = 1e-5)
expect_equal(-mle4$minimum, attr(mllogis(large_data), "logLik"), tolerance = 1e-5)

## Finds errors with na and data out of bounds.
expect_error(mllogis(c(tiny_data, NA)))

## Checks that na.rm works as intended.
expect_equal(coef(mllogis(small_data)),
             coef(mllogis(c(small_data, NA), na.rm = TRUE)))

## Check class.
est = mllogis(small_data, na.rm = TRUE)
expect_equal(attr(est, "model"), "Logistic")
expect_equal(class(est), "univariateML")