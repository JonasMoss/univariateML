context("mlgumbel")

## Data generation.
set.seed(313)
small_data = extraDistr::rkumar(100, 1, 1)
tiny_data = extraDistr::rkumar(10, 3, 7)
medium_data = extraDistr::rkumar(1000, 9, 11)

## Checks if the ML is correct.
mle1 = suppressWarnings(nlm(function(p) {
  -sum(extraDistr::dkumar(small_data, p[1], p[2], log = TRUE))
}, p = c(1,1)))

mle2 = nlm(function(p) {
  -sum(extraDistr::dkumar(tiny_data, p[1], p[2], log = TRUE))
}, p = c(3, 7))

mle3 = nlm(function(p) {
  -sum(extraDistr::dkumar(medium_data, p[1], p[2], log = TRUE))
}, p = c(9, 11))

expect_equal(mle1$estimate, as.numeric(mlkumar(small_data)), tolerance = 1e-5)
expect_equal(mle2$estimate, as.numeric(mlkumar(tiny_data)), tolerance = 1e-5)
expect_equal(mle3$estimate, as.numeric(mlkumar(medium_data)), tolerance = 1e-5)

expect_equal(-mle1$minimum, attr(mlkumar(small_data), "logLik"), tolerance = 1e-5)
expect_equal(-mle2$minimum, attr(mlkumar(tiny_data), "logLik"), tolerance = 1e-5)
expect_equal(-mle3$minimum, attr(mlkumar(medium_data), "logLik"), tolerance = 1e-5)

## Checks warning with small iterlim.
expect_warning(mlkumar(tiny_data, iterlim = 1))

## Finds errors with na and data out of bounds.
expect_error(mlkumar(c(tiny_data, NA)))
expect_error(mlkumar(c(tiny_data, 0)))
expect_error(mlkumar(c(tiny_data, 1)))

## Checks that na.rm works as intended.
expect_equal(coef(mlkumar(small_data)),
             coef(mlkumar(c(small_data, NA), na.rm = TRUE)))

est = mlkumar(tiny_data)
## Check class.
expect_equal(attr(est, "model"), "Kumaraswamy")
expect_equal(class(est), "univariateML")
