context("mlgumbel")

## Data generation.
set.seed(313)
small_data <- extraDistr::rgumbel(100, 1, 1)
tiny_data <- extraDistr::rgumbel(10, 3, 7)
medium_data <- extraDistr::rgumbel(1000, 9, 11)

## Checks if the ML is correct.
mle1 <- suppressWarnings(nlm(function(p) {
  -sum(extraDistr::dgumbel(small_data, p[1], p[2], log = TRUE))
}, p = c(1, 1)))

mle2 <- nlm(function(p) {
  -sum(extraDistr::dgumbel(tiny_data, p[1], p[2], log = TRUE))
}, p = c(3, 7))

mle3 <- nlm(function(p) {
  -sum(extraDistr::dgumbel(medium_data, p[1], p[2], log = TRUE))
}, p = c(9, 11))

expect_equal(mle1$estimate, as.numeric(mlgumbel(small_data)),
  tolerance = 1e-5
)
expect_equal(mle2$estimate, as.numeric(mlgumbel(tiny_data)),
  tolerance = 1e-5
)
expect_equal(mle3$estimate, as.numeric(mlgumbel(medium_data)),
  tolerance = 1e-5
)

expect_equal(-mle1$minimum, attr(mlgumbel(small_data), "logLik"),
  tolerance = 1e-5
)
expect_equal(-mle2$minimum, attr(mlgumbel(tiny_data), "logLik"),
  tolerance = 1e-5
)
expect_equal(-mle3$minimum, attr(mlgumbel(medium_data), "logLik"),
  tolerance = 1e-5
)

## Checks warning with small iterlim.
expect_warning(mlgumbel(tiny_data, iterlim = 1))

## Finds errors with na and data out of bounds.
expect_error(mlgumbel(c(tiny_data, NA)))

## Checks that na.rm works as intended.
expect_equal(
  coef(mlgumbel(small_data)),
  coef(mlgumbel(c(small_data, NA), na.rm = TRUE))
)

est <- mlgumbel(tiny_data)
## Check class.
expect_equal(attr(est, "model"), "Gumbel")
expect_equal(class(est), "univariateML")
