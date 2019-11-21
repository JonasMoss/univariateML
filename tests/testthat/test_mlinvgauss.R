context("mlcuachy")

## Data generation.
set.seed(313)
tiny_data <- actuar::rinvgauss(10, 1, 7)
small_data <- actuar::rinvgauss(100, 10, 3)
medium_data <- actuar::rinvgauss(1000, 1 / 2, 2)
large_data <- actuar::rinvgauss(10000, 20, 13)

## Checks if the ML is correct.

mle1 <- suppressWarnings(nlm(function(p) {
  -sum(actuar::dinvgauss(tiny_data, p[1], p[2], log = TRUE))
}, p = c(1, 7)))

mle2 <- suppressWarnings(nlm(function(p) {
  -sum(actuar::dinvgauss(small_data, p[1], p[2], log = TRUE))
}, p = c(10, 3)))

mle3 <- suppressWarnings(nlm(function(p) {
  -sum(actuar::dinvgauss(medium_data, p[1], p[2], log = TRUE))
}, p = c(1 / 2, 2)))

mle4 <- suppressWarnings(nlm(function(p) {
  -sum(actuar::dinvgauss(large_data, p[1], p[2], log = TRUE))
}, p = c(20, 13)))

## Checks estimates.
expect_equal(mle1$estimate, as.numeric(mlinvgauss(tiny_data)),
  tolerance = 1e-5
)
expect_equal(mle2$estimate, as.numeric(mlinvgauss(small_data)),
  tolerance = 1e-5
)
expect_equal(mle3$estimate, as.numeric(mlinvgauss(medium_data)),
  tolerance = 1e-5
)
expect_equal(mle4$estimate, as.numeric(mlinvgauss(large_data)),
  tolerance = 1e-5
)

## Checks logLiks.
expect_equal(-mle1$minimum, attr(mlinvgauss(tiny_data), "logLik"),
  tolerance = 1e-5
)
expect_equal(-mle2$minimum, attr(mlinvgauss(small_data), "logLik"),
  tolerance = 1e-5
)
expect_equal(-mle3$minimum, attr(mlinvgauss(medium_data), "logLik"),
  tolerance = 1e-5
)
expect_equal(-mle4$minimum, attr(mlinvgauss(large_data), "logLik"),
  tolerance = 1e-5
)

## Finds errors with na and data out of bounds.
expect_error(mlinvgauss(c(tiny_data, 0)))
expect_error(mlinvgauss(c(tiny_data, NA)))

## Checks that na.rm works as intended.
expect_equal(
  coef(mlinvgauss(small_data)),
  coef(mlinvgauss(c(small_data, NA), na.rm = TRUE))
)

## Check class.
est <- mlinvgauss(small_data, na.rm = TRUE)
expect_equal(attr(est, "model"), "Inverse Gaussian")
expect_equal(class(est), "univariateML")
