context("mlpower")

## Data generation.
set.seed(313)
small_data <- extraDistr::rpower(100, 1, 1)
tiny_data <- extraDistr::rpower(10, 3, 7)
medium_data <- extraDistr::rpower(1000, 9, 11)

epsilon <- .Machine$double.eps^0.5

## Checks if the ML is correct.
mle1 <- suppressWarnings(nlm(function(p) {
  -sum(extraDistr::dpower(small_data, max(small_data) + epsilon, p, log = TRUE))
}, p = 1))

mle2 <- suppressWarnings(nlm(function(p) {
  -sum(extraDistr::dpower(tiny_data, max(tiny_data) + epsilon, p,
    log = TRUE
  ))
}, p = 7))

mle3 <- suppressWarnings(nlm(function(p) {
  -sum(extraDistr::dpower(medium_data, max(medium_data) + epsilon, p,
    log = TRUE
  ))
}, p = 11))

expect_equal(mle1$estimate, as.numeric(mlpower(small_data))[2],
  tolerance = 1e-5
)
expect_equal(mle2$estimate, as.numeric(mlpower(tiny_data))[2],
  tolerance = 1e-5
)
expect_equal(mle3$estimate, as.numeric(mlpower(medium_data))[2],
  tolerance = 1e-5
)

expect_equal(-mle1$minimum, attr(mlpower(small_data), "logLik"),
  tolerance = 1e-5
)
expect_equal(-mle2$minimum, attr(mlpower(tiny_data), "logLik"),
  tolerance = 1e-5
)
expect_equal(-mle3$minimum, attr(mlpower(medium_data), "logLik"),
  tolerance = 1e-5
)


## Finds errors with na and data out of bounds.
expect_error(mlpower(c(tiny_data, NA)))

## Checks that na.rm works as intended.
expect_equal(
  coef(mlpower(small_data)),
  coef(mlpower(c(small_data, NA), na.rm = TRUE))
)

est <- mlpower(tiny_data)

## Check class.
expect_equal(attr(est, "model"), "PowerDist")
expect_equal(class(est), "univariateML")


## Check support.
expect_equal(class(attr(est, "support")), "numeric")
