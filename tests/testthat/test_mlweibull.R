context("mlweibull")

## Data generation.
set.seed(313)
small_data <- rweibull(100, 1, 1)
tiny_data <- rweibull(10, 3, 7)

## Checks if the ML is correct.
mle1 <- nlm(\(p) {
  -mean(dweibull(small_data, p[1], p[2], log = TRUE))
}, p = c(1, 1))$estimate

mle2 <- nlm(\(p) {
  -mean(dweibull(tiny_data, p[1], p[2], log = TRUE))
}, p = c(3, 7))$estimate

expect_equal(mle1, as.numeric(mlweibull(small_data)), tolerance = 1e-5)
expect_equal(mle2, as.numeric(mlweibull(tiny_data)), tolerance = 1e-5)

## Checks warning with small iterlim.
expect_warning(mlweibull(tiny_data, iterlim = 1))

## Finds errors with na and data out of bounds.
expect_error(mlweibull(c(tiny_data, NA)))
expect_error(mlweibull(c(0, tiny_data)))

## Checks that na.rm works as intended.
expect_equal(
  coef(mlweibull(small_data)),
  coef(mlweibull(c(small_data, NA), na.rm = TRUE))
)

## Is the log-likelihood correct?
est <- mlweibull(small_data, na.rm = TRUE)
expect_equal(
  sum(dweibull(small_data, est[1], est[2], log = TRUE)),
  attr(est, "logLik")
)

## Check class.
expect_equal(attr(est, "model"), "Weibull")
expect_equal(class(est), "univariateML")


## Check support.
expect_equal(class(attr(est, "support"))[[1]], "numeric")


## Check support.
expect_equal(class(attr(est, "support"))[[1]], "numeric")
