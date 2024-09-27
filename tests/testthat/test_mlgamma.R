context("mlgamma")

## Data generation.
set.seed(313)
small_data <- stats::rgamma(100, 1, 1)
tiny_data <- stats::rgamma(10, 3, 7)
data3 <- c(0, tiny_data)

## Checks if the ML is correct.
mle1 <- nlm(\(p) {
  -mean(stats::dgamma(small_data, p[1], p[2], log = TRUE))
}, p = c(1, 1))$estimate

mle2 <- nlm(\(p) {
  -mean(stats::dgamma(tiny_data, p[1], p[2], log = TRUE))
}, p = c(1, 1))$estimate

expect_equal(mle1, as.numeric(mlgamma(small_data)), tolerance = 1e-5)
expect_equal(mle2, as.numeric(mlgamma(tiny_data)), tolerance = 1e-5)

## Checks warning with small iterlim.
expect_warning(mlgamma(tiny_data, iterlim = 1))

## Finds errors with na and data out of bounds.
expect_error(mlgamma(c(tiny_data, NA)))
expect_error(mlgamma(c(tiny_data, 0)))

## Checks that na.rm works as intended.
expect_equal(
  coef(mlgamma(small_data)),
  coef(mlgamma(c(small_data, NA), na.rm = TRUE))
)

## Is the log-likelihood correct?
est <- mlgamma(small_data, na.rm = TRUE)
expect_equal(
  sum(stats::dgamma(small_data, est[1], est[2], log = TRUE)),
  attr(est, "logLik")
)

## Check class.
expect_equal(attr(est, "model"), "Gamma")
expect_equal(class(est), "univariateML")


## Check support.
expect_equal(class(attr(est, "support"))[[1]], "Intervals")
