context("mlinvgamma")

## Data generation.
set.seed(313)
small_data <- extraDistr::rinvgamma(100, 1, 1)
tiny_data <- extraDistr::rinvgamma(10, 3, 7)
data3 <- c(0, tiny_data)

## Checks if the ML is correct.
mle1 <- nlm(\(p) {
  -mean(extraDistr::dinvgamma(small_data, p[1], p[2], log = TRUE))
}, p = c(1, 1))$estimate

mle2 <- nlm(\(p) {
  -mean(extraDistr::dinvgamma(tiny_data, p[1], p[2], log = TRUE))
}, p = c(1, 1))$estimate

expect_equal(mle1, as.numeric(mlinvgamma(small_data)), tolerance = 1e-4)
expect_equal(mle2, as.numeric(mlinvgamma(tiny_data)), tolerance = 1e-4)

## Checks warning with small iterlim.
expect_warning(mlinvgamma(tiny_data, iterlim = 1))

## Finds errors with na and data out of bounds.
expect_error(mlinvgamma(c(tiny_data, NA)))
expect_error(mlinvgamma(c(tiny_data, 0)))

## Checks that na.rm works as intended.
expect_equal(
  coef(mlinvgamma(small_data)),
  coef(mlinvgamma(c(small_data, NA), na.rm = TRUE))
)

## Is the log-likelihood correct?
est <- mlinvgamma(small_data, na.rm = TRUE)
expect_equal(
  sum(extraDistr::dinvgamma(small_data, est[1], est[2], log = TRUE)),
  attr(est, "logLik")
)

## Check class.
expect_equal(attr(est, "model"), "InvGamma")
expect_equal(class(est), "univariateML")
