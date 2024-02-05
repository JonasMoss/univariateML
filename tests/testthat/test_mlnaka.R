context("mlnakagami")

## Data generation.
set.seed(313)
small_data <- nakagami::rnaka(100, 1, 1)
tiny_data <- nakagami::rnaka(10, 7, 7)
data3 <- c(0, tiny_data)

## Checks if the ML is correct.
mle1 <- nlm(function(p) {
  -mean(nakagami::dnaka(small_data, p[1], p[2], log = TRUE))
}, p = c(1, 1))$estimate

mle2 <- nlm(function(p) {
  -mean(nakagami::dnaka(tiny_data, p[1], p[2], log = TRUE))
}, p = c(7, 7))$estimate

expect_equal(mle1, as.numeric(mlnaka(small_data)), tolerance = 1e-5)
expect_equal(mle2, as.numeric(mlnaka(tiny_data)), tolerance = 1e-5)

## Checks warning with small iterlim.
expect_warning(mlnaka(tiny_data, iterlim = 1))

## Finds errors with na and data out of bounds.
expect_error(mlnaka(c(tiny_data, NA)))
expect_error(mlnaka(c(tiny_data, 0)))

## Checks that na.rm works as intended.
expect_equal(
  coef(mlnaka(small_data)),
  coef(mlnaka(c(small_data, NA), na.rm = TRUE))
)

## Is the log-likelihood correct?
est <- mlnaka(small_data, na.rm = TRUE)
expect_equal(
  sum(nakagami::dnaka(small_data, est[1], est[2], log = TRUE)),
  attr(est, "logLik")
)

## Check class.
expect_equal(attr(est, "model"), "Nakagami")
expect_equal(class(est), "univariateML")


## Check support.
expect_equal(class(attr(est, "support")), "numeric")
