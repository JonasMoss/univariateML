context("mlgamma")

## Data generation.
set.seed(313)
small_data <- stats::rgamma(100, 1, 1)
tiny_data <- stats::rgamma(10, 3, 7)
data3 <- c(0, tiny_data)

## Checks if the ML is correct.
mle1 <- nlm(function(p) {
  -mean(stats::dgamma(small_data, p[1], p[2], log = TRUE))
}, p = c(1, 1))$estimate

mle2 <- nlm(function(p) {
  -mean(stats::dgamma(tiny_data, p[1], p[2], log = TRUE))
}, p = c(1, 1))$estimate

expect_equal(mle1, as.numeric(mlgamma(small_data)), tolerance = 1e-5)
expect_equal(mle2, as.numeric(mlgamma(tiny_data)), tolerance = 1e-5)

## Checks warning with small iterlim.
## expect_warning(mlgamma(tiny_data, iterlim = 1))

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
expect_equal(class(attr(est, "support"))[[1]], "numeric")

test_that("constant data have no attained gamma MLE", {
  expect_error(mlgamma(rep(2, 5)), "does not exist")
  expect_error(mlgamma(2), "does not exist")
})

test_that("gamma resolves near ties and extreme scales", {
  x <- 1 + (0:4) * .Machine$double.eps
  f <- mlgamma(x)
  expect_gt(f[["shape"]], 1e29)
  expect_true(is.finite(attr(f, "logLik")))
  expect_equal(f[["shape"]] / f[["rate"]], mean(x))
  y <- c(.2, .8, 1, 2, 4)
  base <- mlgamma(y)
  for (s in c(1e-300, 1e300)) {
    f <- mlgamma(y * s)
    expect_equal(f[["shape"]], base[["shape"]], tolerance = 1e-8)
    expect_equal(f[["rate"]] * s, base[["rate"]], tolerance = 1e-8)
    expect_equal(attr(f, "logLik") + length(y)*log(s), attr(base, "logLik"), tolerance = 1e-8)
  }
})
