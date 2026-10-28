context("mllnorm")

## Data generation.
set.seed(313)
small_data <- stats::rlnorm(100, 2, 3)
tiny_data <- stats::rlnorm(10, 0, 1)

## Finds errors with na and data out of bounds.
expect_error(mllnorm(c(tiny_data, NA)))
expect_error(mllnorm(c(tiny_data, -1)))
expect_error(mllnorm(c(tiny_data, 0)))

## Checks that na.rm works as intended.
expect_equal(
  coef(mllnorm(small_data)),
  coef(mllnorm(c(small_data, NA), na.rm = TRUE))
)

## Is the log-likelihood correct?
est <- mllnorm(small_data, na.rm = TRUE)
expect_equal(
  sum(stats::dlnorm(small_data, est[1], est[2], log = TRUE)),
  attr(est, "logLik")
)

## Check class.
expect_equal(attr(est, "model"), "Lognormal")
expect_equal(class(est), "univariateML")


## Check support.
expect_equal(class(attr(est, "support"))[[1]], "numeric")


test_that("lnorm handles a seeded sample with a large outlier", {
  set.seed(7)
  x <- c(stats::rlnorm(199), 1e3)
  objective <- function(p) -sum(stats::dlnorm(x, p[1], exp(p[2]), log = TRUE))
  oracle <- optim(c(mean(log(x)), log(sd(log(x)))), objective,
    method = "Nelder-Mead", control = list(reltol = 1e-14, maxit = 10000))
  expected <- c(oracle$par[1], exp(oracle$par[2]))
  fit <- mllnorm(x)
  expect_true(all(is.finite(fit)))
  expect_true(all(abs(as.numeric(fit) - expected) <= 1e-4 * abs(expected)))
  expect_gte(attr(fit, "logLik"), -oracle$value - 1e-6)
})
