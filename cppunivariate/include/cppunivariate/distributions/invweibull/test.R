context("mllinvweibull")

## Data generation.
set.seed(313)
small_data <- actuar::rinvweibull(100, 7, 3)
tiny_data <- actuar::rinvweibull(10, 1, 1)

## Finds errors with na and data out of bounds.
expect_error(mlinvweibull(c(tiny_data, NA)))
expect_error(mlinvweibull(c(tiny_data, -1)))
expect_error(mlinvweibull(c(tiny_data, 0)))

# Check correctness
expect_equal(mlweibull(1 / small_data)[1], mlinvweibull(small_data)[1])

## Checks that na.rm works as intended.
expect_equal(
  coef(mlinvweibull(small_data)),
  coef(mlinvweibull(c(small_data, NA), na.rm = TRUE))
)

## Is the log-likelihood correct?
est <- mlinvweibull(small_data, na.rm = TRUE)
expect_equal(
  sum(actuar::dinvweibull(small_data, est[1], est[2], log = TRUE)),
  attr(est, "logLik")
)

## Check class.
expect_equal(attr(est, "model"), "InverseWeibull")
expect_equal(class(est), "univariateML")

# Check names.
expect_equal(names(est), c("shape", "rate"))


## Check support.
expect_equal(class(attr(est, "support"))[[1]], "numeric")

## Explicit shape starts are now used and validated.
x_start <- simulate_default("mlinvweibull", 100)
a_start <- mlinvweibull(x_start, reltol = 1e-12)
b_start <- mlinvweibull(x_start, shape0 = 1.2 * coef(a_start)[["shape"]], reltol = 1e-12)
expect_equal(coef(b_start), coef(a_start), tolerance = 1e-8)
for (bad_start in c(0, -1, Inf, NA_real_)) {
  expect_error(mlinvweibull(x_start, shape0 = bad_start), "positive finite")
}
