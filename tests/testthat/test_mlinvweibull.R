context("mllinvweibull")

## Data generation.
set.seed(313)
small_data = actuar::rinvweibull(100, 7, 3)
tiny_data = actuar::rinvweibull(10, 1, 1)

## Finds errors with na and data out of bounds.
expect_error(mlinvweibull(c(tiny_data, NA)))
expect_error(mlinvweibull(c(tiny_data, -1)))
expect_error(mlinvweibull(c(tiny_data, 0)))

# Check correctness
expect_equal(mlweibull(1/small_data)[1], mlinvweibull(small_data)[1])

## Checks that na.rm works as intended.
expect_equal(coef(mlinvweibull(small_data)),
             coef(mlinvweibull(c(small_data, NA), na.rm = TRUE)))

## Is the log-likelihood correct?
est = mlinvweibull(small_data, na.rm = TRUE)
expect_equal(sum(actuar::dinvweibull(small_data, est[1], est[2], log = TRUE)),
             attr(est, "logLik"))

## Check class.
expect_equal(attr(est, "model"), "InverseWeibull")
expect_equal(class(est), "univariateML")

# Check names.
expect_equal(names(est), c("shape", "rate"))