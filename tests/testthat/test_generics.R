context("generics")

# Test of logLik function.

x = stats::rweibull(10, 2 ,3)
obj = mlweibull(x)
expect_equal(attr(logLik(obj), "nobs"), 10)
expect_equal(attr(logLik(obj), "df"), 2)
expect_equal(attr(logLik(obj), "class"), "logLik")

# Test of invisible return in print / summmary function.

expect_identical(obj, print(obj))
expect_identical(obj, summary(obj))

# Test of coef function.
expect_equal(stats::setNames(as.numeric(obj), names(obj)), coef(obj))