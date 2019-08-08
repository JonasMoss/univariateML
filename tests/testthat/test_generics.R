context("generics")

#" Test logLik function.

x = stats::rweibull(10, 2 ,3)
expect_equal(attr(logLik(mlweibull(x)), "nobs"), 10)
expect_equal(attr(logLik(mlweibull(x)), "df"), 2)
expect_equal(attr(logLik(mlweibull(x)), "class"), "logLik")