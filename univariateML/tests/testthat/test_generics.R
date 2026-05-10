context("generics")

set.seed(313)
# Test of logLik function.
x <- stats::rweibull(10, 2, 3)
obj <- mlweibull(x)
expect_equal(attr(logLik(obj), "nobs"), 10)
expect_equal(attr(logLik(obj), "df"), 2)
expect_equal(attr(logLik(obj), "class"), "logLik")

# Test of invisible return in print / summmary function.

expect_identical(obj, print(obj))
expect_identical(obj, summary(obj))

# Test of coef function.
expect_equal(stats::setNames(as.numeric(obj), names(obj)), coef(obj))

# Test of plotting functions
expect_identical(plot(obj), obj)
expect_identical(points(obj), obj)
expect_identical(lines(obj), obj)

x <- stats::runif(10, 0, 1)
obj <- mlbeta(x)
expect_identical(plot(obj), obj)
expect_identical(points(obj), obj)
expect_identical(lines(obj), obj)

x <- extraDistr::rpower(10, 2, 3)
obj <- mlpower(x)
expect_identical(plot(obj), obj)
expect_identical(points(obj), obj)
expect_identical(lines(obj), obj)

x <- stats::rnorm(10, 2, 3)
obj <- mlnorm(x)
expect_identical(plot(obj), obj)
expect_identical(points(obj), obj)
expect_identical(lines(obj), obj)
