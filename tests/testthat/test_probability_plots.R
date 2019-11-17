context("probability plots")

## Test of to_univariateML function.
set.seed(1)
y <- rexp(10)
obj <- mlgamma(y)
expect_s3_class(to_univariateML(y, mlgamma), "univariateML")
expect_s3_class(to_univariateML(y, mlgamma(y)), "univariateML")
expect_error(to_univariateML(y, 1))
expect_error(to_univariateML(y, c))

## Testing of ppmlplot functions.

# Is datax the default value?
expect_equal(
  ppmlplot(y, obj, plot.it = FALSE),
  ppmlplot(y, obj, plot.it = FALSE, datax = FALSE)
)

# Do x and y switch places when 'datax = TRUE'
expect_equal(
  ppmlplot(y, obj, plot.it = FALSE, datax = FALSE)$y,
  ppmlplot(y, obj, plot.it = FALSE, datax = TRUE)$x
)

expect_equal(
  ppmlplot(y, obj, plot.it = FALSE, datax = FALSE)$x,
  ppmlplot(y, obj, plot.it = FALSE, datax = TRUE)$y
)


# What happens with only NAs?
expect_error(ppmlplot(c(NA, NA), obj, plot.it = FALSE, datax = FALSE))
expect_error(ppmlplot(c(), obj, plot.it = FALSE, datax = FALSE))

# Does plot.it matter?

expect_equal(
  ppmlplot(y, obj, plot.it = FALSE),
  ppmlplot(y, obj)
)
expect_equal(
  ppmlpoints(y, obj, plot.it = FALSE),
  ppmlpoints(y, obj)
)

# Does ppline return NULL?
expect_null(ppmlline(y, obj))

## Testing of qqmlplot functions

# Is datax the default value?
expect_equal(
  qqmlplot(y, obj, plot.it = FALSE),
  qqmlplot(y, obj, plot.it = FALSE, datax = FALSE)
)

# Do x and y switch places when 'datax = TRUE'
expect_equal(
  qqmlplot(y, obj, plot.it = FALSE, datax = FALSE)$y,
  qqmlplot(y, obj, plot.it = FALSE, datax = TRUE)$x
)

expect_equal(
  qqmlplot(y, obj, plot.it = FALSE, datax = FALSE)$x,
  qqmlplot(y, obj, plot.it = FALSE, datax = TRUE)$y
)

# What happens with only NAs?
expect_error(qqmlplot(c(NA, NA), obj, plot.it = FALSE, datax = FALSE))
expect_error(qqmlplot(c(), obj, plot.it = FALSE, datax = FALSE))

# Does plot.it matter?

expect_equal(
  qqmlplot(y, obj, plot.it = FALSE),
  qqmlplot(y, obj)
)
expect_equal(
  qqmlpoints(y, obj, plot.it = FALSE),
  qqmlpoints(y, obj)
)

# Does qqline return NULL?
expect_null(qqmlline(y, obj))
expect_null(qqmlline(y, obj, datax = TRUE))
