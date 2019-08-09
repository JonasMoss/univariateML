context("mllomax")

## Data generation.
set.seed(313)
tiny_data = extraDistr::rlomax(10, 1, 7)
small_data = extraDistr::rlomax(100, 10, 3)
medium_data = extraDistr::rlomax(1000, 1/2, 2)
large_data = extraDistr::rlomax(10000, 20, 13)

## Checks if the ML is correct.
mle2 = suppressWarnings(nlm(function(p) {
  -sum(extraDistr::dlomax(small_data, p[1], p[2], log = TRUE))
}, p = c(1,1))$estimate)

mle3 = suppressWarnings(nlm(function(p) {
  -sum(extraDistr::dlomax(medium_data, p[1], p[2], log = TRUE))
}, p = c(1,1))$estimate)

mle4 = suppressWarnings(nlm(function(p) {
  -sum(extraDistr::dlomax(large_data, p[1], p[2], log = TRUE))
}, p = c(1,1))$estimate)


expect_equal(mle2, as.numeric(mllomax(small_data)), tolerance = 1e-3)
expect_equal(mle3, as.numeric(mllomax(medium_data)), tolerance = 1e-3)
expect_equal(mle4, as.numeric(mllomax(large_data)), tolerance = 1e-3)

## Checks warning with small iterlim.
expect_warning(mllomax(small_data, iterlim = 1))

## Error when the MLE does not exist.
expect_error(mllomax(airquality$Wind))
expect_error(mllomax(tiny_data, start = median(tiny_data)))

## Finds errors with na and data out of bounds.
expect_error(mllomax(c(tiny_data, NA)))
expect_error(mllomax(c(0, tiny_data)))

## Checks that na.rm works as intended.
expect_equal(coef(mllomax(small_data)),
             coef(mllomax(c(small_data, NA), na.rm = TRUE)))

## Is the log-likelihood correct?
est = mllomax(small_data, na.rm = TRUE)
expect_equal(sum(extraDistr::dlomax(small_data, est[1], est[2], log = TRUE)),
             attr(est, "logLik"))

## Check class.
expect_equal(attr(est, "model"), "Lomax")
expect_equal(class(est), "univariateML")