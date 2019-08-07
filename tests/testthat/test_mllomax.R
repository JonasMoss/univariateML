context("mllomax")

## Data generation.
set.seed(313)
tiny_data = extraDistr::rlomax(10, 1, 7)
small_data = extraDistr::rlomax(100, 10, 3)
medium_data= extraDistr::rlomax(1000, 1/2, 2)
large_data= extraDistr::rlomax(1000, 20, 13)

## Checks if the ML is correct.
mle1 = suppressWarnings(nlm(function(p) {
  -mean(extraDistr::dlomax(tiny_data, p[1], p[2], log = TRUE))
}, p = c(1,1))$estimate)

mle2 = suppressWarnings(nlm(function(p) {
  -mean(extraDistr::dlomax(small_data, p[1], p[2], log = TRUE))
}, p = c(1,1))$estimate)

mle3 = suppressWarnings(nlm(function(p) {
  -mean(extraDistr::dlomax(medium_data, p[1], p[2], log = TRUE))
}, p = c(1,1))$estimate)

mle4 = suppressWarnings(nlm(function(p) {
  -mean(extraDistr::dlomax(large_data, p[1], p[2], log = TRUE))
}, p = c(1,1))$estimate)

expect_equal(mle1, as.numeric(mllomax(tiny_data)), tolerance = 1e-5)
expect_equal(mle2, as.numeric(mllomax(small_data)), tolerance = 1e-5)
expect_equal(mle3, as.numeric(mllomax(medium_data)), tolerance = 1e-5)
expect_equal(mle4, as.numeric(mllomax(large_data)), tolerance = 1e-5)


## Checks the control parameters.
expect_equal(mlbeta(tiny_data, type = "gradient"),
             mlbeta(tiny_data), tolerance = 1e-5)
expect_equal(mlbeta(small_data, type = "gradient"),
             mlbeta(small_data, type = "hessian"), tolerance = 1e-5)

## Finds errors with na and data out of bounds.
expect_error(mlbeta(c(0, tiny_data)))
expect_error(mlbeta(c(tiny_data, 1)))
expect_error(mlbeta(c(tiny_data, NA)))

## Checks that na.rm works as intended.
expect_equal(mlbeta(small_data),
             mlbeta(c(small_data, NA), na.rm = TRUE))

## Is the log-likelihood correct?
est = mlbeta(small_data, na.rm = TRUE)
expect_equal(sum(dbeta(small_data, est[1], est[2], log = TRUE)),
             attr(est, "logLik"))

## Check class.
expect_equal(attr(est, "model"), "Beta")
expect_equal(class(est), "univariateML")