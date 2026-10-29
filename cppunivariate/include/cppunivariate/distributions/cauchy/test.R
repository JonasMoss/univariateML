context("mlcauchy")

## Data generation.
set.seed(313)
tiny_data <- stats::rcauchy(10, 1, 7)
small_data <- stats::rcauchy(100, 10, 3)
medium_data <- stats::rcauchy(1000, 1 / 2, 2)
large_data <- stats::rcauchy(10000, 20, 13)

## Checks if the ML is correct.

mle1 <- suppressWarnings(nlm(function(p) {
  -sum(stats::dcauchy(tiny_data, p[1], p[2], log = TRUE))
}, p = c(1, 1)))

mle2 <- suppressWarnings(nlm(function(p) {
  -sum(stats::dcauchy(small_data, p[1], p[2], log = TRUE))
}, p = c(1, 1)))

mle3 <- suppressWarnings(nlm(function(p) {
  -sum(stats::dcauchy(medium_data, p[1], p[2], log = TRUE))
}, p = c(1, 1)))

mle4 <- suppressWarnings(nlm(function(p) {
  -sum(stats::dcauchy(large_data, p[1], p[2], log = TRUE))
}, p = c(
  stats::median(large_data),
  stats::median(abs(large_data - stats::median(large_data)))
)))

## Checks estimates.
expect_equal(mle1$estimate, as.numeric(mlcauchy(tiny_data)),
  tolerance = 1e-5
)
expect_equal(mle2$estimate, as.numeric(mlcauchy(small_data)),
  tolerance = 1e-5
)
expect_equal(mle3$estimate, as.numeric(mlcauchy(medium_data)),
  tolerance = 1e-5
)
expect_equal(mle4$estimate, as.numeric(mlcauchy(large_data)),
  tolerance = 1e-5
)

## Checks logLiks.
expect_equal(-mle1$minimum, attr(mlcauchy(tiny_data), "logLik"),
  tolerance = 1e-5
)
expect_equal(-mle2$minimum, attr(mlcauchy(small_data), "logLik"),
  tolerance = 1e-5
)
expect_equal(-mle3$minimum, attr(mlcauchy(medium_data), "logLik"),
  tolerance = 1e-5
)
expect_equal(-mle4$minimum, attr(mlcauchy(large_data), "logLik"),
  tolerance = 1e-5
)

## Finds errors with na and data out of bounds.
expect_error(mlcauchy(c(tiny_data, NA)))

## Checks that na.rm works as intended.
expect_equal(
  coef(mlcauchy(small_data)),
  coef(mlcauchy(c(small_data, NA), na.rm = TRUE))
)

## Check class.
est <- mlcauchy(small_data, na.rm = TRUE)
expect_equal(attr(est, "model"), "Cauchy")
expect_equal(class(est), "univariateML")


## Check support.
expect_equal(class(attr(est, "support"))[[1]], "numeric")


test_that("cauchy handles a seeded sample with a large outlier", {
  set.seed(7)
  x <- c(stats::rcauchy(199), 1e3)
  objective <- function(p) -sum(stats::dcauchy(x, p[1], exp(p[2]), log = TRUE))
  oracle <- optim(c(median(x), log(stats::mad(x))), objective,
    method = "Nelder-Mead", control = list(reltol = 1e-14, maxit = 10000))
  expected <- c(oracle$par[1], exp(oracle$par[2]))
  fit <- mlcauchy(x)
  expect_true(all(is.finite(fit)))
  expect_true(all(abs(as.numeric(fit) - expected) <= 1e-4 * abs(expected)))
  expect_gte(attr(fit, "logLik"), -oracle$value - 1e-6)
})


test_that("Cauchy fits three observations and diagnoses nonexistent MLEs", {
  x <- c(-2.2425190273413, 0.723253413022922, -2.56072856385821)
  objective <- function(p) -sum(stats::dcauchy(x, p[1], exp(p[2]), log = TRUE))
  oracle <- optim(c(median(x), log(mad(x))), objective,
    control = list(reltol = 1e-14, maxit = 10000))
  expected <- c(oracle$par[1], exp(oracle$par[2]))
  fit <- mlcauchy(x)
  expect_true(all(abs(as.numeric(fit) - expected) <= 1e-4 * abs(expected)))
  expect_gte(attr(fit, "logLik"), -oracle$value - 1e-6)
  for (x in list(1, c(1, 1), c(1, 1, 2), c(1, 1, 2, 3))) {
    expect_error(mlcauchy(x), "MLE does not exist")
  }
})

test_that("balanced two-point Cauchy data have attained nonunique maxima", {
  for (x in list(c(1, 2), c(1, 1, 2, 2))) {
    f <- mlcauchy(x)
    expect_equal(as.numeric(f), c(1.5, .5))
    objective <- function(p) -sum(dcauchy(x, p[1], exp(p[2]), log = TRUE))
    o <- optim(c(1.4, log(.6)), objective, control = list(reltol=1e-12))
    expect_equal(attr(f, "logLik"), -o$value, tolerance=1e-8)
  }
})
test_that("Cauchy fitting respects extreme scale changes", {
  x <- c(-3, -.5, 0, 1, 4)
  f <- mlcauchy(x)
  for (s in c(1e-300, 1e300)) {
    g <- mlcauchy(x*s)
    expect_equal(as.numeric(g)/s, as.numeric(f), tolerance=1e-4)
    expect_equal(attr(g,"logLik") + length(x)*log(s), attr(f,"logLik"), tolerance=1e-6)
  }
})
