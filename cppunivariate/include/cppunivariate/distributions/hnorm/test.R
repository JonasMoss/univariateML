# The translated half-normal is extraDistr's truncated normal with a = mean.
dhn <- function(x, mu, sigma, log = FALSE) extraDistr::dtnorm(x, mu, sigma, a = mu, log = log)

test_that("translated half-normal closed form agrees with density oracle", {
  set.seed(73)
  x <- extraDistr::rtnorm(200, -3, 2, a = -3)
  fit <- mlhnorm(x)
  mu <- unname(fit[1])
  # Boundary MLE min(x), reported one double below it (dtnorm excludes a).
  expect_lt(mu, min(x))
  expect_lte(min(x) - mu, 2 * .Machine$double.eps * abs(min(x)))
  expect_equal(unname(coef(fit)), c(min(x), sqrt(mean((x - min(x))^2))), tolerance = 1e-12)
  oracle <- optimize(function(s) -sum(dhn(x, mu, s, log = TRUE)), c(0.01, 20), tol = 1e-10)
  expect_equal(unname(fit[2]), oracle$minimum, tolerance = 1e-7)
  expect_equal(attr(fit, "logLik"), -oracle$objective, tolerance = 1e-12)
  expect_equal(attr(fit, "logLik"), sum(dml(x, fit, log = TRUE)), tolerance = 1e-12)
  expect_lt(sum(dhn(x, mu - 0.01, fit[2], log = TRUE)), attr(fit, "logLik"))
  expect_equal(coef(mlhnorm(c(x, NA), na.rm = TRUE)), coef(fit))
  # Location-scale equivariance.
  expect_equal(unname(coef(mlhnorm(5 + 3 * x))), c(5 + 3 * mu, 3 * unname(fit[2])), tolerance = 1e-12)
  expect_error(mlhnorm(1), "at least two observations with distinct values")
  expect_error(mlhnorm(rep(2, 4)), "degenerate data")
  expect_error(mlhnorm(c(x, NA)))
  expect_error(mlhnorm(c(x, Inf)))
  expect_error(mlhnorm(x, iterlim = 10))
})

test_that("dml/pml/qml/rml map mu and sigma onto extraDistr's truncated normal", {
  fit <- mlhnorm(c(-4, -3, 0, 2))
  mu <- unname(fit[1]); sigma <- unname(fit[2])
  x <- c(-5, -4, -3.5, 0, 3)
  # Translated half-normal density 2 * dnorm(x, mu, sigma) on (mu, Inf).
  expect_equal(dml(x, fit), ifelse(x > mu, 2 * dnorm(x, mu, sigma), 0))
  expect_equal(dml(x[-1], fit, log = TRUE), log(2) + dnorm(x[-1], mu, sigma, log = TRUE))
  expect_equal(pml(x, fit), pmax(0, 2 * pnorm(x, mu, sigma) - 1))
  u <- c(0.001, 0.1, 0.5, 0.9, 0.999)
  expect_equal(qml(u, fit), mu + sigma * qnorm((1 + u) / 2))
  for (lower in c(TRUE, FALSE)) for (logs in c(TRUE, FALSE)) {
    p <- if (logs) log(u) else u
    expect_equal(pml(qml(p, fit, lower, logs), fit, lower), u, tolerance = 1e-10)
  }
  set.seed(9); actual <- rml(50, fit)
  set.seed(9); expect_equal(actual, extraDistr::rtnorm(50, mu, sigma, a = mu))
  expect_true(all(actual >= mu))
  expect_length(simulate_default("mlhnorm", 10), 10)
  expect_true(all(simulate_default("mlhnorm", 100) >= 0))
})

test_that("bootstrap refits with mlhnorm", {
  set.seed(5)
  fit <- mlhnorm(extraDistr::rtnorm(30, 1, 2, a = 1))
  boot <- bootstrapml(fit, reps = 20, reducer = identity)
  expect_equal(dim(boot), c(2L, 20L))
  expect_true(all(boot["sigma", ] > 0))
})

test_that("active truncated-normal repairs retain the half-normal map and endpoints", {
  expect_identical(univariateML:::univariateML_args$mlhnorm,
                   c(mean = "mu", sd = "sigma", a = "mu"))
  expect_true(univariateML:::univariateML_reference_fixes[["extraDistr::dtnorm"]]$broken())
  fit <- mlhnorm(c(-4,-3,0,2))
  mu <- unname(fit[1]); sigma <- unname(fit[2])
  x <- mu + sigma*c(-1,0,.01,.5,2)
  expect_equal(dml(x, fit), ifelse(x > mu, 2*dnorm(x, mu, sigma), 0), tolerance = 1e-12)
  expect_equal(dml(mu, fit, log = TRUE), -Inf)
  for (lower in c(TRUE,FALSE)) for (logs in c(TRUE,FALSE)) {
    upper <- pmin(1, 2*pnorm(x, mu, sigma, lower.tail = FALSE))
    expected <- if (lower) 1-upper else upper
    if (logs) expected <- log(expected)
    expect_equal(pml(x, fit, lower.tail = lower, log.p = logs), expected, tolerance = 1e-12)
    u <- c(0,.1,.5,.9,1)
    expected <- mu + sigma*qnorm(if (lower) (1-u)/2 else u/2, lower.tail = FALSE)
    expect_equal(qml(if (logs) log(u) else u, fit, lower.tail = lower, log.p = logs),
                 expected, tolerance = 1e-12)
  }
  set.seed(12); actual <- rml(30, fit)
  set.seed(12); expect_identical(actual, extraDistr::rtnorm(30, mu, sigma, a = mu))
})

test_that("half-normal re-profiles after rounding its endpoint", {
  x <- 1e15 + c(0, 1, 2, 3)
  f <- mlhnorm(x)
  expect_equal(f[["sigma"]], sqrt(mean((x - f[["mu"]])^2)))
  expect_equal(attr(f, "logLik"), sum(extraDistr::dtnorm(x, mean = f[["mu"]], sd = f[["sigma"]], a = f[["mu"]], log = TRUE)))
})
