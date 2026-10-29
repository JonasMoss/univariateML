context("mlsnorm")

## Data generation.
set.seed(313)
small_data <- rsnorm(100)
tiny_data <- rsnorm(100)

## Finds errors with na and data out of bounds.
expect_error(mlsnorm(c(tiny_data, NA)))

## Checks that na.rm works as intended.
expect_equal(
  coef(mlsnorm(small_data)),
  coef(mlsnorm(c(small_data, NA), na.rm = TRUE))
)

## Is the log-likelihood correct?
est <- mlsnorm(small_data, na.rm = TRUE)
expect_equal(
  sum(dsnorm(small_data, est[1], est[2], est[3], log = TRUE)),
  attr(est, "logLik")
)

## Are the fallback methods working correctly?
expect_equal(
  log(dml(small_data, est)),
  dml(small_data, est, log = TRUE)
)
expect_equal(
  log(pml(small_data, est)),
  pml(small_data, est, log.p = TRUE)
)
expect_equal(
  1 - pml(small_data, est),
  pml(small_data, est, lower.tail = FALSE)
)
expect_equal(
  qml(1:3 / 4, est),
  qml(log(1:3 / 4), est, log.p = TRUE)
)
expect_equal(
  qml(1 - 1:3 / 4, est),
  qml(1:3 / 4, est, lower.tail = FALSE)
)


## Check class.
expect_equal(attr(est, "model"), "Skew Normal")
expect_equal(class(est), "univariateML")



## Check support.
expect_equal(class(attr(est, "support"))[[1]], "numeric")

# Independent direct-sum profile oracle: grid, then refine every nominated
# local minimum. Endpoints are included explicitly, never approximated.
snorm_profile_reference <- function(x) {
  objective <- function(c) {
    d <- x - c
    (sum(d[d < 0]^2)^(1/3) + sum(d[d >= 0]^2)^(1/3))^3
  }
  grid <- seq(min(x), max(x), length.out = 2001)
  q <- vapply(grid, objective, numeric(1))
  candidates <- which(q[2:2000] <= q[1:1999] &
                      q[2:2000] <= q[3:2001]) + 1L
  modes <- c(min(x), max(x), vapply(candidates, function(i)
    optimize(objective, grid[c(i-1L, i+1L)], tol = 1e-12)$minimum,
    numeric(1)))
  values <- vapply(modes, objective, numeric(1))
  winner <- which.min(values)
  list(boundary = if (winner <= 2L) winner else 0L,
       logLik = length(x)/2 * (log(2/pi) - log(values[winner]/length(x)) - 1))
}

test_that("exact snorm profile agrees with 240 independent grid searches", {
  set.seed(28012026)
  for (i in seq_len(240)) {
    n <- c(5L, 10L, 30L, 100L, 250L, 500L)[(i-1L) %% 6L + 1L]
    x <- switch((i-1L) %% 4L + 1L, rnorm(n), rexp(n),
                rsnorm(n, xi = 2.5), -rexp(n))
    reference <- snorm_profile_reference(x)
    if (reference$boundary != 0L) {
      expect_error(mlsnorm(x), if (reference$boundary == 1L)
        "mode at the sample minimum.*MLE does not exist" else
        "mode at the sample maximum.*MLE does not exist", info = paste("sample", i))
    } else {
      fit <- mlsnorm(x)
      expect_gte(attr(fit, "logLik"), reference$logLik - 1e-9)
      expect_equal(sum(dsnorm(x, fit[1], fit[2], fit[3], log = TRUE)),
                   attr(fit, "logLik"), tolerance = 1e-12)
    }
  }
})

test_that("two-piece conversion matches fGarch log densities", {
  for (a in c(0.2, 1, 3)) for (b in c(0.3, 1, 4)) {
    c <- 0.7
    x <- c + c(-2*a, -0.1*a, 0, 0.1*b, 2*b)
    m1 <- sqrt(2/pi)
    mu <- c + m1*(b-a)
    sd <- sqrt(a*a-a*b+b*b-m1*m1*(b-a)^2)
    expected <- log(2/(a+b)) + dnorm((x-c)/ifelse(x<c,a,b), log=TRUE)
    expect_lt(max(abs(dsnorm(x,mu,sd,sqrt(b/a),log=TRUE)-expected)), 1e-14)
  }
})

test_that("half-normal boundary messages name the available fitter", {
  x <- c(0, 0, 0, 1, 2, 8)
  expect_error(mlsnorm(x), "Use `mlhnorm` to fit")
  expect_error(mlsnorm(-x), "Use `mlhnorm` on `-x` \\(a half-normal reflected at the sample maximum\\)")
})

# Package-owned dpqr contract, independent of the optional reference package.
test_that("snorm dpqr integrate, invert and preserve extreme tails", {
  pars <- list(mean = .3, sd = 1.7, xi = 1.8)
  d <- function(x, ...) do.call(dsnorm, c(list(x), pars, list(...)))
  p <- function(q, ...) do.call(psnorm, c(list(q), pars, list(...)))
  q <- function(p, ...) do.call(qsnorm, c(list(p), pars, list(...)))
  for (x in c(-2, .3, 3)) {
    expect_equal(p(x), integrate(d, -Inf, x, rel.tol = 2e-10, subdivisions = 1000L)$value,
                 tolerance = 2e-9)
    expect_equal(p(x, lower.tail = FALSE),
                 integrate(d, x, Inf, rel.tol = 2e-10, subdivisions = 1000L)$value,
                 tolerance = 2e-9)
  }
  expect_equal(integrate(d, -Inf, Inf, rel.tol = 1e-9, subdivisions = 1000L)$value,
               1, tolerance = 2e-9)
  for (lower in c(TRUE, FALSE)) {
    lp <- c(-1000, -700, -100, -30, -2, -.1, -1e-12)
    expect_equal(p(q(lp, lower.tail = lower, log.p = TRUE), lower.tail = lower,
                   log.p = TRUE), lp, tolerance = 2e-11)
    prob <- c(1e-100, 1e-20, .01, .5, .99)
    expect_equal(p(q(prob, lower.tail = lower), lower.tail = lower), prob,
                 tolerance = 2e-12)
  }
  expect_equal(d(c(-Inf, Inf)), c(0, 0))
  expect_equal(d(c(-Inf, Inf), log = TRUE), c(-Inf, -Inf))
  expect_equal(p(c(-Inf, Inf)), c(0, 1))
  expect_equal(q(c(0, 1)), c(-Inf, Inf))
  expect_equal(q(c(-Inf, 0), log.p = TRUE), c(-Inf, Inf))
  expect_true(all(is.finite(d(c(-1000, 1000), log = TRUE))))
})

test_that("snorm dpqr recycle, propagate missing values and use R RNG", {
  expect_length(dsnorm(numeric()), 0)
  expect_equal(dsnorm(c(0, 1, 2), mean = c(0, 1)),
    c(dsnorm(0), dsnorm(1, mean = 1), dsnorm(2)))
  expect_identical(dsnorm(NA_real_), NA_real_)
  expect_true(is.nan(psnorm(NaN)))
  expect_warning(expect_true(is.nan(dsnorm(0, sd = -1))), "NaNs produced")
  expect_warning(expect_true(is.nan(qsnorm(1.1))), "NaNs produced")
  expect_warning(expect_true(is.nan(rsnorm(1, sd = -1))), "NaNs produced")
  set.seed(81); first <- rsnorm(100)
  set.seed(81); expect_identical(rsnorm(100), first)
  expect_length(rsnorm(1:7), 7)
  expect_length(rsnorm(0), 0)
  expect_length(rsnorm(3, mean = numeric()), 0)
  expect_identical(rsnorm(1, mean = NA_real_), NA_real_)
  set.seed(712)
  draws <- rsnorm(5000)
  for (prob in c(.1, .5, .9))
    expect_lt(abs(mean(draws <= qsnorm(prob)) - prob), .025)
  for (bad in c(-1, 0, Inf)) {
    expect_warning(expect_true(is.nan(dsnorm(0, sd = bad))), "NaNs produced")
    expect_warning(expect_true(is.nan(psnorm(0, sd = bad))), "NaNs produced")
    expect_warning(expect_true(is.nan(qsnorm(.5, sd = bad))), "NaNs produced")
  }
  fit <- mlsnorm(small_data)
  expect_equal(as.numeric(logLik(fit)), sum(dml(small_data, fit, log = TRUE)),
               tolerance = 1e-10)
  expect_equal(pml(0, fit), do.call(psnorm, c(list(0), as.list(coef(fit)))))
  expect_equal(qml(.2, fit), do.call(qsnorm, c(list(.2), as.list(coef(fit)))))
  set.seed(93); expected <- do.call(rsnorm, c(list(10), as.list(coef(fit))))
  set.seed(93); expect_identical(rml(10, fit), expected)
})

test_that("snorm matches frozen fGarch values (reference.json)", {
  expect_reference_values("snorm")
})

test_that("snorm skew parameter domain is enforced", {
  for (bad in c(-1, 0, Inf))
    expect_warning(expect_true(is.nan(dsnorm(0, xi = bad))), "NaNs produced")
})

test_that("snorm log tails exceed the backend exponent range", {
  for (lower in c(TRUE, FALSE)) {
    x <- qsnorm(-20000, lower.tail = lower, log.p = TRUE)
    expect_true(is.finite(x))
    expect_equal(psnorm(x, lower.tail = lower, log.p = TRUE), -20000,
                 tolerance = 1e-10)
    x <- qsnorm(-1e-30, lower.tail = lower, log.p = TRUE)
    expect_equal(psnorm(x, lower.tail = lower, log.p = TRUE)/(-1e-30), 1,
                 tolerance = 1e-10)
  }
})
