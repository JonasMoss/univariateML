# Independent stable normal CDF differences, used only by the test oracle.
tnorm_test_logmass <- function(a, b) {
  if (a >= 0) {
    big <- pnorm(a, lower.tail = FALSE, log.p = TRUE)
    small <- pnorm(b, lower.tail = FALSE, log.p = TRUE)
  } else {
    big <- pnorm(b, log.p = TRUE); small <- pnorm(a, log.p = TRUE)
  }
  big + log(-expm1(small - big))
}
tnorm_test_ll <- function(x, m, s, a, b) {
  sum(dnorm(x, m, s, log = TRUE)) - length(x)*tnorm_test_logmass((a-m)/s, (b-m)/s)
}

test_that("mltnorm agrees with independent multistart likelihood oracles", {
  for (bounds in list(c(-1, 2), c(0, Inf), c(-Inf, 1))) {
    a <- bounds[1]; b <- bounds[2]
    x <- extraDistr::qtnorm(((1:150)-.5)/150, .4, .8, a, b)
    fit <- mltnorm(x, a = a, b = b)
    objective <- function(p) {
      ll <- tnorm_test_ll(x, p[1], exp(p[2]), a, b)
      if (is.finite(ll)) -ll else 1e100
    }
    oracle <- lapply(c(-1, 0, 1), function(m)
      optim(c(m, 0), objective, method = "Nelder-Mead",
            control = list(reltol = 1e-12, maxit = 3000)))
    expect_equal(-as.numeric(logLik(fit)), min(vapply(oracle, `[[`, 0., "value")), tolerance = 1e-8)
    expect_equal(sum(dml(x, fit, log = TRUE)), as.numeric(logLik(fit)), tolerance = 1e-9)
    expect_equal(attr(fit, "fixed"), c("a", "b"))
    expect_equal(attr(logLik(fit), "df"), 2L)
    for (m in c(.2, .4, .6)) {
      held <- mltnorm(x, a = a, b = b, mean = m)
      ref <- optimize(function(ls) -tnorm_test_ll(x, m, exp(ls), a, b), c(-5, 5), tol = 1e-10)
      expect_equal(as.numeric(logLik(held)), -ref$objective, tolerance = 1e-8)
      expect_equal(attr(held, "fixed"), c("mean", "a", "b"))
      expect_equal(attr(logLik(held), "df"), 1L)
    }
    for (s in c(.3, .8, 3)) {
      held <- mltnorm(x, a = a, b = b, sd = s)
      ref <- optimize(function(m) -tnorm_test_ll(x, m, s, a, b), c(-100, 100), tol = 1e-10)
      expect_equal(as.numeric(logLik(held)), -ref$objective, tolerance = 1e-8)
      expect_equal(attr(held, "fixed"), c("sd", "a", "b"))
    }
    both <- mltnorm(x, a = a, b = b, mean = .4, sd = .8)
    expect_equal(attr(both, "fixed"), c("mean", "sd", "a", "b"))
    expect_equal(attr(logLik(both), "df"), 0L)
    expect_equal(as.numeric(logLik(both)), tnorm_test_ll(x, .4, .8, a, b), tolerance = 1e-10)
  }
  x <- c(-2, -.3, 0, .5, 1, 3)
  fit <- mltnorm(x)
  expect_equal(as.numeric(fit[c("mean", "sd")]), as.numeric(mlnorm(x)))
  expect_equal(as.numeric(fit[c("a", "b")]), c(-Inf, Inf))
})

test_that("mltnorm validates known bounds and classifies escapes", {
  x <- c(.1, .3, .5, .7)
  for (ab in list(c(1, 0), c(0, 0), c(Inf, Inf), c(-Inf, -Inf), c(NA, 1)))
    expect_error(mltnorm(x, a = ab[1], b = ab[2]), "bounds")
  for (ab in list(c(.1, 1), c(0, .7), c(.2, 1), c(0, .6)))
    expect_error(mltnorm(x, a = ab[1], b = ab[2]), "strictly between")
  for (s in c(0, -1, Inf, NA)) expect_error(mltnorm(x, sd = s), "positive")
  expect_error(mltnorm(x, mean = Inf), "finite")
  expect_error(mltnorm(rep(.3, 3)), "unbounded")
  expect_error(mltnorm(x, reltol = 0), "positive")
  ends <- c(.01, .02, .98, .99)
  expect_error(mltnorm(ends, a = 0, b = 1), "truncated exponential")
  expect_error(mltnorm(ends, a = 0, b = 1, mean = .5), "uniform")
  over <- c(.01, .02, .03, 4)
  expect_error(mltnorm(over, a = 0), "mlexp\\(x - a\\)")
  expect_error(mltnorm(-over, b = 0), "mlexp\\(b - x\\)")
  expect_error(mltnorm(x, a = 0, b = 1, iterlim = 1), "converge")
  expect_equal(as.numeric(mltnorm(c(x, NA), na.rm = TRUE)), as.numeric(mltnorm(x)))
})
