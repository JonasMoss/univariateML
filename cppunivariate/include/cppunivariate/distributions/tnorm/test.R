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
  fit <- mltnorm(x, a = -Inf, b = Inf)
  expect_equal(as.numeric(fit[c("mean", "sd")]), as.numeric(mlnorm(x)))
  expect_equal(as.numeric(fit[c("a", "b")]), c(-Inf, Inf))
})

test_that("mltnorm validates known bounds and classifies escapes", {
  x <- c(.1, .3, .5, .7)
  for (ab in list(c(1, 0), c(0, 0), c(Inf, Inf), c(-Inf, -Inf), c(NA, 1)))
    expect_error(mltnorm(x, a = ab[1], b = ab[2]), "bounds")
  for (ab in list(c(.2, 1), c(0, .6)))
    expect_error(mltnorm(x, a = ab[1], b = ab[2]), "closed bounds")
  for (s in c(0, -1, Inf, NA)) expect_error(mltnorm(x, sd = s, a = -Inf, b = Inf), "positive")
  expect_error(mltnorm(x, mean = Inf, a = -Inf, b = Inf), "finite")
  expect_error(mltnorm(rep(.3, 3), a = -Inf, b = Inf), "unbounded")
  expect_error(mltnorm(x, reltol = 0, a = -Inf, b = Inf), "positive")
  ends <- c(.01, .02, .98, .99)
  expect_error(mltnorm(ends, a = 0, b = 1), "truncated exponential")
  expect_error(mltnorm(ends, a = 0, b = 1, mean = .5), "uniform")
  over <- c(.01, .02, .03, 4)
  expect_error(mltnorm(over, a = 0, b = Inf), "mlexp\\(x - a\\)")
  expect_error(mltnorm(-over, b = 0, a = -Inf), "mlexp\\(b - x\\)")
  expect_error(mltnorm(x, a = 0, b = 1, iterlim = 1), "converge")
  expect_equal(as.numeric(mltnorm(c(x, NA), na.rm = TRUE, a = -Inf, b = Inf)), as.numeric(mltnorm(x, a = -Inf, b = Inf)))
})

test_that("runtime truncated-normal repairs agree with stable normal differences", {
  fixes <- univariateML:::univariateML_reference_fixes
  for (type in c("d", "p", "q")) {
    fix <- fixes[[paste0("extraDistr::", type, "tnorm")]]
    expect_true(fix$broken())
    ref <- getExportedValue("extraDistr", paste0(type, "tnorm"))
    for (logs in c(FALSE, TRUE)) {
      if (type == "d") {
        expect_equal(fix$replacement(c(-.5,.2,1), c(0,.1,.2), 1, -1, 2, log = logs),
                     ref(c(-.5,.2,1), c(0,.1,.2), 1, -1, 2, log = logs), tolerance = 1e-12)
        x <- c(10.01,10.05,10.1)
        expected <- dnorm(x, log = TRUE) - pnorm(10, lower.tail = FALSE, log.p = TRUE)
        if (!logs) expected <- exp(expected)
        expect_equal(fix$replacement(x, a = 10, log = logs), expected, tolerance = 1e-12)
      } else for (lower in c(FALSE, TRUE)) {
        x <- if (type == "p") c(-.5,.2,1) else c(.1,.5,.9)
        if (type == "q" && logs) x <- log(x)
        expect_equal(fix$replacement(x, c(0,.1,.2), 1, -1, 2, lower.tail = lower, log.p = logs),
                     ref(x, c(0,.1,.2), 1, -1, 2, lower.tail = lower, log.p = logs),
                     tolerance = 1e-12)
        if (type == "q") {
          u <- c(.1,.5,.9)
          tail <- if (lower) 1-u else u
          expected <- qnorm(pnorm(40, lower.tail = FALSE, log.p = TRUE) + log(tail),
                            lower.tail = FALSE, log.p = TRUE)
          actual <- fix$replacement(if (logs) log(u) else u, a = 40, b = 41,
                                    lower.tail = lower, log.p = logs)
        } else {
          x <- c(40.001,40.01,40.05)
          lp <- pnorm(x, lower.tail = FALSE, log.p = TRUE) -
            pnorm(40, lower.tail = FALSE, log.p = TRUE)
          expected <- if (lower) -expm1(lp) else exp(lp)
          if (logs) expected <- log(expected)
          actual <- fix$replacement(x, a = 40, b = 41, lower.tail = lower, log.p = logs)
        }
        expect_equal(actual, expected, tolerance = 1e-12)
      }
    }
  }
})
