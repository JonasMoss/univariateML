trgamma_oracle <- function(x) {
  nll <- function(p) {
    v <- -sum(actuar::dtrgamma(x, shape1 = exp(p[1]), shape2 = exp(p[2]),
                               scale = exp(p[3]), log = TRUE))
    if (is.finite(v)) v else 1e100
  }
  y <- log(x)
  s <- sqrt(mean((y - mean(y))^2))
  best <- list(value = Inf)
  for (a in c(.05, .3, 1, 3, 10, 50, 300)) for (m in c(.5, 1, 2)) {
    tau <- m * sqrt(trigamma(a)) / s
    start <- c(log(a), log(tau), mean(y) - digamma(a) / tau)
    if (nll(start) >= 1e90) next
    o <- optim(start, nll, method = "Nelder-Mead", control = list(maxit = 4000, reltol = 1e-12))
    o <- optim(o$par, nll, method = "BFGS", control = list(maxit = 2000, reltol = 1e-14))
    if (o$value < best$value) best <- o
  }
  ll <- -best$value
  faces <- c(lnorm = attr(mllnorm(x), "logLik"), power = attr(mlpower(x, epsilon = 0), "logLik"))
  face <- if (max(faces) >= ll - 1e-8 * length(x)) names(which.max(faces)) else "interior"
  list(ll = max(ll, faces), face = face, estimates = exp(best$par))
}

test_that("mltrgamma agrees with a multistart actuar oracle and its faces", {
  cases <- list(
    list(n = 30, a = 2, t = 1.5, s = 2), list(n = 200, a = 2, t = 1.5, s = 2),
    list(n = 100, a = .4, t = 3, s = 1), list(n = 100, a = 1, t = .8, s = 5),
    # Near the lognormal limit: the fit is interior or escapes to mllnorm.
    list(n = 200, a = 60, t = .25, s = 1e-5), list(n = 400, a = 200, t = .15, s = 1e-12)
  )
  for (case in cases) for (seed in 1:3) {
    set.seed(seed)
    x <- actuar::rtrgamma(case$n, shape1 = case$a, shape2 = case$t, scale = case$s)
    oracle <- suppressWarnings(trgamma_oracle(x))
    fit <- tryCatch(mltrgamma(x), error = function(e) e)
    info <- paste(case$n, case$a, seed)
    if (oracle$face == "interior") {
      expect_false(inherits(fit, "error"), info = info)
      if (inherits(fit, "error")) next
      expect_gte(attr(fit, "logLik"), oracle$ll - 1e-7 * case$n)
      expect_equal(attr(fit, "logLik"), oracle$ll, tolerance = 1e-6, info = info)
      if (case$a < 50) expect_equal(as.numeric(fit), oracle$estimates, tolerance = 1e-3, info = info)
      expect_equal(sum(actuar::dtrgamma(x, fit[1], fit[2], scale = fit[3], log = TRUE)),
                   attr(fit, "logLik"), tolerance = 1e-10)
    } else {
      expect_error(mltrgamma(x), paste0("Use `ml", oracle$face, "`"), info = info)
    }
  }
})

test_that("mltrgamma faces are triggered by data from the limit models", {
  z <- qnorm(((1:400) - .5) / 400)
  expect_error(mltrgamma(exp(z + .05 * (z^2 - 1))), "Use `mllnorm`")
  expect_error(mltrgamma(3 * (((1:400) - .5) / 400)^(1 / 2.5)), "Use `mlpower`")
  expect_error(mltrgamma(c(1, 2, 3)), "Use `mlpower`")
  # Lognormal samples escape exactly when log(x) is right-skewed (here).
  for (seed in 1:6) {
    set.seed(seed)
    x <- rlnorm(300)
    k3 <- mean((log(x) - mean(log(x)))^3)
    if (k3 > 0) expect_error(mltrgamma(x), "Use `mllnorm`")
    else expect_gt(attr(mltrgamma(x), "logLik"), attr(mllnorm(x), "logLik"))
  }
  # Left-skewed log data very close to lognormal: interior, scale underflows.
  z <- qnorm(((1:4000) - .5) / 4000)
  x <- exp(z - .001 * (z^2 - 1))
  expect_error(mltrgamma(x), "not representable.*Use `mllnorm`")
})

test_that("mltrgamma fixed shapes reduce to gamma, Weibull and exact profiles", {
  set.seed(3)
  x <- actuar::rtrgamma(150, shape1 = 2, shape2 = 1.5, scale = 2)
  n <- length(x)
  for (tau in c(.4, 1, 2.5)) {
    fit <- mltrgamma(x, shape2 = tau)
    g <- mlgamma(x^tau, abstol = 1e-12)
    expect_equal(unname(fit["shape2"]), tau)
    expect_equal(unname(fit["shape1"]), unname(g["shape"]), tolerance = 1e-8)
    expect_equal(unname(fit["scale"]), unname((1 / g["rate"])^(1 / tau)), tolerance = 1e-8)
    expect_equal(attr(fit, "logLik"),
                 attr(g, "logLik") + n * log(tau) + (tau - 1) * sum(log(x)), tolerance = 1e-10)
    expect_identical(attr(fit, "fixed"), "shape2")
    expect_equal(attr(logLik(fit), "df"), 2L)
  }
  w <- mltrgamma(x, shape1 = 1)
  ref <- mlweibull(x, reltol = 1e-12)
  expect_equal(unname(w[2:3]), unname(ref[1:2]), tolerance = 1e-8)
  expect_equal(attr(w, "logLik"), attr(ref, "logLik"), tolerance = 1e-10)
  for (a in c(.2, 3, 30)) {
    fit <- mltrgamma(x, shape1 = a)
    nll <- function(p) {
      value <- suppressWarnings(-sum(actuar::dtrgamma(x, a, exp(p[1]), scale = exp(p[2]), log = TRUE)))
      if (is.finite(value)) value else 1e100
    }
    o <- optim(unname(c(log(fit[2]) + .3, log(fit[3]) - .3)), nll,
               method = "Nelder-Mead", control = list(reltol = 1e-14, maxit = 5000))
    o <- optim(o$par, nll, method = "BFGS", control = list(reltol = 1e-14, maxit = 2000))
    expect_equal(attr(fit, "logLik"), -o$value, tolerance = 1e-9)
    expect_equal(unname(fit[2:3]), unname(exp(o$par)), tolerance = 1e-5)
    both <- mltrgamma(x, shape1 = a, shape2 = 1.2)
    o <- optimize(function(s) -sum(actuar::dtrgamma(x, a, 1.2, scale = exp(s), log = TRUE)),
                  c(-20, 20), tol = 1e-12)
    expect_equal(attr(both, "logLik"), -o$objective, tolerance = 1e-10)
    expect_identical(attr(both, "fixed"), c("shape1", "shape2"))
    expect_equal(attr(logLik(both), "df"), 1L)
  }
  expect_identical(attr(mltrgamma(x), "fixed"), character())
  for (bad in c(0, -1, Inf, NaN)) {
    expect_error(mltrgamma(x, shape1 = bad), "positive finite")
    expect_error(mltrgamma(x, shape2 = bad), "positive finite")
  }
  expect_error(mltrgamma(c(2, 2, 2)), "two distinct")
})

test_that("mltrgamma and mlinvtrgamma are reciprocal fits", {
  set.seed(7)
  x <- actuar::rtrgamma(250, shape1 = .8, shape2 = 2, scale = 3)
  a <- mltrgamma(x)
  b <- mlinvtrgamma(1 / x)
  expect_equal(unname(b[1:2]), unname(a[1:2]), tolerance = 1e-8)
  expect_equal(unname(b[3]), unname(1 / a[3]), tolerance = 1e-8)
  expect_equal(attr(b, "logLik"), attr(a, "logLik") + 2 * sum(log(x)), tolerance = 1e-12)
  expect_equal(actuar::dinvtrgamma(1 / x[1:5], 2, 1.5, scale = 1 / 3, log = TRUE),
               actuar::dtrgamma(x[1:5], 2, 1.5, scale = 3, log = TRUE) + 2 * log(x[1:5]))
})

test_that("runtime gamma-transform repairs detect defects and retain ordinary values", {
  fixes <- univariateML:::univariateML_reference_fixes
  family <- "trgamma"
  for (type in c("d", "p", "q", "r")) {
    fix <- fixes[[paste0("actuar::", type, family)]]
    expect_true(fix$broken())
    ref <- getExportedValue("actuar", paste0(type, family))
    if (type == "r") {
      set.seed(21); actual <- fix$replacement(20, 2, 1.5, scale = 3)
      set.seed(21); expected <- ref(20, 2, 1.5, scale = 3)
      expect_equal(actual, expected, tolerance = 1e-12)
      set.seed(21); seed <- .Random.seed
      expect_true(fix$broken())
      expect_identical(.Random.seed, seed)
      set.seed(21); actual <- fix$replacement(20, c(2,3), c(1.5,2), scale = c(3,4))
      set.seed(21); expected <- ref(20, c(2,3), c(1.5,2), scale = c(3,4))
      expect_equal(actual, expected, tolerance = 1e-12)
      # Independent small-shape gamma asymptote: log(G) = (log(U)+lgamma(1+a))/a.
      set.seed(21); u <- runif(20)
      expected <- exp(1 * (log(u) + lgamma(1+1e-6))/(1e-6*1e6))
      set.seed(21); actual <- fix$replacement(20, 1e-6, 1e6)
      expect_equal(actual, expected, tolerance = 1e-12)
    } else if (type == "d") {
      for (logs in c(FALSE, TRUE))
        expect_equal(fix$replacement(c(.5, 1, 3), c(2,3,4), 1.5, scale = 3, log = logs),
                     ref(c(.5,1,3), c(2,3,4), 1.5, scale = 3, log = logs), tolerance = 1e-12)
      expect_equal(fix$replacement(1, 1e12, 1, scale = 1e-12, log = TRUE),
                   dgamma(1, 1e12, scale = 1e-12, log = TRUE), tolerance = 1e-12)
    } else {
      for (lower in c(FALSE, TRUE)) for (logs in c(FALSE, TRUE)) {
        x <- if (type == "q") c(.1,.4,.9) else c(.5,1,3)
        if (type == "q" && logs) x <- log(x)
        expect_equal(fix$replacement(x, c(2,3,4), 1.5, scale = 3,
                                     lower.tail = lower, log.p = logs),
                     ref(x, c(2,3,4), 1.5, scale = 3, lower.tail = lower, log.p = logs),
                     tolerance = 1e-12)
        # Independent gamma small-argument asymptote, not the repair's anchor.
        if (type == "q") {
          u <- c(.2,.5,.8)
          gprob <- if (FALSE != lower) u else 1-u
          expected <- exp(1 * (log(gprob)+lgamma(1+1e-6))/(1e-6*1e6))
          actual <- fix$replacement(if (logs) log(u) else u, 1e-6, 1e6,
                                    lower.tail = lower, log.p = logs)
        } else {
          x <- exp(1 * log(c(.2,.5,.8)))
          lp <- 1 * log(x) - lgamma(1+1e-6)
          expected <- if (FALSE != lower) exp(lp) else -expm1(lp)
          if (logs) expected <- log(expected)
          actual <- fix$replacement(x, 1e-6, 1e6, lower.tail = lower, log.p = logs)
        }
        expect_equal(actual, expected, tolerance = 1e-12)
      }
    }
  }
})

test_that("trgamma log quantiles and large power transforms remain representable", {
  f <- univariateML:::univariateML_reference_fixes[["actuar::qtrgamma"]]$replacement
  # At shape2=1, compare directly to R's stable gamma quantile.
  expect_equal(f(-1000, 2, 1, log.p = TRUE),
               qgamma(-1000, 2, log.p = TRUE), tolerance = 1e-12)
  a <- 100; tau <- .0067; scale <- exp(-1 * log(a)/tau)
  u <- c(.01,.5,.99)
  expected <- exp(log(scale) + log(qgamma(u, a, lower.tail = TRUE))/tau)
  expect_equal(f(u, a, tau, scale = scale), expected, tolerance = 1e-12)
  rfix <- univariateML:::univariateML_reference_fixes[["actuar::rtrgamma"]]
  if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
    rm(".Random.seed", envir = .GlobalEnv)
  expect_true(rfix$broken())
  expect_false(exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
})
