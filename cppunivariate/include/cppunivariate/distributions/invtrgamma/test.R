test_that("mlinvtrgamma agrees with a multistart actuar oracle", {
  nll_for <- function(x) function(p) {
    v <- suppressWarnings(-sum(actuar::dinvtrgamma(x, shape1 = exp(p[1]), shape2 = exp(p[2]),
                                  scale = exp(p[3]), log = TRUE)))
    if (is.finite(v)) v else 1e100
  }
  for (generating_shape in c(2, 60)) for (seed in 1:4) {
    set.seed(seed)
    x <- actuar::rinvtrgamma(200, shape1 = generating_shape,
                             shape2 = if (generating_shape == 2) 1.5 else .25, scale = .5)
    nll <- nll_for(x)
    y <- -log(x)
    s <- sqrt(mean((y - mean(y))^2))
    best <- list(value = Inf)
    for (a in c(.3, 1, 3, 10, 60, 300)) for (m in c(.5, 1, 2)) {
      tau <- m * sqrt(trigamma(a)) / s
      start <- c(log(a), log(tau), -(mean(y) - digamma(a) / tau))
      o <- optim(start, nll, method = "Nelder-Mead", control = list(maxit = 4000, reltol = 1e-12))
      o <- optim(o$par, nll, method = "BFGS", control = list(maxit = 2000, reltol = 1e-14))
      if (o$value < best$value) best <- o
    }
    faces <- c(attr(mllnorm(x), "logLik"), attr(mlpareto(x), "logLik"))
    if (max(faces) >= -best$value - 1e-8 * length(x)) {
      expect_error(mlinvtrgamma(x), if (which.max(faces) == 1) "Use `mllnorm`" else "Use `mlpareto`")
      next
    }
    fit <- tryCatch(mlinvtrgamma(x), error = function(e) e)
    if (inherits(fit, "error")) {
      expect_match(conditionMessage(fit), "not representable")
      next
    }
    expect_equal(attr(fit, "logLik"), -best$value, tolerance = 1e-8)
    if (generating_shape == 2) expect_equal(as.numeric(fit), exp(best$par), tolerance = 1e-3)
    expect_equal(sum(actuar::dinvtrgamma(x, fit[1], fit[2], scale = fit[3], log = TRUE)),
                 attr(fit, "logLik"), tolerance = 1e-10)
    forward <- mltrgamma(1 / x)
    expect_equal(unname(fit[1:2]), unname(forward[1:2]), tolerance = 1e-8)
    expect_equal(unname(fit[3]), unname(1 / forward[3]), tolerance = 1e-8)
    expect_equal(attr(fit, "logLik"), attr(forward, "logLik") - 2 * sum(log(x)), tolerance = 1e-12)
  }
})

test_that("mlinvtrgamma mirrors the lognormal and Pareto faces", {
  p <- ((1:400) - .5) / 400
  z <- qnorm(p)
  expect_error(mlinvtrgamma(exp(-(z + .05 * (z^2 - 1)))), "Use `mllnorm`")
  expect_error(mlinvtrgamma(2 * (1 - p)^(-1 / 1.7)), "Use `mlpareto`")
  z <- qnorm(((1:4000) - .5) / 4000)
  expect_error(mlinvtrgamma(exp(-(z - .001 * (z^2 - 1)))), "not representable.*Use `mllnorm`")
})

test_that("mlinvtrgamma fixed shapes reduce to inverse gamma and inverse Weibull", {
  set.seed(11)
  x <- actuar::rinvtrgamma(150, shape1 = 2, shape2 = 1.5, scale = .5)
  g <- mlinvtrgamma(x, shape2 = 1)
  ref <- mlinvgamma(x, abstol = 1e-12)
  expect_equal(unname(g[c(1, 3)]), unname(ref[1:2]), tolerance = 1e-8)
  expect_equal(attr(g, "logLik"), attr(ref, "logLik"), tolerance = 1e-10)
  expect_identical(attr(g, "fixed"), "shape2")
  w <- mlinvtrgamma(x, shape1 = 1)
  ref <- mlinvweibull(x, reltol = 1e-12)
  expect_equal(unname(w[2:3]), unname(c(ref[1], 1 / ref[2])), tolerance = 1e-8)
  expect_equal(attr(w, "logLik"), attr(ref, "logLik"), tolerance = 1e-10)
  both <- mlinvtrgamma(x, shape1 = 2, shape2 = 1.5)
  o <- optimize(function(s) -sum(actuar::dinvtrgamma(x, 2, 1.5, scale = exp(s), log = TRUE)),
                c(-20, 20), tol = 1e-12)
  expect_equal(attr(both, "logLik"), -o$objective, tolerance = 1e-10)
  expect_equal(attr(logLik(both), "df"), 1L)
  expect_error(mlinvtrgamma(x, shape2 = -1), "positive finite")
})

test_that("runtime gamma-transform repairs detect defects and retain ordinary values", {
  fixes <- univariateML:::univariateML_reference_fixes
  family <- "invtrgamma"
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
      expected <- exp(-1 * (log1p(-u) + lgamma(1+1e-6))/(1e-6*1e6))
      set.seed(21); actual <- fix$replacement(20, 1e-6, 1e6)
      expect_equal(actual, expected, tolerance = 1e-12)
    } else if (type == "d") {
      for (logs in c(FALSE, TRUE))
        expect_equal(fix$replacement(c(.5, 1, 3), c(2,3,4), 1.5, scale = 3, log = logs),
                     ref(c(.5,1,3), c(2,3,4), 1.5, scale = 3, log = logs), tolerance = 1e-12)
      expect_equal(fix$replacement(1, 1e12, 1, scale = 1e12, log = TRUE),
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
          gprob <- if (TRUE != lower) u else 1-u
          expected <- exp(-1 * (log(gprob)+lgamma(1+1e-6))/(1e-6*1e6))
          actual <- fix$replacement(if (logs) log(u) else u, 1e-6, 1e6,
                                    lower.tail = lower, log.p = logs)
        } else {
          x <- exp(-1 * log(c(.2,.5,.8)))
          lp <- -1 * log(x) - lgamma(1+1e-6)
          expected <- if (TRUE != lower) exp(lp) else -expm1(lp)
          if (logs) expected <- log(expected)
          actual <- fix$replacement(x, 1e-6, 1e6, lower.tail = lower, log.p = logs)
        }
        expect_equal(actual, expected, tolerance = 1e-12)
      }
    }
  }
})

test_that("invtrgamma log quantiles and large power transforms remain representable", {
  f <- univariateML:::univariateML_reference_fixes[["actuar::qinvtrgamma"]]$replacement
  # At shape2=1, compare directly to R's stable gamma quantile.
  expect_equal(f(-1000, 2, 1, log.p = TRUE),
               1/qgamma(-1000, 2, lower.tail = FALSE, log.p = TRUE), tolerance = 1e-12)
  a <- 100; tau <- .0065; scale <- exp(1 * log(a)/tau)
  u <- c(.01,.5,.99)
  expected <- exp(log(scale) - log(qgamma(u, a, lower.tail = FALSE))/tau)
  expect_equal(f(u, a, tau, scale = scale), expected, tolerance = 1e-12)
  rfix <- univariateML:::univariateML_reference_fixes[["actuar::rinvtrgamma"]]
  if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
    rm(".Random.seed", envir = .GlobalEnv)
  expect_true(rfix$broken())
  expect_false(exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
})
