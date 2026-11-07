test_that("sgomp includes the exponential member", {
  x <- c(0, 1e-4, .1, 1, 10)
  expect_equal(dsgomp(x = x, b = 2, eta = 0, log = TRUE), dexp(x, 2, log = TRUE))
  expect_equal(psgomp(q = x, b = 2, eta = 0), pexp(x, 2))
  expect_equal(qsgomp(p = c(0.1, 0.5, 0.99), b = 2, eta = 0), qexp(c(.1, .5, .99), 2))
})
test_that("sgomp extreme tails invert directly", {
  expect_equal(dsgomp(x = 1000, b = 1, eta = 1, log = TRUE), -1000 + log(2))
  expect_equal(psgomp(q = 1000, b = 1, eta = 1, lower.tail = FALSE, log.p = TRUE), -1000 + log(2))
  expect_equal(psgomp(q = 1e-20, b = 1, eta = 1, log.p = TRUE), log(1e-20) - 1)
  for (eta in c(0, 1e-10, 1, 100, 1e100)) for (tail in c(TRUE, FALSE)) {
    lp <- c(-1, -20, -700)
    q <- qsgomp(p = lp, b = 2, eta = eta, lower.tail = tail, log.p = TRUE)
    expect_equal(psgomp(q = q, b = 2, eta = eta, lower.tail = tail, log.p = TRUE), lp, tolerance = 1e-10)
  }
  expect_equal(psgomp(qsgomp(p = -1e+300, b = 1, eta = 1e+308, lower.tail = TRUE, log.p = TRUE), 1, 1e308, TRUE, TRUE), -1e300, tolerance = 1e-12)
  expect_equal(psgomp(qsgomp(p = -1e+300, b = 1, eta = 1, lower.tail = FALSE, log.p = TRUE), 1, 1, FALSE, TRUE), -1e300, tolerance = 1e-12)
  expect_equal(qsgomp(p = -1000, b = 1e-300, eta = 0, lower.tail = TRUE, log.p = TRUE), exp(-1000 - log(1e-300)), tolerance = 1e-12)
  expect_equal(qsgomp(p = c(0, 1), b = 1, eta = 1), c(0, Inf))
  expect_equal(dsgomp(x = c(-Inf, Inf), b = 1, eta = 1), c(0, 0))
  expect_equal(psgomp(q = c(-Inf, Inf), b = 1, eta = 1), c(0, 1))
})
test_that("sgomp recycling missing values and RNG follow R", {
  expect_equal(dsgomp(c(.1,.2,.3), c(1,2), 1), dsgomp(c(.1,.2,.3), c(1,2,1), 1))

  expect_identical(dsgomp(x = c(NA_real_, NaN), b = 1, eta = 1), c(NA_real_, NaN))
  expect_length(dsgomp(x = numeric(), b = 1, eta = 1), 0)
  expect_warning(expect_true(is.nan(dsgomp(x = 1, b = -1, eta = 1))), "NaNs")
  expect_warning(expect_true(is.nan(qsgomp(p = 2, b = 1, eta = 1))), "NaNs")
  set.seed(47); a <- rsgomp(n = 20, b = 2, eta = 3)
  set.seed(47); expect_identical(rsgomp(n = 20, b = 2, eta = 3), a)
  set.seed(47); u <- matrix(runif(40), 2)
  expect_equal(a, pmax(-log(u[1, ]), log(3) - log(-log(u[2, ]))) / 2)
  expect_length(rsgomp(n = 1:3, b = 1, eta = 1), 3)
  expect_length(rsgomp(n = 0, b = 1, eta = 1), 0)
})
test_that("sgomp faces and fixed subsets have consistent likelihood", {
  x <- c(0, 0, 0, 1, 10)
  f <- mlsgomp(x)
  expect_equal(unname(f['eta']), 0)
  expect_equal(unname(f['b']), 1 / mean(x))
  expect_equal(attr(f, 'logLik'), sum(dsgomp(x = x, b = f[1], eta = f[2], log = TRUE)))
  expect_error(mlsgomp(rep(2, 3)), 'unbounded')
  expect_error(mlsgomp(c(0, 0), eta = 2), 'unbounded')
  expect_error(mlsgomp(c(-1, 1)), 'support')
  expect_error(mlsgomp(x, b = NA_real_), 'positive')
  expect_error(mlsgomp(x, iterlim = 1), 'converge')
  for (args in list(list(b = 1), list(eta = 2), list(b = 1, eta = 2))) {
    f <- do.call(mlsgomp, c(list(c(.3, .8, 1, 1.5, 2)), args))
    expect_setequal(attr(f, 'fixed'), names(args))
    expect_equal(attr(logLik(f), 'df'), 2L - length(args))
    expect_equal(attr(f, 'logLik'), sum(dsgomp(x = c(0.3, 0.8, 1, 1.5, 2), b = f[1], eta = f[2], log = TRUE)))
  }
})
test_that("sgomp profile agrees with slow multistart oracle", {
  skip_unless_tier("full")
  for (x in list(c(.5, .8, 1, 1.2, 1.8), c(0, .1, 1, 2, 8), c(.1, 1, 3, 10))) {
    f <- mlsgomp(x)
    objective <- function(t) -sum(dsgomp(x = x, b = exp(t[1]), eta = exp(t[2]), log = TRUE))
    fits <- lapply(c(-5, -1, 1, 4), function(k) optim(c(log(1 / mean(x)), k), objective,
                       method = 'Nelder-Mead', control = list(maxit = 3000, reltol = 1e-12)))
    oracle <- max(-vapply(fits, `[[`, numeric(1), 'value'), sum(dexp(x, 1 / mean(x), TRUE)))
    expect_equal(attr(f, 'logLik'), oracle, tolerance = 1e-7)
  }
})
test_that("sgomp fitted-model evaluation and utilities work", {
  set.seed(71); x <- rsgomp(n = 60, b = 1, eta = 3); f <- mlsgomp(x, b = 1)

  expect_equal(pml(qml(c(.1, .5, .9), f), f), c(.1, .5, .9), tolerance = 1e-10)
  set.seed(4); expect_length(rml(10, f), 10)
  expect_silent(plot(f))
  boot <- bootstrapml(f, reps = 3, reducer = identity)
  expect_equal(unname(boot['b', ]), rep(1, 3))
  expect_silent(model_select(x, models = c('sgomp', 'exp')))
})
test_that("sgomp held-coordinate fits agree with scalar oracles", {
  skip_unless_tier("full")
  x <- c(0, .2, .7, 1, 2, 3)
  for (b in c(.1, 1, 10)) {
    f <- mlsgomp(x, b = b)
    oracle <- optimize(function(t) sum(dsgomp(x = x, b = b, eta = exp(t), log = TRUE)), c(-30, 30), maximum = TRUE, tol = 1e-11)
    expect_equal(attr(f, 'logLik'), max(oracle$objective, sum(dsgomp(x = x, b = b, eta = 0, log = TRUE))), tolerance = 1e-8)
  }
  for (eta in c(0, .1, 10)) {
    f <- mlsgomp(x, eta = eta)
    oracle <- optimize(function(t) sum(dsgomp(x = x, b = exp(t), eta = eta, log = TRUE)), c(-15, 15), maximum = TRUE, tol = 1e-11)
    expect_equal(attr(f, 'logLik'), oracle$objective, tolerance = 1e-8)
  }
  f <- mlsgomp(c(0, 0), b = 2)
  expect_equal(as.numeric(f), c(2, 0))
  expect_equal(attr(f, 'logLik'), 2 * log(2))
})

test_that("sgomp: P3 unfinished optimizer stops", {
  g <- jsonlite::fromJSON(file.path(Sys.getenv("UMVL_DISTRIBUTIONS_DIR"), "sgomp", "golden.json"))
  x <- g$data
  expect_error(mlsgomp(x, iterlim=1, reltol=1e-10),
               "[Ff]ail|converge|unresolved|exhausted|score check")
})

test_that("sgomp shape gain survives huge common likelihood", {
  for (large in c(1e17,1e100,1e300)) {
    x <- c(1e-300,1,large)
    f <- mlsgomp(x,b=1)
    eta <- unname(f["eta"])
    e1 <- exp(-1); linear <- 3*e1-e1^2
    oracle <- 2*(1-2*e1)/(linear+sqrt(linear^2+4*(1-e1^2)*(1-2*e1)))
    expect_equal(eta,oracle,tolerance=1e-9)
    a <- -expm1(-x); e <- exp(-x)
    expect_lt(abs(sum(a/(1+eta*a)-e)),1e-10)
    expect_gt(sum(log1p(eta*a)-eta*e),.028)
  }
})

test_that("sgomp positive face score overrides a tiny likelihood tie", {
  x <- log(2)+1e-6
  f <- mlsgomp(x,b=1)
  e <- exp(-x); a <- -expm1(-x); eta <- unname(f["eta"])
  expect_gt(eta,0)
  expect_equal(eta,(a-e)/(a*e),tolerance=1e-9)
  gain <- log1p(eta*a)-eta*e
  expect_gt(gain,0)
  expect_lt(gain,1e-8)
})

test_that("shifted Gompertz held-shape rate agrees with a scalar oracle", {
  x <- c(0,.1,.3,2,9)
  for(eta in c(.01,2,1e10)) {
    f <- mlsgomp(x,eta=eta)
    density <- getFromNamespace("dsgomp","univariateML")
    objective <- function(z) -sum(density(x,exp(z),eta,log=TRUE))
    oracle <- optimize(objective,c(-10,10),tol=1e-11)
    expect_equal(attr(f,"logLik"),-oracle$objective,tolerance=1e-9)
    expect_identical(unname(f["eta"]),eta)
  }
})

test_that("shifted Gompertz unfinished bracket preserves a finite witness", {
  e <- tryCatch(mlsgomp(c(1,1.01),iterlim=1),error=identity)
  expect_s3_class(e,"error")
  expect_identical(e$kind,"numerical")
  expect_true(is.finite(e$loglik_lower))
})
