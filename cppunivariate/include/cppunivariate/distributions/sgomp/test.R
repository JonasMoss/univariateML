test_that("sgomp agrees with accurate references and exponential member", {
  x <- c(0, 1e-4, .1, 1, 10)
  for (b in c(.2, 2)) for (eta in c(.01, 1, 100)) {
    expect_equal(dsgomp(x, b, eta), extraDistr::dsgomp(x, b, eta), tolerance = 1e-12)
    expect_equal(psgomp(x, b, eta), extraDistr::psgomp(x, b, eta), tolerance = 1e-12)
  }
  expect_equal(dsgomp(x, 2, 0, log = TRUE), dexp(x, 2, log = TRUE))
  expect_equal(psgomp(x, 2, 0), pexp(x, 2))
  expect_equal(qsgomp(c(.1, .5, .99), 2, 0), qexp(c(.1, .5, .99), 2))
})
test_that("sgomp extreme tails invert directly", {
  expect_equal(dsgomp(1000, 1, 1, log = TRUE), -1000 + log(2))
  expect_equal(psgomp(1000, 1, 1, FALSE, TRUE), -1000 + log(2))
  expect_equal(psgomp(1e-20, 1, 1, log.p = TRUE), log(1e-20) - 1)
  for (eta in c(0, 1e-10, 1, 100, 1e100)) for (tail in c(TRUE, FALSE)) {
    lp <- c(-1, -20, -700)
    q <- qsgomp(lp, 2, eta, tail, TRUE)
    expect_equal(psgomp(q, 2, eta, tail, TRUE), lp, tolerance = 1e-10)
  }
  expect_equal(psgomp(qsgomp(-1e300, 1, 1e308, TRUE, TRUE), 1, 1e308, TRUE, TRUE), -1e300, tolerance = 1e-12)
  expect_equal(psgomp(qsgomp(-1e300, 1, 1, FALSE, TRUE), 1, 1, FALSE, TRUE), -1e300, tolerance = 1e-12)
  expect_equal(qsgomp(-1000, 1e-300, 0, TRUE, TRUE), exp(-1000 - log(1e-300)), tolerance = 1e-12)
  expect_equal(qsgomp(c(0, 1), 1, 1), c(0, Inf))
  expect_equal(dsgomp(c(-Inf, Inf), 1, 1), c(0, 0))
  expect_equal(psgomp(c(-Inf, Inf), 1, 1), c(0, 1))
})
test_that("sgomp recycling missing values and RNG follow R", {
  expect_equal(dsgomp(c(.1, .2, .3), c(1, 2), 1), dsgomp(c(.1, .2, .3), c(1, 2, 1), 1))
  expect_identical(dsgomp(c(NA_real_, NaN), 1, 1), c(NA_real_, NaN))
  expect_length(dsgomp(numeric(), 1, 1), 0)
  expect_warning(expect_true(is.nan(dsgomp(1, -1, 1))), "NaNs")
  expect_warning(expect_true(is.nan(qsgomp(2, 1, 1))), "NaNs")
  set.seed(47); a <- rsgomp(20, 2, 3)
  set.seed(47); expect_identical(rsgomp(20, 2, 3), a)
  set.seed(47); u <- matrix(runif(40), 2)
  expect_equal(a, pmax(-log(u[1, ]), log(3) - log(-log(u[2, ]))) / 2)
  expect_length(rsgomp(1:3, 1, 1), 3)
  expect_length(rsgomp(0, 1, 1), 0)
})
test_that("sgomp faces and fixed subsets have consistent likelihood", {
  x <- c(0, 0, 0, 1, 10)
  f <- mlsgomp(x)
  expect_equal(unname(f['eta']), 0)
  expect_equal(unname(f['b']), 1 / mean(x))
  expect_equal(attr(f, 'logLik'), sum(dsgomp(x, f[1], f[2], TRUE)))
  expect_error(mlsgomp(rep(2, 3)), 'unbounded')
  expect_error(mlsgomp(c(0, 0), eta = 2), 'unbounded')
  expect_error(mlsgomp(c(-1, 1)), 'support')
  expect_error(mlsgomp(x, b = NA_real_), 'positive')
  expect_error(mlsgomp(x, iterlim = 1), 'converge')
  for (args in list(list(b = 1), list(eta = 2), list(b = 1, eta = 2))) {
    f <- do.call(mlsgomp, c(list(c(.3, .8, 1, 1.5, 2)), args))
    expect_setequal(attr(f, 'fixed'), names(args))
    expect_equal(attr(logLik(f), 'df'), 2L - length(args))
    expect_equal(attr(f, 'logLik'), sum(dsgomp(c(.3, .8, 1, 1.5, 2), f[1], f[2], TRUE)))
  }
})
test_that("sgomp profile agrees with slow multistart oracle", {
  for (x in list(c(.5, .8, 1, 1.2, 1.8), c(0, .1, 1, 2, 8), c(.1, 1, 3, 10))) {
    f <- mlsgomp(x)
    objective <- function(t) -sum(dsgomp(x, exp(t[1]), exp(t[2]), TRUE))
    fits <- lapply(c(-5, -1, 1, 4), function(k) optim(c(log(1 / mean(x)), k), objective,
                       method = 'Nelder-Mead', control = list(maxit = 3000, reltol = 1e-12)))
    oracle <- max(-vapply(fits, `[[`, numeric(1), 'value'), sum(dexp(x, 1 / mean(x), TRUE)))
    expect_equal(attr(f, 'logLik'), oracle, tolerance = 1e-7)
  }
})
test_that("sgomp fitted-model evaluation and utilities work", {
  set.seed(71); x <- rsgomp(60, 1, 3); f <- mlsgomp(x, b = 1)
  expect_equal(dml(x, f), dsgomp(x, f[1], f[2]))
  expect_equal(pml(qml(c(.1, .5, .9), f), f), c(.1, .5, .9), tolerance = 1e-10)
  set.seed(4); expect_length(rml(10, f), 10)
  expect_silent(plot(f))
  boot <- bootstrapml(f, reps = 3, reducer = identity)
  expect_equal(unname(boot['b', ]), rep(1, 3))
  expect_silent(model_select(x, models = c('sgomp', 'exp')))
})
test_that("sgomp held-coordinate fits agree with scalar oracles", {
  x <- c(0, .2, .7, 1, 2, 3)
  for (b in c(.1, 1, 10)) {
    f <- mlsgomp(x, b = b)
    oracle <- optimize(function(t) sum(dsgomp(x, b, exp(t), TRUE)), c(-30, 30), maximum = TRUE, tol = 1e-11)
    expect_equal(attr(f, 'logLik'), max(oracle$objective, sum(dsgomp(x, b, 0, TRUE))), tolerance = 1e-8)
  }
  for (eta in c(0, .1, 10)) {
    f <- mlsgomp(x, eta = eta)
    oracle <- optimize(function(t) sum(dsgomp(x, exp(t), eta, TRUE)), c(-15, 15), maximum = TRUE, tol = 1e-11)
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
