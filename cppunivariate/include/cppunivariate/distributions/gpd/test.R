test_that("mlgpd agrees with multistart constrained density oracles", {
  for (shape in c(.3, 0, -.3, -.8)) {
    x <- extraDistr::qgpd(((1:200)-.5)/200, sigma = 2, xi = shape)
    fit <- mlgpd(x)
    objective <- function(p) {
      if (p[2] < -.5) return(1e100)
      ans <- -sum(extraDistr::dgpd(x, sigma = exp(p[1]), xi = p[2], log = TRUE))
      if (is.finite(ans)) ans else 1e100
    }
    fits <- lapply(c(-.5, -.2, .01, .3, 1), function(k)
      optim(c(log(2), k), objective, method = "Nelder-Mead",
            control = list(reltol = 1e-12, maxit = 2000)))
    best <- min(vapply(fits, function(z) z$value, numeric(1)))
    expect_equal(-as.numeric(logLik(fit)), best, tolerance = 1e-7)
    expect_equal(attr(fit, "fixed"), "mu")
    expect_equal(attr(logLik(fit), "df"), 2L)
    if (shape == -.8) expect_equal(unname(fit[["xi"]]), -.5)
    shifted <- mlgpd(x + 10, mu = 10)
    expect_equal(unname(shifted[["mu"]]), 10)
    expect_equal(unname(shifted[c("sigma", "xi")]), unname(fit[c("sigma", "xi")]), tolerance = 1e-8)
    for (k in c(-.5, -.3, 0, .3, 2)) {
      held <- mlgpd(x + 10, mu = 10, xi = k)
      lower <- if (k < 0) log(-k*max(x)) else -30
      oracle <- optimize(function(s) -sum(extraDistr::dgpd(x, sigma = exp(s), xi = k, log = TRUE)), c(lower + 1e-10, 20), tol = 1e-11)
      expect_equal(unname(held[["xi"]]), k)
      expect_equal(attr(held, "fixed"), c("mu", "xi"))
      expect_equal(attr(logLik(held), "df"), 1L)
      expect_equal(-as.numeric(logLik(held)), oracle$objective, tolerance = 1e-8)
      if (k == 0) expect_equal(unname(held[["sigma"]]), 1/unname(mlexp(x)[["rate"]]))
    }
  }
})
test_that("mlgpd rejects invalid thresholds, shapes and observations", {
  expect_error(mlgpd(c(1, 2, 3), mu = 1), "strictly above")
  expect_error(mlgpd(c(-1, 2, 3)), "strictly above")
  expect_error(mlgpd(c(1, 2, 3), mu = Inf), "finite")
  for (k in c(-.51, Inf, NaN)) expect_error(mlgpd(c(1,2,3), xi = k), "at least -0.5")
  expect_error(mlgpd(c(1, 2, 3), iterlim = 1), "converge")
  expect_error(mlgpd(c(1, 2, NA)))
})
