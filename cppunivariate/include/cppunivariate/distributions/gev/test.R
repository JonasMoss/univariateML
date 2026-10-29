test_that("mlgev matches constrained reference density oracles", {
  set.seed(20261029)
  for (shape in c(-.9, -.3, 0, .3, 2)) {
    x <- extraDistr::rgev(100, xi = shape)
    fit <- mlgev(x)
    objective <- function(p, fixed = NULL) {
      k <- if (is.null(fixed)) p[3] else fixed
      if (k < -.5 || k > 1) return(1e100)
      ans <- -sum(extraDistr::dgev(x, mu = p[1], sigma = exp(p[2]), xi = k, log = TRUE))
      if (is.finite(ans)) ans else 1e100
    }
    starts <- c(-.5, -.2, 0, .3, 1)
    values <- vapply(starts, function(k) {
      scale <- max(sd(x), abs(k) * diff(range(x)))
      optim(c(median(x), log(scale), k), objective, method = "Nelder-Mead",
            control = list(reltol = 1e-12, maxit = 3000))$value
    }, numeric(1))
    expect_lte(-as.numeric(logLik(fit)), min(values) + 1e-6)
    expect_equal(as.numeric(logLik(fit)), sum(dml(x, fit, log = TRUE)), tolerance = 1e-9)
    expect_equal(attr(logLik(fit), "df"), 3L)
    for (k in c(-.5, 0, .3, 1)) {
      held <- mlgev(x, xi = k)
      oracle <- optim(c(held[["mu"]] + .1, log(held[["sigma"]]) + .1),
                      function(p) objective(p, k), method = "Nelder-Mead",
                      control = list(reltol = 1e-12, maxit = 2000))
      expect_equal(-as.numeric(logLik(held)), oracle$value, tolerance = 1e-7)
      expect_equal(unname(held[["xi"]]), k)
      expect_equal(attr(held, "fixed"), "xi")
      expect_equal(attr(logLik(held), "df"), 2L)
    }
    gumbel <- mlgumbel(x)
    zero <- mlgev(x, xi = 0)
    expect_equal(as.numeric(zero[c("mu", "sigma")]), as.numeric(gumbel), tolerance = 1e-7)
  }
})
test_that("mlgev reports invalid inputs and returns attained bounds", {
  for (x in list(c(1,2), c(0,0,1), c(0,1,1,2), rep(1,5)))
    expect_error(mlgev(x), "at least three|tie multiplicity")
  for (k in c(-.51,1.01,Inf,NaN)) expect_error(mlgev(1:5, xi = k), "in \\[-0.5, 1\\]")
  expect_error(mlgev(c(1,2,NA)))
  expect_error(mlgev(1:5, iterlim = 1), "converge")
  for (k in c(-.9, 2)) {
    x <- extraDistr::qgev(((1:400)-.5)/400, xi = k)
    fit <- mlgev(x)
    expect_equal(unname(fit[["xi"]]), if (k < 0) -.5 else 1)
  }
})
