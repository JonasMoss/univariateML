# Parameters held fixed (README params: fixable / fixed) are reported in the
# `fixed` attribute and are not counted as estimated.

test_that("a supplied size is held fixed and not counted in AIC/BIC", {
  set.seed(1)
  x <- rbinom(100, 20, 0.3)
  fit <- mlbinom(x, size = 20)
  expect_equal(attr(fit, "fixed"), "size")
  expect_equal(unname(fit["size"]), 20)
  expect_equal(attr(logLik(fit), "df"), 1L)
  expect_equal(AIC(fit), -2 * attr(fit, "logLik") + 2)
  free <- mlbinom(x)
  expect_equal(attr(free, "fixed"), character(0))
  expect_equal(attr(logLik(free), "df"), 2L)
  nb <- mlnbinom(rnbinom(100, size = 3, mu = 4), size = 3)
  expect_equal(attr(nb, "fixed"), "size")
  expect_equal(attr(logLik(nb), "df"), 1L)
})

test_that("fatigue's location is always fixed", {
  set.seed(1)
  fit <- mlfatigue(extraDistr::rfatigue(50, 0.5, 2))
  expect_equal(attr(fit, "fixed"), "mu")
  expect_equal(attr(logLik(fit), "df"), 2L)
  expect_equal(BIC(fit), -2 * attr(fit, "logLik") + 2 * log(50))
})

test_that("bootstrapml refits with the same parameters held fixed", {
  set.seed(1)
  fit <- mlbinom(rbinom(50, 20, 0.3), size = 20)
  boot <- bootstrapml(fit, reps = 10, reducer = identity)
  expect_true(all(boot["size", ] == 20))
})

test_that("model_select counts only estimated parameters", {
  set.seed(1)
  x <- extraDistr::rfatigue(100, 0.5, 2)
  sel <- model_select(x, models = c("exp", "fatigue", "weibull"), return = "all")
  # Rows are sorted by AIC; p must follow its own row.
  expect_equal(sel$ml, c("fatigue", "weibull", "exp"))
  expect_equal(unname(sel$p), c(2L, 2L, 1L))
  expect_equal(sel$AIC[sel$ml == "fatigue"], AIC(mlfatigue(x)))
})
