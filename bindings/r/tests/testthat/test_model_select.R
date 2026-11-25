context("model_select")

## Data generation.
set.seed(313)
small_data <- rnorm(100, 0, 3)
tiny_data <- rnorm(10, 0, 3)

## Finds errors with na.
expect_error(model_select(c(tiny_data, NA)))

## Finds incorrect/insufficent models
expect_error(model_select(tiny_data, models = c("norm", "stupid")))
expect_error(model_select(tiny_data, models = c("exp", "lnorm")))

## Finds wrong criterion
expect_error(model_select(tiny_data, criterion = "cv"))

## Checks that na.rm works as intended.
expect_equal(
  coef(model_select(small_data, models = c("norm", "cauchy"))),
  coef(model_select(c(small_data, NA), models = c("norm", "cauchy"), na.rm = TRUE))
)

## Does it find the correct model?
est <- mlnorm(small_data)
est_aic <- model_select(small_data, models = c("norm", "cauchy"), criterion = "aic")
est_select <- model_select(small_data, models = c("norm", "cauchy"), criterion = "loglik")
est_bic <- model_select(small_data, models = c("norm", "cauchy"), criterion = "bic")

attr(est, "call") <- str2lang("f(x = x, na.rm = na.rm)")
attr(est_aic, "call") <- str2lang("f(x = x, na.rm = na.rm)")
attr(est_select, "call") <- str2lang("f(x = x, na.rm = na.rm)")
attr(est_bic, "call") <- str2lang("f(x = x, na.rm = na.rm)")

expect_equal(est, est_aic)
expect_equal(est, est_select)
expect_equal(est, est_bic)

## Check class.
est <- model_select(small_data, models = c("norm", "cauchy"))
expect_equal(attr(est, "model"), "Normal")
expect_equal(class(est), "univariateML")


## Test data frame version
est <- model_select(small_data, models = c("norm", "cauchy", "zeta"), type = "continuous", return = "all")
expect(est |> inherits("data.frame"), "est is not a data frame")
expect_equal(est$model, c("Normal", "Cauchy"))
expect_equal(
  c(est$d_logLik[[1]], est$d_AIC[[1]], est$d_BIC[[1]]),
  c(0, 0, 0)
)

test_that("model selection forwards controls only to fitters that accept them", {
  x <- c(0.4, 0.9, 1.5, 2.5, 3)
  fits <- model_select(x, c("exp", "gamma", "norm"), return = "all",
                       tol = 1e-8, iterlim = 100L)
  expect_setequal(fits$ml, c("exp", "gamma", "norm"))
  for (id in c("exp", "norm")) {
    fit <- fits$univariateML[[match(id, fits$ml)]]
    expect_equal(coef(fit), coef(get(paste0("ml", id))(x)))
  }
  gamma <- fits$univariateML[[match("gamma", fits$ml)]]
  expect_equal(coef(gamma), coef(mlgamma(x, tol = 1e-8, iterlim = 100L)))
  rejected <- model_select(x, c("exp", "gamma", "norm"), return = "all", tol = 0)
  expect_setequal(rejected$ml, c("exp", "norm"))
})

test_that("model selection keeps holds and modifier-specific controls", {
  x <- c(0, 1, 2, 2, 4, 6)
  fit <- model_select(x, "binom", size = 10, truncate = c(0, 8), tol = 1e-8)
  expect_identical(attr(fit, "fixed"), "size")
  expect_equal(coef(fit), coef(mlbinom(x, size = 10, truncate = c(0, 8), tol = 1e-8)))
  expect_error(model_select(x, "binom", size = 10, truncate = c(0, 8), tol = 0),
               "invalid modifier controls")
})

test_that("all failed fits retain their original conditions", {
  x <- rep(2, 8)
  err <- tryCatch(
    model_select(x, c("norm", "cauchy")),
    univariateML_nomle = function(e) "caught as nomle",
    univariateML_stop = identity
  )
  expect_identical(class(err),
                   c("univariateML_select_failed", "univariateML_stop", "error", "condition"))
  expect_identical(err$kind, "select_failed")
  expect_identical(err$reason, "all-fits-failed")
  expect_named(err$failures, c("mlnorm", "mlcauchy"))
  for (failure in err$failures) {
    expect_s3_class(failure, "univariateML_nomle")
    expect_identical(failure$kind, "nomle")
    expect_identical(failure$reason, "unbounded")
  }
  expect_match(conditionMessage(err), "(mlnorm)", fixed = TRUE)
  expect_match(conditionMessage(err), "(mlcauchy)", fixed = TRUE)
  for (failure in err$failures) {
    expect_match(conditionMessage(err), conditionMessage(failure), fixed = TRUE)
  }
})

test_that("selection reports per-model reasons when required coefficients are missing", {
  err <- tryCatch(model_select(1:8, c("fatigue", "binom")),
                  univariateML_select_failed = identity)
  expect_identical(err$reason, "no-eligible-models")
  expect_named(err$failures, c("mlfatigue", "mlbinom"))
  expect_match(err$failures$mlfatigue, "Missing required `mu`", fixed = TRUE)
  expect_match(err$failures$mlbinom, "Missing required `size`", fixed = TRUE)
  for (fitter in names(err$failures)) {
    expect_match(conditionMessage(err), paste(fitter, err$failures[[fitter]]), fixed = TRUE)
  }
})

test_that("selection reports both skipped and attempted candidates when none fit", {
  err <- tryCatch(model_select(rep(2, 8), c("binom", "norm")),
                  univariateML_select_failed = identity)
  expect_identical(err$reason, "all-fits-failed")
  expect_named(err$failures, c("mlbinom", "mlnorm"))
  expect_match(err$failures$mlbinom, "Missing required `size`", fixed = TRUE)
  expect_s3_class(err$failures$mlnorm, "univariateML_nomle")
  expect_match(conditionMessage(err), err$failures$mlbinom, fixed = TRUE)
})

test_that("a failed candidate adds no warning when another model fits", {
  x <- rep(2, 8)
  expect_no_warning(best <- model_select(x, c("norm", "exp")))
  expect_equal(coef(best), coef(mlexp(x)))
  expect_no_warning(all <- model_select(x, c("norm", "exp"), return = "all"))
  expect_identical(all$ml, "exp")
})

test_that("selection reports an empty list when no candidate passes the type filter", {
  err <- tryCatch(model_select(1:8, "norm", type = "discrete"),
                  univariateML_select_failed = identity)
  expect_identical(err$reason, "no-eligible-models")
  expect_length(err$failures, 0L)
})
