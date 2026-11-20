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
