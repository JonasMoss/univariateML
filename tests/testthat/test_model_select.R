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
  coef(model_select(small_data)),
  coef(model_select(c(small_data, NA), na.rm = TRUE))
)

## Does it find the correct model?
est <- mlnorm(small_data)
attr(est, "call") <- str2lang("f(x = x, na.rm = na.rm)")
expect_equal(
  est,
  model_select(small_data, models = c("norm", "cauchy"), criterion = "loglik"),
  model_select(small_data, models = c("norm", "cauchy"), criterion = "aic"),
  model_select(small_data, models = c("norm", "cauchy"), criterion = "bic")
)

## Check class.
est <- model_select(small_data, models = c("norm", "cauchy"))
expect_equal(attr(est, "model"), "Normal")
expect_equal(class(est), "univariateML")
