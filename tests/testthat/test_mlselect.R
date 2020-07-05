context("mlselect")

## Data generation.
set.seed(313)
small_data <- rnorm(100, 0, 3)
tiny_data <- rnorm(10, 0, 3)

## Finds errors with na.
expect_error(mlselect(c(tiny_data, NA)))

## Finds incorrect/insufficent families
expect_error(mlselect(tiny_data, families = c("norm", "stupid")))
expect_error(mlselect(tiny_data, families = c("exp", "lnorm")))

## Finds wrong criterion
expect_error(mlselect(tiny_data, criterion = "cv"))

## Checks that na.rm works as intended.
expect_equal(
  coef(mlselect(small_data)),
  coef(mlselect(c(small_data, NA), na.rm = TRUE))
)

## Does it find the correct model?
est <- mlnorm(small_data)
attr(est, "call") <- str2lang("f(x = x, na.rm = na.rm)")
expect_equal(
  est,
  mlselect(small_data, families = c("norm", "cauchy"), criterion = "loglik"),
  mlselect(small_data, families = c("norm", "cauchy"), criterion = "aic"),
  mlselect(small_data, families = c("norm", "cauchy"), criterion = "bic")
)

## Check class.
est <- mlselect(small_data, families = c("norm", "cauchy"))
expect_equal(attr(est, "model"), "Normal")
expect_equal(class(est), "univariateML")
