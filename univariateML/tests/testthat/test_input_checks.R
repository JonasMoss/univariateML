# Generating automatic tests.

forms_rhs <- c("x", "na.rm", "...")
for (dens in densities) {
  eval(call("context", paste0("Inputs, formals, and logLik: ", dens)))
  eval(call("expect_error", call(dens, "x")))
  eval(call("expect_error", call(dens, lm)))
  eval(call("expect_error", call(dens, replicate(3, 1:3))))
  eval(call("expect_error", call(dens, NA)))
  forms_lhs <- names(eval(call("formals", dens)))
  expect_equal(forms_lhs, forms_rhs)
  set.seed(1)
  x <- simulate_default(dens, 10)
  if (identical(dens, "mlemg")) x <- qemg((seq_len(40) - 0.5) / 40, 0, 1, 1)
  if (identical(dens, "mlbetapr")) {
    # This default ten-point sample selects the inverse-gamma face.
    expect_error(mlbetapr(x), "Use `mlinvgamma`")
    set.seed(313)
    x <- extraDistr::rbetapr(1000, 1, 2, scale = 3)
  }
  # A ten-point Student-t sample can have its supremum on a face.
  # Use the interior regression sample for this successful-fit contract.
  if (identical(dens, "mlstd")) {
    set.seed(313)
    x <- rstd(500, nu = 4)
  }
  if (identical(dens, "mllst")) {
    # The default ten-point sample escapes to normal; use interior t data.
    set.seed(313)
    x <- extraDistr::rlst(500, df = 3)
  }
  if (dens == "mlsnorm") {
    # This small default sample has an exclusive half-normal maximum.
    expect_error(mlsnorm(x), "translated half-normal.*MLE does not exist")
    x <- simulate_default(dens, 500)
  }
  if (dens == "mlsstd") {
    # This owned-RNG default sample selects the half-normal corner.
    expect_error(mlsstd(x), "translated half-normal.*Use .mlhnorm.")
    set.seed(313)
    x <- rsstd(500)
  }
  if (dens %in% c("mlged", "mlsged")) {
    set.seed(812)
    x <- rged(500, nu = 1.5)
  }
  if (dens %in% c("mlburr", "mlinvburr")) {
    # The free-scale ten-point fit can select a limit model.
    # Use interior data for the successful-fit contract.
    set.seed(1)
    x <- actuar::rburr(200, shape1 = 1, shape2 = 2, scale = 3)
    if (dens == "mlinvburr") x <- 1 / x
  }
  # Required design quantities (README fit_args): hypergeometric draws `k`
  # and population size `N`; beta-binomial `size`.
  args <- switch(dens, mlgb2 = list(shape1 = .3, shape3 = .4), mlhyper = list(k = 10, N = 50), mlbbinom = list(size = 10), mlcmb = list(size = 10), list())
  object <- do.call(dens, c(list(x), args))
  density <- univariateML_to_function(object, "d")
  expect_equal(sum(density(x, log = TRUE)), attr(object, "logLik"))
}
