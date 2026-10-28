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
  # A ten-point Student-t sample can have its supremum on a face.
  # Use the interior regression sample for this successful-fit contract.
  if (identical(dens, "mlstd")) {
    set.seed(313)
    x <- fGarch::rstd(500, nu = 4)
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
  if (dens %in% c("mlged", "mlsged")) {
    set.seed(812)
    x <- fGarch::rged(500, nu = 1.5)
  }
  object <- do.call(dens, list(x))
  density <- univariateML_to_function(object, "d")
  expect_equal(sum(density(x, log = TRUE)), attr(object, "logLik"))
}
