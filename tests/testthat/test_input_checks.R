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
  object <- rlang::exec(dens, x)
  density <- univariateML_to_function(object, "d")
  expect_equal(sum(density(x, log = TRUE)), attr(object, "logLik"))
}
