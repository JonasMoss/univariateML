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
  root <- Sys.getenv("UMVL_DISTRIBUTIONS_DIR")
  if (!nzchar(root)) next
  golden <- jsonlite::fromJSON(file.path(root, sub("^ml", "", dens), "golden.json"))
  x <- as.numeric(golden$data)
  args <- golden_fit_args(dens)
  object <- do.call(dens, c(list(x), args))
  density <- univariateML_to_function(object, "d")
  expect_equal(sum(density(x, log = TRUE)), attr(object, "logLik"))
}
