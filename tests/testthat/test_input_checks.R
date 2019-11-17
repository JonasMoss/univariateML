# Generating automatic tests for input checks.

for (dens in densities) {
  eval(call("context", paste0("Input check: ", dens)))
  eval(call("expect_error", call(dens, "x")))
  eval(call("expect_error", call(dens, lm)))
  eval(call("expect_error", call(dens, replicate(3, 1:3))))
  eval(call("expect_error", call(dens, NA)))
}
