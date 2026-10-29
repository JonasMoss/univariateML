test_that("empty and out-of-range counts fail before native conversion", {
  for (id in setdiff(names(univariateML_metadata), "mlzinb")) {
    fun <- get(id)
    expect_error(fun(numeric()), "positive length")
    expect_error(fun(c(NA_real_, NaN), na.rm = TRUE), "positive length")
  }
  for (fun in list(mlpois, mlgeom, mlbinom, mlnbinom, mllgser, mldunif))
    expect_error(fun(c(1, 2^31)), "integer.max")
})
