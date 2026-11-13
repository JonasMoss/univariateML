test_that("empty and out-of-range counts fail before native conversion", {
  for (id in umvl_filter_families(setdiff(names(univariateML_metadata), ".modifier_zinb"))) {
    fun <- get(id)
    args <- golden_fit_args(id)
    expect_error(do.call(fun, c(list(numeric()), args)), "positive length", info = id)
    expect_error(do.call(fun, c(list(c(NA_real_, NaN), na.rm = TRUE), args)),
                 "positive length", info = id)
  }
  for (fun in list(mlpois, mlgeom, mlbinom, mlnbinom, mllgser, mldunif))
    expect_error(do.call(fun, c(list(c(1, 2^31)), if(identical(fun, mlbinom)) list(size=NULL) else list())), "integer.max")
})
