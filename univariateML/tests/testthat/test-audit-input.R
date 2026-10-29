test_that("empty and out-of-range counts fail before native conversion", {
  for (id in setdiff(names(univariateML_metadata), "mlzinb")) {
    fun <- get(id)
    args <- switch(id, mlhyper = list(k = 10, N = 50), mlnhyper = list(r = 3, N = 50), mlbbinom = list(size = 10), mlcmb = list(size = 10), list())
    expect_error(do.call(fun, c(list(numeric()), args)), "positive length", info = id)
    expect_error(do.call(fun, c(list(c(NA_real_, NaN), na.rm = TRUE), args)),
                 "positive length", info = id)
  }
  for (fun in list(mlpois, mlgeom, mlbinom, mlnbinom, mllgser, mldunif))
    expect_error(fun(c(1, 2^31)), "integer.max")
})
