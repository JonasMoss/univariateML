test_that("PGW natural upper tails retain accuracy after extreme rescaling", {
  p <- getFromNamespace("ppgw", "univariateML")
  # Independent 1000-digit references in the PGW job's check-ranges.json.
  points <- list(
    c(x=3e-300, scale=1e-300, shape=1.0, exponent=4.7256055559161645, upper=2.6801379583380074e-304),
    c(x=6.551080335043532e-308, scale=1.0, shape=1.0, exponent=9.999999999999807e+307, upper=2.680137958338261e-304),
    c(x=9.999999999999998e+199, scale=1e+200, shape=3e+18, exponent=1.8188645488006158e+222, upper=2.6801379583375254e-304),
    c(x=3e+300, scale=1e+300, shape=5.963050297740191, exponent=1.0, upper=9.859676543765704e-305)
  )
  for (point in points) {
    got <- p(point[["x"]], point[["scale"]], point[["shape"]], point[["exponent"]], lower.tail=FALSE)
    expect_true(is.finite(got) && abs(got/point[["upper"]]-1)<=1e-10,
                info=paste(point,collapse=", "))
  }
})

test_that("PGW inverse failures and exact centers survive the R shell", {
  q <- getFromNamespace("qpgw", "univariateML")
  expect_warning(bad <- q(-3.000000004,1e-308,1e-12,2,lower.tail=FALSE,log.p=TRUE),
                 "NaNs produced")
  expect_true(is.nan(bad))
  expect_identical(q(-1,2,1e-308,1,lower.tail=FALSE,log.p=TRUE),2)
  cmin <- .Machine$double.xmin*.Machine$double.eps
  got <- q(-744.8065848419628,1,1e-4,cmin,log.p=TRUE)
  expect_lt(abs(got/1.0000000013261663-1),1e-9)
})
