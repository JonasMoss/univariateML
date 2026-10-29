test_that("half-Cauchy scale agrees with independent density optimization", {
  samples <- list(c(1), c(1,4), c(0,1,2), c(.001,1,2,1000), c(1,1,1,20))
  for (x in samples) {
    f <- mlhcauchy(x)
    o <- optimize(function(t) -sum(extraDistr::dhcauchy(x, exp(t), log = TRUE)), c(-25,25), tol = 1e-11)
    expect_equal(log(f[["sigma"]]), o$minimum, tolerance = 1e-6)
    expect_equal(attr(f,"logLik"), -o$objective, tolerance = 1e-10)
    expect_equal(sum(dml(x,f,log=TRUE)), attr(f,"logLik"), tolerance = 1e-10)
  }
  expect_equal(mlhcauchy(c(1,4))[["sigma"]], 2)
  expect_error(mlhcauchy(c(0,1)), "finite unattained")
  expect_error(mlhcauchy(c(0,0,1)), "unbounded")
  expect_error(mlhcauchy(rep(0,4)), "unbounded")
  expect_error(mlhcauchy(-1), "support")
  expect_error(mlhcauchy(Inf))
  expect_equal(mlhcauchy(c(1,4)*1e200)[["sigma"]]/1e200, 2, tolerance=1e-11)
  expect_equal(mlhcauchy(c(1,4)*1e-200)[["sigma"]]/1e-200, 2, tolerance=1e-11)
})

test_that("half-Cauchy reference repairs preserve ordinary values and tails", {
  f <- mlhcauchy(c(1,4)); s <- f[["sigma"]]
  expect_equal(dml(c(-1,0,1,10),f), extraDistr::dhcauchy(c(-1,0,1,10),s))
  expect_equal(dml(1e200,f,log=TRUE), log(2/pi)+log(s)-2*log(1e200), tolerance=1e-12)
  for (lower in c(TRUE,FALSE)) for (logs in c(TRUE,FALSE)) {
    u <- c(0,.001,.5,.999,1)
    expect_equal(pml(qml(if(logs)log(u)else u,f,lower,logs),f,lower),u,tolerance=1e-11)
  }
  expect_equal(pml(qml(-100,f,FALSE,TRUE),f,FALSE,TRUE), -100, tolerance=1e-12)
  expect_equal(pml(qml(-100,f,TRUE,TRUE),f,TRUE,TRUE), -100, tolerance=1e-12)
  set.seed(7); a <- rml(30,f)
  set.seed(7); expect_identical(a, extraDistr::rhcauchy(30,s))
})
