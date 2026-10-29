test_that("Poisson agrees with its reference likelihood", {
  x <- c(0, 1, 2, 4)
  fit <- mlpois(x)
  expect_equal(unname(coef(fit)), mean(x))
  expect_equal(attr(fit, "logLik"), sum(dpois(x, mean(x), log = TRUE)))
})

test_that("Poisson likelihood retains precision near the count ceiling", {
  for(x in list(rep(.Machine$integer.max,3),c(1e6,1e6+1,1e6+3))) {
    f<-mlpois(x)
    expect_equal(attr(f,"logLik"),sum(dpois(x,mean(x),log=TRUE)),tolerance=1e-10)
  }
})
