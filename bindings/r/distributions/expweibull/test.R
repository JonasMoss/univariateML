test_that("expweibull routes every hold subset and carries certification", {
  x <- qml(((1:100)-.5)/100,mlexpweibull(scale=2,shape=1.5,exponent=2.5))
  values <- list(scale=2,shape=1.5,exponent=2.5)
  for(mask in 0:7) {
    held <- names(values)[as.logical(bitwAnd(mask,2^(0:2)))]
    fit <- do.call(mlexpweibull,c(list(x=x),values[held]))
    expect_identical(sort(attr(fit,"fixed")),sort(held))
    for(name in held) expect_equal(unname(fit[name]),values[[name]])
    expect_equal(attr(fit,"logLik"),sum(dml(x,fit,log=TRUE)),tolerance=1e-10)
    if(mask<7) expect_identical(attr(fit,"certified"),bitwAnd(mask,4L)!=0 || bitwAnd(mask,3L)==3)
    else expect_equal(attr(logLik(fit),"df"),0L)
  }
})
test_that("expweibull validates inputs and reports proved concentration", {
  expect_error(mlexpweibull(c(0,1,2)),"positive")
  for(name in c("scale","shape","exponent")) expect_error(do.call(mlexpweibull,c(list(x=c(1,2,3)),setNames(list(0),name))),"positive")
  expect_error(mlexpweibull(rep(2,5)),"unbounded")
  expect_error(mlexpweibull(rep(2,5),shape=1.5),"unbounded")
  expect_error(mlexpweibull(rep(2,5),scale=2,exponent=1),"unbounded")
  expect_error(mlexpweibull(rep(2,5),scale=1),"unbounded")
  expect_s3_class(mlexpweibull(rep(2,5),shape=1.5,exponent=2.5),"univariateML")
  expect_s3_class(mlexpweibull(rep(2,5),scale=2,shape=1.5),"univariateML")
})
test_that("held scale power stops retain endpoint likelihood semantics", {
  strict <- tryCatch(mlexpweibull(c(1,1.5,2),scale=3),error=identity)
  expect_s3_class(strict,"univariateML_limit")
  expect_identical(strict$model,"power")
  expect_equal(unname(strict$estimates["alpha"]),3)
  tied <- tryCatch(mlexpweibull(c(1,2,3,3),scale=3),error=identity)
  expect_s3_class(tied,"univariateML_limit")
  expect_match(conditionMessage(tied),"endpoint ties")
  expect_identical(tied$model,"unavailable:power-envelope-endpoint-ties")
  beta <- 4/sum(log(3/c(1,2,3,3)))
  expect_equal(tied$limit_loglik,4*(log(beta)-1)-sum(log(c(1,2,3,3)))-2*log(expm1(1)),tolerance=1e-10)
})
test_that("held exponent one equals Weibull", {
  x <- c(.4,.7,1.2,1.3,2.4,2.8,4,6)
  ew <- mlexpweibull(x,exponent=1)
  w <- mlweibull(x)
  expect_equal(unname(ew[c("shape","scale")]),unname(w[c("shape","scale")]),tolerance=1e-10)
  expect_true(attr(ew,"certified"))
})
test_that("open two-free fits match an independent likelihood search", {
  skip_unless_tier("full")
  x <- qml(((1:100)-.5)/100,mlexpweibull(scale=2,shape=1.5,exponent=2.5))
  for(mask in 0:3) {
    held <- as.logical(bitwAnd(mask,2^(0:2)))
    fit <- do.call(mlexpweibull,c(list(x=x),list(scale=2,shape=1.5,exponent=2.5)[held]))
    objective <- function(v) {
      p <- log(c(2,1.5,2.5));p[!held] <- v
      ll <- sum(dexpweibull(x,exp(p[1]),exp(p[2]),exp(p[3]),log=TRUE))
      if(is.finite(ll)) -ll else 1e100
    }
    for(c0 in c(.3,1,3,10)) {
      p <- log(c(2,1.5,c0))
      opt <- optim(p[!held],objective,method="BFGS",control=list(maxit=1000,reltol=1e-12))
      expect_gte(attr(fit,"logLik"),-opt$value-1e-7)
    }
  }
})
test_that("expweibull accuracy stops carry public candidates", {
  err <- tryCatch(mlexpweibull(c(1,2,3),scale=2,shape=1.5,tol=1e-30),error=identity)
  expect_s3_class(err,"univariateML_tolerance")
  expect_gt(err$attainable_tol,0)
  expect_true(nrow(err$candidates)>0)
})
test_that("held Weibull shape remains accurate below the normal range", {
  x <- c(.5,1,2,4)
  b <- .Machine$double.xmin*.Machine$double.eps
  fit <- mlexpweibull(x,shape=b,exponent=1)
  expect_equal(unname(fit["scale"]),sqrt(2),tolerance=1e-10)
  expect_identical(unname(fit["shape"]),b)
  expect_true(attr(fit,"certified"))
  expect_equal(attr(fit,"logLik"),sum(dml(x,fit,log=TRUE)),tolerance=1e-10)
})
