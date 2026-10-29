test_that("slash agrees with an independent multistart density oracle", {
  logdensity <- function(x, m, s) {
    z <- (x-m)/s; t <- z*z/2
    ans <- -.5*log(2*pi)-log(s)+log(-expm1(-t))-2*log(abs(z))
    ans[z==0] <- -.5*log(2*pi)-log(2*s)
    ans
  }
  set.seed(709)
  samples <- c(list(c(0,1),c(0,0,1,2),c(-10,-9,0,1,2,20)),
               lapply(seq_len(18),function(j) extraDistr::rslash(5+j, j%%3, .5+j%%4)))
  for(x in samples) {
    f <- mlslash(x)
    oracle <- -Inf
    for(m in quantile(x,c(.1,.5,.9))) for(s in c(.1,1,3)*sd(x)) {
      o <- optim(c(m,log(s)),function(t) -sum(logdensity(x,t[1],exp(t[2]))),
                 method="Nelder-Mead",control=list(maxit=1500,reltol=1e-12))
      oracle <- max(oracle,-o$value)
    }
    expect_gte(attr(f,"logLik"),oracle-1e-7*length(x))
    expect_equal(attr(f,"logLik"),sum(logdensity(x,f[1],f[2])),tolerance=1e-10)
    held <- mlslash(x,sigma=1)
    o <- optimize(function(m)sum(logdensity(x,m,1)),range(x),maximum=TRUE)
    expect_gte(attr(held,"logLik"),o$objective-1e-6)
    expect_equal(attr(held,"fixed"),"sigma")
  }
})
test_that("slash collapse and input errors are explicit", {
  expect_error(mlslash(c(0,0,0,1)),"unbounded")
  expect_error(mlslash(rep(0,4)),"unbounded")
  expect_error(mlslash(c(0,0,-1,1)),"collapse")
  expect_s3_class(mlslash(c(0,0,0,1),sigma=1),"univariateML")
  expect_error(mlslash(1:5,sigma=-1),"positive")
  expect_error(mlslash(1:5,iterlim=0),"failed")
  expect_error(mlslash(c(1,NA)))
  expect_error(mlslash(c(1,Inf)))
  expect_equal(coef(mlslash(c(1:5,NA),na.rm=TRUE)),coef(mlslash(1:5)))
})
test_that("continuous slash quantiles recycle and retain log tails", {
  f <- mlslash(c(-3,-1,0,2,8))
  q <- univariateML_to_function(f,"q"); p <- univariateML_to_function(f,"p")
  d <- univariateML_to_function(f,"d")
  for(lower in c(TRUE,FALSE)) {
    lp <- c(-1e-10,-log(2),-10,-100,-600)
    expect_equal(p(q(lp,lower.tail=lower,log.p=TRUE),lower.tail=lower,log.p=TRUE),lp,tolerance=1e-10)
  }
  expect_equal(q(c(0,1)),c(-Inf,Inf))
  expect_equal(q(c(-Inf,0),log.p=TRUE,lower.tail=FALSE),c(Inf,-Inf))
  expect_equal(q(.5,mu=c(-1,2),sigma=c(1,3)),c(-1,2),tolerance=1e-12)
  expect_length(q(numeric()),0)
  expect_true(is.na(q(NA_real_)))
  expect_warning(expect_true(is.nan(q(-1))),"NaNs")
  expect_equal(d(f[1]),unname(1/(2*sqrt(2*pi)*f[2])),tolerance=1e-12)
  expect_equal(d(f[1]+1e-10),d(f[1]),tolerance=1e-12)
  set.seed(35); actual <- rml(10,f)
  set.seed(35); expected <- extraDistr::rslash(10,f[1],f[2])
  expect_identical(actual,expected)
})
test_that("continuous inversion avoids standardized overflow at tiny scales", {
  q <- univariateML_continuous_quantile(univariateML_reference_quantiles[["extraDistr::qslash"]])
  p <- univariateML_reference_fixes[["extraDistr::pslash"]]$replacement
  x <- q(-1000,mu=0,sigma=1e-300,log.p=TRUE,lower.tail=FALSE)
  expect_true(is.finite(x))
  expect_equal(p(x,mu=0,sigma=1e-300,log.p=TRUE,lower.tail=FALSE),-1000,tolerance=1e-10)
})
test_that("slash CDF matches its normal mixture and algebraic tail", {
  p <- univariateML_reference_fixes[["extraDistr::pslash"]]$replacement
  for(x in c(-5,-1,0,1,5)) {
    oracle <- integrate(function(u)pnorm(x*u),0,1,rel.tol=1e-12)$value
    expect_equal(p(x),oracle,tolerance=1e-12)
  }
  expect_equal(p(1e200,lower.tail=FALSE,log.p=TRUE),-.5*log(2*pi)-log(1e200),tolerance=1e-12)
})
test_that("slash globally searches separated location modes with held scale", {
  x <- c(-100,100)
  f <- mlslash(x,sigma=1)
  expect_gt(abs(f[["mu"]]),90)
  candidate <- sum(extraDistr::dslash(x,mu=-100,sigma=1,log=TRUE))
  expect_gte(attr(f,"logLik"),candidate)
  expect_gt(attr(f,"logLik"),sum(extraDistr::dslash(x,mu=0,sigma=1,log=TRUE))+5)
})
test_that("slash fits and likelihoods are equivariant over extreme units", {
  x <- c(-3,-1,0,2,8); f <- mlslash(x)
  for(s in c(1e-200,1e200)) {
    g <- mlslash(x*s)
    expect_equal(as.numeric(g)/s,as.numeric(f),tolerance=1e-6)
    expect_equal(attr(g,"logLik")+length(x)*log(s),attr(f,"logLik"),tolerance=1e-10)
  }
  f <- mlslash(rep(2,5),sigma=1)
  expect_equal(f[["mu"]],2,tolerance=1e-7)
})
test_that("slash normalizes by the bulk rather than a remote outlier", {
  f <- mlslash(c(-2,-1,0,1,2,1e100))
  expect_true(all(is.finite(f)))
  expect_lt(f[["sigma"]],10)
  expect_lt(abs(f[["mu"]]),3)
})

test_that("unrepresentable finite slash probabilities give infinite quantiles", {
  q <- univariateML_continuous_quantile(univariateML_reference_quantiles[["extraDistr::qslash"]])
  expect_equal(q(-1000,log.p=TRUE),-Inf)
  expect_equal(q(-1000,lower.tail=FALSE,log.p=TRUE),Inf)
})
test_that("slash half mass with zero reciprocal sum can have an attained MLE", {
  x <- c(rep(0,10),-1,rep(9,9)); n <- length(x)
  expect_equal(sum(1/x[x!=0]),0,tolerance=1e-15)
  boundary <- -.5*n*log(2*pi)-.5*n*log(2)-2*sum(log(abs(x[x!=0])))
  candidate <- sum(extraDistr::dslash(x,mu=4,sigma=3,log=TRUE))
  expect_gt(candidate,boundary+.3)
  f <- mlslash(x)
  expect_gte(attr(f,"logLik"),candidate-1e-9)
})
test_that("slash free-scale location profile has separated finite maxima", {
  x <- c(rep(0,10),-1,rep(10,9)); n <- length(x)
  boundary <- -.5*n*log(2*pi)-.5*n*log(2)-2*sum(log(abs(x[x!=0])))
  left <- sum(extraDistr::dslash(x,mu=-1/625,sigma=1/5,log=TRUE))
  right <- sum(extraDistr::dslash(x,mu=9/2,sigma=7/2,log=TRUE))
  expect_gt(left,boundary+1e-4)
  expect_gt(right,boundary+.05)
  f <- mlslash(x)
  expect_gte(attr(f,"logLik"),right-1e-9)
})
