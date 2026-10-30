test_that("slash agrees with an independent multistart density oracle", {
  logdensity <- function(x, m, s) {
    z <- (x-m)/s; t <- z*z/2
    ans <- -.5*log(2*pi)-log(s)+log(-expm1(-t))-2*log(abs(z))
    ans[z==0] <- -.5*log(2*pi)-log(2*s)
    ans
  }
  set.seed(709)
  samples <- c(list(c(0,1),c(0,0,1,2),c(-10,-9,0,1,2,20)),
               lapply(seq_len(18),function(j) rslash(n = 5 + j, mu = j%%3, sigma = 0.5 + j%%4)))
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
  set.seed(35); expected <- rslash(n = 10, mu = f[1], sigma = f[2])
  expect_identical(actual,expected)
})
test_that("continuous inversion avoids standardized overflow at tiny scales", {
  q <- qslash
  p <- pslash
  x <- q(p = -1000, mu = 0, sigma = 1e-300, lower.tail = FALSE, log.p = TRUE)
  expect_true(is.finite(x))
  expect_equal(p(q = x, mu = 0, sigma = 1e-300, lower.tail = FALSE, log.p = TRUE),-1000,tolerance=1e-10)
})
test_that("slash CDF matches its normal mixture and algebraic tail", {
  p <- pslash
  for(x in c(-5,-1,0,1,5)) {
    oracle <- integrate(function(u)pnorm(x*u),0,1,rel.tol=1e-12)$value
    expect_equal(p(q = x, mu = 0, sigma = 1),oracle,tolerance=1e-12)
  }
  expect_equal(p(q = 1e+200, mu = 0, sigma = 1, lower.tail = FALSE, log.p = TRUE),-.5*log(2*pi)-log(1e200),tolerance=1e-12)
})
test_that("slash globally searches separated location modes with held scale", {
  x <- c(-100,100)
  f <- mlslash(x,sigma=1)
  expect_gt(abs(f[["mu"]]),90)
  candidate <- sum(dslash(x = x, mu = -100, sigma = 1, log = TRUE))
  expect_gte(attr(f,"logLik"),candidate)
  expect_gt(attr(f,"logLik"),sum(dslash(x = x, mu = 0, sigma = 1, log = TRUE))+5)
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
  q <- qslash
  expect_equal(q(p = -1000, mu = 0, sigma = 1, log.p = TRUE),-Inf)
  expect_equal(q(p = -1000, mu = 0, sigma = 1, lower.tail = FALSE, log.p = TRUE),Inf)
})
test_that("slash half mass with zero reciprocal sum can have an attained MLE", {
  x <- c(rep(0,10),-1,rep(9,9)); n <- length(x)
  expect_equal(sum(1/x[x!=0]),0,tolerance=1e-15)
  boundary <- -.5*n*log(2*pi)-.5*n*log(2)-2*sum(log(abs(x[x!=0])))
  candidate <- sum(dslash(x = x, mu = 4, sigma = 3, log = TRUE))
  expect_gt(candidate,boundary+.3)
  f <- mlslash(x)
  expect_gte(attr(f,"logLik"),candidate-1e-9)
})
test_that("slash free-scale location profile has separated finite maxima", {
  x <- c(rep(0,10),-1,rep(10,9)); n <- length(x)
  boundary <- -.5*n*log(2*pi)-.5*n*log(2)-2*sum(log(abs(x[x!=0])))
  left <- sum(dslash(x = x, mu = -1/625, sigma = 1/5, log = TRUE))
  right <- sum(dslash(x = x, mu = 9/2, sigma = 7/2, log = TRUE))
  expect_gt(left,boundary+1e-4)
  expect_gt(right,boundary+.05)
  f <- mlslash(x)
  expect_gte(attr(f,"logLik"),right-1e-9)
})


test_that("slash new holds agree with independent multistart likelihoods", {
  set.seed(8100+5457)
  held_values <- c(mu=0.2,sigma=1.3)
  samples <- list(rlogis(20),rlogis(100),c(-2,-1,0,0,1,3),c(-1,2),
                  qnorm((1:20-.5)/20),seq(-1,1,length.out=20))
  ld <- function(x,p) {
    z <- (x-p[1])/p[2]; t <- z*z/2
    result <- -.5*log(2*pi)-log(p[2])+log(-expm1(-t))-2*log(abs(z))
    result[z==0] <- -.5*log(2*pi)-log(2*p[2])
    result
  }
  for(x in samples)for(mask in 1:3) {
    held <- names(held_values)[as.logical(intToBits(mask)[seq_along(held_values)])]
    args <- as.list(held_values[held])
    f <- tryCatch(suppressWarnings(do.call(mlslash,c(list(x=x),args))),error=identity)
    if(inherits(f,"error")) {
      expect_match(conditionMessage(f),"[Uu]se `ml|unbounded|collapse|support cannot contain|does not exist|positive")
      next
    }
    expect_identical(unname(as.numeric(f[held])),unname(held_values[held]))
    expect_setequal(attr(f,"fixed"),held)
    expect_equal(attr(logLik(f),"df"),length(held_values)-length(held))
    expect_equal(as.numeric(logLik(f)),sum(ld(x,as.numeric(f))),tolerance=1e-7)
    free <- setdiff(names(held_values),held)
    if(!length(free))next
    encode <- function(p) {z<-c(p[1],log(p[2]));names(z)<-names(held_values);z[free]}
    decode <- function(z) {
      p<-held_values
      for(j in seq_along(z)) {
        k<-match(free[j],names(p))
        p[k]<-if(k==1) z[j] else if(k==2) exp(z[j]) else exp(z[j])
      }
      p
    }
    objective <- function(z) {
      if(any(!is.finite(z))||any(abs(z)>30))return(1e100)
      v<-suppressWarnings(sum(ld(x,decode(z))))
      if(is.finite(v)) -v else 1e100
    }
    # Starts depend on the sample and held values, not on the fitted result.
    values <- c()
    for(shift in c(-1,0,1)) {
      start<-held_values;start[1]<-median(x)+shift;start[2]<-max(.2,sd(x))*exp(shift/2)
      start[held]<-held_values[held]
      o<-suppressWarnings(optim(encode(start),objective,method="Nelder-Mead",
           control=list(maxit=1800,reltol=1e-11)))
      values<-c(values,o$value)
    }
    expect_gte(as.numeric(logLik(f)),-min(values)-2e-6*length(x))
  }
})


test_that("slash conditional collapse, feasibility and invalid holds", {
  expect_error(mlslash(c(0,1),mu=0),"collapse")
  expect_error(mlslash(c(0,0,1),mu=0),"unbounded")
  expect_true(is.finite(logLik(mlslash(rep(2,4),mu=0))))
  expect_equal(as.numeric(mlslash(rep(2,4),mu=2,sigma=1)),c(2,1))
  expect_error(mlslash(1:5,mu=Inf),"finite")
})


test_that("slash independent density oracle agrees with a mixture integral at the centre", {
  for(z in c(0,1e-12,1e-6,.3)) {
    reference <- integrate(function(u) u*dnorm(u*z),0,1,rel.tol=1e-12)$value
    actual <- if(z==0) 1/(2*sqrt(2*pi)) else -expm1(-z*z/2)/(sqrt(2*pi)*z*z)
    expect_equal(actual,reference,tolerance=1e-12)
  }
})

test_that("slash: P3 unfinished local profile stops", {
  expect_error(mlslash(c(-2,-.3,.1,.8,4),iterlim=1), "failed")
})
