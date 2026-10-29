# Independent R normal interval calculation, with log-tail subtraction.
bhatt_oracle_density <- function(x,m,s,a) {
  lo <- (x-m-a)/s; hi <- (x-m+a)/s
  l <- ifelse(lo>=0,pnorm(lo,lower.tail=FALSE,log.p=TRUE),pnorm(hi,log.p=TRUE))
  r <- ifelse(lo>=0,pnorm(hi,lower.tail=FALSE,log.p=TRUE),pnorm(lo,log.p=TRUE))
  l+log(-expm1(pmin(0,r-l)))-log(2*a)
}
test_that("bhatt interior and faces agree with an independent multistart oracle", {
  set.seed(710)
  samples <- c(list(seq(-1,1,length.out=20),qnorm((1:30-.5)/30)),
               lapply(seq_len(8),function(j) extraDistr::rbhatt(20+5*j,0,1,3)))
  for(x in samples) {
    f <- tryCatch(mlbhatt(x),error=identity)
    normal <- sum(dnorm(x,mean(x),sqrt(mean((x-mean(x))^2)),log=TRUE))
    uniform <- -length(x)*log(diff(range(x)))
    oracle <- max(normal,uniform)
    for(s in c(.15,.7,1)*sd(x))for(a in c(.2,1,2)*sd(x)) {
      o <- optim(c(mean(x),log(s),log(a)),function(t) {
        ll <- sum(bhatt_oracle_density(x,t[1],exp(t[2]),exp(t[3])))
        if(is.finite(ll)) -ll else 1e100
      },control=list(maxit=1200,reltol=1e-11))
      oracle <- max(oracle,-o$value)
    }
    if(inherits(f,"error")) {
      expect_match(conditionMessage(f),"mlnorm|mlunif")
      expect_lte(oracle,max(normal,uniform)+2e-7*length(x))
    } else {
      expect_gte(attr(f,"logLik"),oracle-2e-7*length(x))
      expect_equal(attr(f,"logLik"),sum(bhatt_oracle_density(x,f[1],f[2],f[3])),tolerance=1e-8)
    }
  }
})
test_that("bhatt held scales have conditional faces and valid interior fits", {
  x <- c(-3,-2,-1,0,1,2,3)
  f <- mlbhatt(x,sigma=1,a=2)
  expect_equal(f[[1]],0,tolerance=1e-6)
  expect_equal(attr(f,"fixed"),c("sigma","a"))
  expect_equal(attr(f,"logLik"),sum(extraDistr::dbhatt(x,0,1,2,log=TRUE)),tolerance=1e-10)
  expect_error(mlbhatt(x,a=4),"mlunif")
  expect_error(mlbhatt(rep(2,5)),"unbounded")
  expect_error(mlbhatt(x,sigma=-1),"positive")
  expect_error(mlbhatt(x,a=0),"positive")
  expect_error(mlbhatt(x,iterlim=0),"failed")
  expect_error(mlbhatt(c(x,NA)))
  expect_error(mlbhatt(c(x,Inf)))
  expect_error(mlbhatt(c(-10,rep(0,20),10)),"mlnorm")
  expect_error(mlbhatt(seq(-1,1,length.out=40)),"mlunif")
})
test_that("bhatt reference repairs and continuous inversion retain tails", {
  f <- mlbhatt(c(-3,-1,0,1,3),sigma=1,a=2)
  d <- univariateML_to_function(f,"d");p <- univariateML_to_function(f,"p")
  q <- univariateML_to_function(f,"q")
  expect_equal(d(c(-40,-10,0,10,40),log=TRUE),
               bhatt_oracle_density(c(-40,-10,0,10,40),f[1],1,2),tolerance=1e-11)
  for(lower in c(TRUE,FALSE)) {
    lp <- c(-1e-10,-log(2),-10,-100,-1000)
    expect_equal(p(q(lp,lower.tail=lower,log.p=TRUE),lower.tail=lower,log.p=TRUE),lp,tolerance=1e-10)
  }
  expect_equal(q(c(0,1)),c(-Inf,Inf))
  expect_equal(q(.5,mu=c(-2,3),a=c(1,2)),c(-2,3),tolerance=1e-12)
  expect_length(q(numeric()),0)
  expect_true(is.na(q(NA_real_)))
  expect_warning(expect_true(is.nan(q(2))),"NaNs")
  set.seed(36); actual <- rml(10,f)
  set.seed(36); expected <- extraDistr::rbhatt(10,f[1],f[2],f[3])
  expect_identical(actual,expected)
})
test_that("each singly held bhatt scale agrees with a conditional oracle", {
  x <- c(-3,-2,-1,0,.5,1,2,3)
  for(held in c("sigma","a")) {
    value <- if(held=="sigma") .2 else .5
    f <- do.call(mlbhatt,c(list(x),setNames(list(value),held)))
    free <- if(held=="sigma") "a" else "sigma"
    best <- -Inf
    for(start in c(.1,1,3)) {
      o <- optim(c(0,log(start)),function(t) {
        par <- setNames(list(value,exp(t[2])),c(held,free))
        ll <- sum(bhatt_oracle_density(x,t[1],par$sigma,par$a))
        if(is.finite(ll)) -ll else 1e100
      },control=list(maxit=1200,reltol=1e-12))
      best <- max(best,-o$value)
    }
    expect_equal(attr(f,"fixed"),held)
    expect_equal(unname(f[[held]]),value)
    expect_gte(attr(f,"logLik"),best-1e-7)
  }
})
test_that("bhatt CDF matches an independent normal mixture integral", {
  p <- univariateML_reference_fixes[["extraDistr::pbhatt"]]$replacement
  for(x in c(-5,-1,0,1,5)) {
    oracle <- integrate(function(u)pnorm(x-u),-2,2,rel.tol=1e-12)$value/4
    expect_equal(p(x,mu=0,sigma=1,a=2),oracle,tolerance=1e-12)
    expect_equal(p(-x,mu=0,sigma=1,a=2,lower.tail=FALSE),oracle,tolerance=1e-12)
  }
})
test_that("bhatt constant samples respect each held subset", {
  expect_error(mlbhatt(rep(2,5),sigma=1),"mlnorm")
  expect_error(mlbhatt(rep(2,5),a=1),"mlunif")
  f <- mlbhatt(rep(2,5),sigma=1,a=1)
  expect_equal(f[["mu"]],2)
})
test_that("bhatt held-scale fits respect extreme changes of units", {
  x <- c(-3,-1,0,1,3); f <- mlbhatt(x,sigma=1,a=2)
  for(s in c(1e-200,1e200)) {
    g <- mlbhatt(x*s,sigma=s,a=2*s)
    expect_equal(as.numeric(g)/s,as.numeric(f),tolerance=1e-7)
    expect_equal(attr(g,"logLik")+length(x)*log(s),attr(f,"logLik"),tolerance=1e-10)
  }
})

test_that("bhatt retains finite log tails when a is below coordinate resolution", {
  d <- univariateML_reference_fixes[["extraDistr::dbhatt"]]$replacement
  p <- univariateML_reference_fixes[["extraDistr::pbhatt"]]$replacement
  expect_equal(d(1,sigma=1e-100,a=1e-200,log=TRUE),dnorm(1,sd=1e-100,log=TRUE),tolerance=1e-12)
  expect_equal(p(1,sigma=1e-100,a=1e-200,lower.tail=FALSE,log.p=TRUE),
               pnorm(1,sd=1e-100,lower.tail=FALSE,log.p=TRUE),tolerance=1e-12)
})

test_that("bhatt evaluation preserves the reference normal limit at a zero", {
  f <- mlbhatt(c(-3,-1,0,1,3),sigma=1,a=2)
  d <- univariateML_to_function(f,"d"); p <- univariateML_to_function(f,"p")
  q <- univariateML_to_function(f,"q")
  x <- c(-40,-10,0,10,40)
  expect_equal(d(x,a=0,log=TRUE),dnorm(x,log=TRUE),tolerance=1e-12)
  expect_equal(p(x,a=0,lower.tail=FALSE,log.p=TRUE),
               pnorm(x,lower.tail=FALSE,log.p=TRUE),tolerance=1e-12)
  lp <- c(-log(2),-10,-100,-1000)
  expect_equal(q(lp,a=0,lower.tail=FALSE,log.p=TRUE),
               qnorm(lp,lower.tail=FALSE,log.p=TRUE),tolerance=1e-10)
})

test_that("bhatt log probabilities close to one retain relative accuracy", {
  f <- mlbhatt(c(-3,-1,0,1,3),sigma=1,a=2)
  p <- univariateML_to_function(f,"p"); q <- univariateML_to_function(f,"q")
  lp <- -c(1e-20,1e-100)
  for(a in c(0,2))for(lower in c(TRUE,FALSE)) {
    x <- q(lp,a=a,lower.tail=lower,log.p=TRUE)
    expect_equal(p(x,a=a,lower.tail=lower,log.p=TRUE)/lp,c(1,1),tolerance=1e-9)
  }
})
