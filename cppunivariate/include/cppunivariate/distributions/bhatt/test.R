# Independent R normal interval calculation, with log-tail subtraction.
bhatt_oracle_density <- function(x,m,s,a) {
  if(a==0) return(dnorm(x,m,s,log=TRUE))
  # Normal-mixture Taylor integral prevents cancellation when the two
  # normal log probabilities differ by less than floating-point precision.
  z <- (x-m)/s; h <- a/s
  if(h<1e-5 && all(abs(z)*h<1e-3)) {
    correction <- h^2*(z^2-1)/6 + h^4*(z^4-6*z^2+3)/120
    return(dnorm(x,m,s,log=TRUE)+log1p(correction))
  }
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
  expect_equal(unname(mlbhatt(x,a=0)["a"]),0)
  expect_error(mlbhatt(x,iterlim=0),"failed")
  expect_error(mlbhatt(c(x,NA)))
  expect_error(mlbhatt(c(x,Inf)))
  expect_equal(unname(mlbhatt(c(-10,rep(0,20),10))["a"]),0)
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
  expect_equal(as.numeric(mlbhatt(rep(2,5),sigma=1)),c(2,1,0))
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


test_that("bhatt new holds agree with independent multistart likelihoods", {
  set.seed(8100+6378)
  held_values <- c(mu=0.2,sigma=1.3,a=0.5)
  samples <- list(rlogis(20),rlogis(100),c(-2,-1,0,0,1,3),c(-1,2),
                  qnorm((1:20-.5)/20),seq(-1,1,length.out=20))
  ld <- function(x,p) bhatt_oracle_density(x,p[1],p[2],p[3])
  for(x in samples)for(mask in 1:7) {
    held <- names(held_values)[as.logical(intToBits(mask)[seq_along(held_values)])]
    args <- as.list(held_values[held])
    f <- tryCatch(suppressWarnings(do.call(mlbhatt,c(list(x=x),args))),error=identity)
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
    encode <- function(p) {z<-c(p[1],log(p[2]));z<-c(z,log(p[3]));names(z)<-names(held_values);z[free]}
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


test_that("bhatt conditional collapse, feasibility and invalid holds", {
  expect_error(mlbhatt(c(-1,0,1),mu=0,a=1),"mlunif")
  expect_error(mlbhatt(rep(2,4),mu=2),"unbounded")
  expect_true(is.finite(logLik(mlbhatt(rep(2,4),mu=0,sigma=1))))
  expect_equal(as.numeric(mlbhatt(c(-1,1),mu=0,sigma=1,a=0)),c(0,1,0))
  expect_error(mlbhatt(1:5,mu=Inf),"finite")
})


test_that("bhatt independent density oracle retains tiny uniform widths", {
  for(a in c(1e-2,1e-6,1e-12))for(x in c(-2,0,2)) {
    reference <- integrate(function(u) dnorm(x-u,.2,1.3),-a,a,
                           rel.tol=1e-12)$value/(2*a)
    expect_equal(bhatt_oracle_density(x,.2,1.3,a),log(reference),tolerance=1e-10)
  }
})

test_that("P3 bhatt returns the normal maximum as an attained member", {
 x<-c(-10,rep(0,20),10)
 expect_silent(f<-mlbhatt(x))
 expect_equal(f[["a"]],0)
 expect_equal(attr(f,"logLik"),attr(mlnorm(x),"logLik"))
 f<-mlbhatt(x,mu=1,sigma=4,a=0)
 expect_equal(as.numeric(f),c(1,4,0))
 expect_equal(attr(f,"logLik"),sum(dnorm(x,1,4,log=TRUE)))
})
