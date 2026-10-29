test_that("CMB agrees with binomial, finite sums, tails and inversion", {
  for (N in c(0,1,10,30)) for (p in c(0,.01,.3,.99,1)) {
    k <- 0:N
    expect_equal(dcmb(k,N,p,1),dbinom(k,N,p),tolerance=1e-12)
    expect_equal(dcmb(k,N,p,1,TRUE),dbinom(k,N,p,TRUE),tolerance=1e-12)
    for (tail in c(TRUE,FALSE)) {
      q <- seq(-.5,N+1,.5)
      expect_equal(pcmb(q,N,p,1,tail),pbinom(q,N,p,tail),tolerance=1e-12)
      expect_equal(pcmb(q,N,p,1,tail,TRUE),pbinom(q,N,p,tail,TRUE),tolerance=1e-12)
      u <- c(0,1e-10,.1,.5,.9,1-1e-10,1)
      expect_equal(qcmb(u,N,p,1,tail),qbinom(u,N,p,tail))
    }
  }
  for (nu in c(-3,0,.5,2,10)) {
    k <- 0:12; lp <- nu*lchoose(12,k)+k*log(.4)+(12-k)*log(.6)
    w <- exp(lp-max(lp)); w <- w/sum(w)
    expect_equal(dcmb(k,12,.4,nu),w,tolerance=1e-12)
    expect_equal(pcmb(k+.75,12,.4,nu),cumsum(w),tolerance=1e-12)
    expect_equal(pcmb(k,12,.4,nu,FALSE),rev(c(0,cumsum(rev(w))[-13])),tolerance=1e-12)
    u <- c(.01,.2,.5,.9,.999)
    for (tail in c(TRUE,FALSE)) {
      z <- qcmb(log(u),12,.4,nu,tail,TRUE)
      f <- pcmb(z,12,.4,nu,tail); prev <- pcmb(z-1,12,.4,nu,tail)
      expect_true(all(if (tail) prev<u & f>=u else prev>u & f<=u))
    }
  }
  for (tail in c(TRUE,FALSE)) for (lg in c(TRUE,FALSE))
    expect_equal(qcmb(pcmb(0:9,10,.4,1.2,tail,lg),10,.4,1.2,tail,lg),0:9)
  expect_equal(pcmb(29,30,.001,1,FALSE,TRUE),dbinom(30,30,.001,TRUE))
  expect_equal(qcmb(-1000,30,.3,1,FALSE,TRUE),30)
})
test_that("CMB fits agree with an independent optim oracle and fixed subsets", {
  x <- c(0,1,1,2,2,3,3,4,5,7,10)
  objective <- function(z) {
    k <- 0:10; lp <- z[1]*k+z[2]*lchoose(10,k)
    logz <- max(lp)+log(sum(exp(lp-max(lp))))
    -sum(z[1]*x+z[2]*lchoose(10,x)-logz)
  }
  o <- optim(c(0,0),objective,method="BFGS",control=list(reltol=1e-13))
  f <- mlcmb(x,size=10)
  expect_equal(c(qlogis(f[["prob"]]),f[["nu"]]),o$par,tolerance=1e-5)
  expect_equal(as.numeric(logLik(f)),-o$value,tolerance=1e-10)
  expect_equal(attr(f,"fixed"),"size")
  expect_equal(attr(logLik(f),"df"),2L)
  for (args in list(list(nu=1),list(prob=.3),list(prob=.3,nu=-1))) {
    g <- do.call(mlcmb,c(list(x,size=10),args))
    expect_setequal(attr(g,"fixed"),c("size",names(args)))
    expect_equal(attr(logLik(g),"df"),2L-length(args))
    expect_equal(as.numeric(logLik(g)),sum(dml(x,g,log=TRUE)))
    if (length(args)==1) {
      oracle <- if (!is.null(args$nu)) optimize(function(eta) objective(c(eta,args$nu)),c(-30,30)) else
        optimize(function(nu) objective(c(qlogis(args$prob),nu)),c(-30,30))
      expect_equal(as.numeric(logLik(g)),-oracle$objective,tolerance=1e-8)
    }
  }
  expect_lt(mlcmb(c(0,1,4,5),size=5)[["nu"]],0)
  g <- mlcmb(x,size=10,nu=1)
  expect_equal(g[["prob"]],mean(x)/10,tolerance=1e-10)
  expect_error(mlcmb(x),"requires.*size")
})
test_that("CMB detects precisely the reachable faces", {
  expect_error(mlcmb(c(2,2),size=5),"point mass")
  expect_error(mlcmb(c(2,3),size=5),"adjacent two-point")
  expect_error(mlcmb(c(0,5),size=5),"endpoint two-point")
  expect_error(mlcmb(c(0,0),size=5,nu=1),"point mass")
  expect_error(mlcmb(c(0,5),size=5,prob=.4),"endpoint two-point")
  expect_error(mlcmb(c(2,3),size=5,prob=.4),"adjacent two-point")
  expect_error(mlcmb(c(3,3),size=6,prob=.4),"point mass")
  expect_error(mlcmb(c(1,1),size=5,prob=.4),NA)
  expect_error(mlcmb(c(2,3),size=5,nu=1),NA)
  expect_error(mlcmb(c(0,0),size=5,prob=.4,nu=1),NA)
  expect_error(mlcmb(0,size=0),"unidentified")
  expect_warning(f <- mlcmb(c(0,1,1),size=1),"not identified")
  expect_equal(f[["nu"]],1)
  expect_equal(f[["prob"]],2/3)
  expect_setequal(attr(f,"fixed"),c("size","nu"))
  expect_equal(attr(logLik(f),"df"),1L)
  expect_error(mlcmb(c(1,7),size=5),"between zero and size")
  expect_error(mlcmb(1,size=5,nu=Inf),"finite")
})
test_that("CMB recycling, warnings, RNG and fitted-model generics work", {
  expect_warning(expect_equal(dcmb(.5,10,.3,1),0),"non-integer")
  expect_warning(expect_true(is.nan(dcmb(1,10,-1,1))),"NaNs")
  expect_warning(expect_true(is.nan(qcmb(2,10,.3,1))),"NaNs")
  expect_identical(dcmb(NA_real_,10,.3,1),NA_real_)
  expect_equal(dcmb(0:3,c(4,5),c(.2,.4,.6),c(-1,1)),mapply(dcmb,0:3,c(4,5,4,5),c(.2,.4,.6,.2),c(-1,1,-1,1)))
  expect_length(dcmb(numeric(),10,.3,1),0)
  expect_length(rcmb(0,10,.3,1),0)
  expect_length(rcmb(3,c(5,6,7,8),.3,1),3)
  expect_identical(pcmb(1,10,NA_real_,1),NA_real_)
  set.seed(78); x <- rcmb(2000,10,.3,1)
  set.seed(78); expect_identical(x,rcmb(2000,10,.3,1))
  f <- mlcmb(x,size=10,nu=1)
  expect_equal(pml(0:10,f),pcmb(0:10,10,f[["prob"]],1))
  expect_equal(qml(c(.1,.5,.9),f),qcmb(c(.1,.5,.9),10,f[["prob"]],1))
  expect_length(rml(20,f),20)
  expect_equal(dim(bootstrapml(f,reps=3)),c(3L,2L))
  expect_s3_class(model_select(x,models="cmb",size=10),"univariateML")
  grDevices::pdf(tempfile(fileext=".pdf")); on.exit(grDevices::dev.off())
  expect_error(plot(f),NA)
})
