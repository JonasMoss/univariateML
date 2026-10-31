test_that("CMB agrees with binomial, finite sums, tails and inversion", {
  for (N in c(0,1,10,30)) for (p in c(0,.01,.3,.99,1)) {
    k <- 0:N
    expect_equal(dcmb(x = k, size = N, prob = p, nu = 1),dbinom(k,N,p),tolerance=1e-12)
    expect_equal(dcmb(x = k, size = N, prob = p, nu = 1, log = TRUE),dbinom(k,N,p,TRUE),tolerance=1e-12)
    for (tail in c(TRUE,FALSE)) {
      q <- seq(-.5,N+1,.5)
      expect_equal(pcmb(q = q, size = N, prob = p, nu = 1, lower.tail = tail),pbinom(q,N,p,tail),tolerance=1e-12)
      expect_equal(pcmb(q = q, size = N, prob = p, nu = 1, lower.tail = tail, log.p = TRUE),pbinom(q,N,p,tail,TRUE),tolerance=1e-12)
      u <- c(0,1e-10,.1,.5,.9,1-1e-10,1)
      expect_equal(qcmb(p = u, size = N, prob = p, nu = 1, lower.tail = tail),qbinom(u,N,p,tail))
    }
  }
  for (nu in c(-3,0,.5,2,10)) {
    k <- 0:12; lp <- nu*lchoose(12,k)+k*log(.4)+(12-k)*log(.6)
    w <- exp(lp-max(lp)); w <- w/sum(w)
    expect_equal(dcmb(x = k, size = 12, prob = 0.4, nu = nu),w,tolerance=1e-12)
    expect_equal(pcmb(q = k + 0.75, size = 12, prob = 0.4, nu = nu),cumsum(w),tolerance=1e-12)
    expect_equal(pcmb(q = k, size = 12, prob = 0.4, nu = nu, lower.tail = FALSE),rev(c(0,cumsum(rev(w))[-13])),tolerance=1e-12)
    u <- c(.01,.2,.5,.9,.999)
    for (tail in c(TRUE,FALSE)) {
      z <- qcmb(p = log(u), size = 12, prob = 0.4, nu = nu, lower.tail = tail, log.p = TRUE)
      f <- pcmb(q = z, size = 12, prob = 0.4, nu = nu, lower.tail = tail); prev <- pcmb(q = z - 1, size = 12, prob = 0.4, nu = nu, lower.tail = tail)
      expect_true(all(if (tail) prev<u & f>=u else prev>u & f<=u))
    }
  }
  for (tail in c(TRUE,FALSE)) for (lg in c(TRUE,FALSE))
    expect_equal(qcmb(pcmb(q = 0:9, size = 10, prob = 0.4, nu = 1.2, lower.tail = tail, log.p = lg),10,.4,1.2,tail,lg),0:9)
  expect_equal(pcmb(q = 29, size = 30, prob = 0.001, nu = 1, lower.tail = FALSE, log.p = TRUE),dbinom(30,30,.001,TRUE))
  expect_equal(qcmb(p = -1000, size = 30, prob = 0.3, nu = 1, lower.tail = FALSE, log.p = TRUE),30)
})
test_that("CMB fits agree with an independent optim oracle and fixed subsets", {
  skip_unless_tier("full")
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
  expect_equal(mlcmb(c(0,0),size=5,nu=1)[["prob"]],0)
  expect_error(mlcmb(c(0,5),size=5,prob=.4),"endpoint two-point")
  expect_error(mlcmb(c(2,3),size=5,prob=.4),"adjacent two-point")
  expect_error(mlcmb(c(3,3),size=6,prob=.4),"point mass")
  expect_error(mlcmb(c(1,1),size=5,prob=.4),NA)
  expect_error(mlcmb(c(2,3),size=5,nu=1),NA)
  expect_error(mlcmb(c(0,0),size=5,prob=.4,nu=1),NA)
  expect_equal(as.numeric(mlcmb(0,size=0)),c(0,.5,1))
  expect_warning(f <- mlcmb(c(0,1,1),size=1),"not identified")
  expect_equal(f[["nu"]],1)
  expect_equal(f[["prob"]],2/3)
  expect_setequal(attr(f,"fixed"),c("size","nu"))
  expect_equal(attr(logLik(f),"df"),1L)
  expect_error(mlcmb(c(1,7),size=5),"between zero and size")
  expect_error(mlcmb(1,size=5,nu=Inf),"finite")
})
test_that("CMB recycling, warnings, RNG and fitted-model generics work", {
  expect_warning(expect_equal(dcmb(x = 0.5, size = 10, prob = 0.3, nu = 1),0),"non-integer")
  expect_warning(expect_true(is.nan(dcmb(x = 1, size = 10, prob = -1, nu = 1))),"NaNs")
  expect_warning(expect_true(is.nan(qcmb(p = 2, size = 10, prob = 0.3, nu = 1))),"NaNs")
  expect_identical(dcmb(x = NA_real_, size = 10, prob = 0.3, nu = 1),NA_real_)
  expect_equal(dcmb(x = 0:3, size = c(4, 5), prob = c(0.2, 0.4, 0.6), nu = c(-1, 1)),mapply(dcmb,0:3,c(4,5,4,5),c(.2,.4,.6,.2),c(-1,1,-1,1)))
  expect_length(dcmb(x = numeric(), size = 10, prob = 0.3, nu = 1),0)
  expect_length(rcmb(n = 0, size = 10, prob = 0.3, nu = 1),0)
  expect_length(rcmb(n = 3, size = c(5, 6, 7, 8), prob = 0.3, nu = 1),3)
  expect_identical(pcmb(q = 1, size = 10, prob = NA_real_, nu = 1),NA_real_)
  set.seed(78); x <- rcmb(n = 2000, size = 10, prob = 0.3, nu = 1)
  set.seed(78); expect_identical(x,rcmb(n = 2000, size = 10, prob = 0.3, nu = 1))
  f <- mlcmb(x,size=10,nu=1)


  expect_length(rml(20,f),20)
  expect_equal(dim(bootstrapml(f,reps=3)),c(3L,2L))
  expect_s3_class(model_select(x,models="cmb",size=10),"univariateML")
  grDevices::pdf(tempfile(fileext=".pdf")); on.exit(grDevices::dev.off())
  expect_error(plot(f),NA)
})

test_that("P3 CMB finite endpoints preserve held dispersion", {
 for(p in c(0,1)) for(nu in c(-2,0,2)) {
   x<-rep(5*p,4);f<-mlcmb(x,size=5,nu=nu)
   expect_equal(as.numeric(f),c(5,p,nu));expect_equal(attr(f,"logLik"),0)
   f<-mlcmb(x,size=5,prob=p,nu=nu)
   expect_equal(dml(c(0,5),f),if(p==0)c(1,0) else c(0,1))
   expect_error(mlcmb(c(0,1),size=5,prob=p,nu=nu))
 }
 expect_equal(as.numeric(mlcmb(c(0,0),size=0,prob=.2,nu=-3)),c(0,.2,-3))
})

test_that("P3 Bernoulli endpoint fits are silent", {
 expect_silent(f<-mlcmb(c(0,0),size=1))
 expect_equal(as.numeric(f),c(1,0,1))
 expect_silent(f<-mlcmb(c(1,1),size=1))
 expect_equal(as.numeric(f),c(1,1,1))
})

test_that("CMB: held canonical coordinates give a certified integer profile", {
  skip_unless_tier("dev")
  set.seed(9302)
  samples <- c(lapply(c(20,100),function(n) rcmb(n,10,.3,1.5)),
               list(c(0,0,1,1,2,4,8,10),rep(c(2,3),10),rep(0,20)),
               lapply(c(20,100),function(n) rpois(n,4)))
  for(x in samples) for(p in c(.2,.6)) for(nu in c(-2,0,.5,1.5)) {
    f <- mlcmb(x,size=NULL,prob=p,nu=nu)
    profile <- vapply(seq(max(x),120),function(N)sum(dcmb(x,N,p,nu,log=TRUE)),0.)
    expect_equal(attr(f,"logLik"),max(profile),tolerance=1e-9)
    expect_equal(f[["size"]],max(x)+which.max(profile)-1)
    expect_identical(unname(f[c("prob","nu")]),c(p,nu))
    expect_setequal(attr(f,"fixed"),c("prob","nu"))
    expect_equal(attr(logLik(f),"df"),1L)
    expect_equal(attr(f,"logLik"),sum(dcmb(x,f[["size"]],p,nu,log=TRUE)))
  }
  # The nu=1 submodel also agrees with an external reference.
  x <- c(0,1,2,2,3,5)
  f <- mlcmb(x,size=NULL,prob=.3,nu=1)
  oracle <- vapply(5:100,function(N)sum(dbinom(x,N,.3,log=TRUE)),0.)
  expect_equal(attr(f,"logLik"),max(oracle),tolerance=1e-10)
})

test_that("CMB: free size validates holds, endpoints and resource failures", {
  for(nu in c(-2,0,2)) for(p in c(0,1)) {
    f <- mlcmb(rep(3*p,20),size=NULL,prob=p,nu=nu)
    expect_equal(as.numeric(f),c(3*p,p,nu))
    expect_equal(attr(f,"logLik"),0)
    expect_setequal(attr(f,"fixed"),c("prob","nu"))
    expect_error(mlcmb(c(0,1),size=NULL,prob=p,nu=nu),"between zero and size")
  }
  expect_error(mlcmb(1:4,size=NULL,prob=.3),"global profile bound")
  expect_error(mlcmb(1:4,size=NULL,nu=1),"global profile bound")
  expect_error(mlcmb(1:4,size=NULL),"global profile bound")
  expect_error(mlcmb(1:4,size=NULL,prob=NaN,nu=1),"prob")
  expect_error(mlcmb(1:4,size=NULL,prob=.3,nu=Inf),"finite")
  expect_error(mlcmb(1:4,size=NULL,prob=.3,nu=1,iterlim=0),"positive")
  expect_error(mlcmb(c(0,100),size=NULL,prob=1e-8,nu=1),"work budget")
})

test_that("CMB: the P0 two-point limit omits a genuine CMP face", {
  # For fixed positive nu, odds = lambda / N^nu gives CMP weights
  # lambda^x / (x!)^nu, with positive mass on three or more integers.
  k <- 0:40; lambda <- 2; nu <- 1.5
  z <- k*log(lambda)-nu*lgamma(k+1)
  face <- exp(z-max(z)); face <- face/sum(face)
  for(N in c(1000,10000)) {
    p <- plogis(log(lambda)-nu*log(N))
    error <- max(abs(dcmb(k,N,p,nu)-face))
    expect_lt(error,2/N)
  }
  expect_gt(sum(face[1:5]),.9)
  expect_gt(min(face[1:4]),.01)
  # At nu=0 and fixed p<1/2 the size limit is geometric, not coercive.
  expect_equal(dcmb(0:8,100,.3,0),dgeom(0:8,1-.3/.7),tolerance=1e-12)
})

test_that("CMB: varying dispersion with held probability reaches a geometric law", {
  p <- .3; r <- .6; k <- 0:8
  # nu tends to zero, while exp(logit(p)) * N^nu stays equal to r.
  errors <- vapply(c(1000,10000,100000), function(N) {
    nu <- log(r/(p/(1-p)))/log(N)
    max(abs(dcmb(k,N,p,nu)-dgeom(k,1-r)))
  }, 0.)
  expect_true(all(diff(errors)<0))
  expect_lt(tail(errors,1),.015)
})

test_that("cmb attained point member evaluates and samples exactly", {
  fit <- mlcmb(rep(5,4),size=5,prob=1,nu=2)
  expect_equal(dml(c(4,5),fit),c(0,1))
  expect_equal(pml(c(4,5),fit),c(0,1))
  expect_equal(qml(.5,fit),5)
  expect_equal(rml(10,fit),rep(5,10))
})
