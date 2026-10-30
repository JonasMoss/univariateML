test_that("tbinom agrees with independent reference likelihood searches", {
  set.seed(981)
  for (j in 1:20) {
    x <- extraDistr::rtbinom(40,size=12,prob=.4,a=0,b=10)
    f <- mltbinom(x,a=0,b=10,size=12)
    objective <- function(t) sum(extraDistr::dtbinom(x,size=12,prob=plogis(t),a=0,b=10,log=TRUE))
    o <- optimize(objective,c(-8,8),maximum=TRUE,tol=1e-10)
    expect_equal(attr(f,"logLik"),o$objective,tolerance=1e-8)
    expect_equal(sum(dml(x,f,log=TRUE)),attr(f,"logLik"),tolerance=1e-10)
  }
  expect_error(mltbinom(rep(1,3),a=0,b=10,size=12),"point mass")
  expect_error(mltbinom(rep(10,3),a=0,b=10,size=12),"point mass")
  expect_error(mltbinom(c(0,2),a=0,b=10,size=12),"outside")
  expect_equal(attr(mltbinom(rep(1,3),a=0,b=1,size=12),"logLik"),0)
  expect_error(mltbinom(1:3),"requires")
  f <- mltbinom(c(1,2,3,4),a=0,b=10,size=12)
  expect_equal(qml(c(.1,.5,.9),f),extraDistr::qtbinom(c(.1,.5,.9),size=12,prob=f[[1]],a=0,b=10))
  set.seed(2); a<-rml(10,f);set.seed(2);expect_identical(a,rml(10,f))
})

test_that("tbinom fixed parameters and repaired extreme tails", {
 f<-mltbinom(rep(1,3),a=0,b=1,size=12,prob=0.4)
 expect_equal(attr(f,"logLik"),0,tolerance=1e-12)
 expect_equal(attr(logLik(f),"df"),0)
 f<-mltbinom(c(91,92,93),a=90,b=100,size=100,prob=0.1)
 expect_true(all(is.finite(dml(c(91,92,93),f,log=TRUE))))
 expect_true(is.finite(pml(99,f,lower.tail=FALSE,log.p=TRUE)))
 expect_true(qml(-100,f,lower.tail=FALSE,log.p=TRUE)<=100)
})
test_that("binomial endpoints with positive conditioning mass are attained", {
 for(v in c(0,12)) {
  f<-mltbinom(rep(v,3),a=-Inf,b=Inf,size=12)
  expect_equal(f[[1]],v/12)
  expect_equal(attr(f,"logLik"),0)
  expect_equal(dml(v,f),1)
 }
})

test_that("tbinom: all estimated-bound subsets match independent conditional fits", {
 set.seed(66)
 samples<-c(lapply(c(20,100),function(n)extraDistr::rtbinom(n,12,.4,0,10)),list(c(1,2,2),rep(3,4),c(0,0,1)))
 for(x in samples)for(fa in c(TRUE,FALSE))for(fb in c(TRUE,FALSE))for(fp in c(TRUE,FALSE)) {
  a<-if(fa) -1 else NULL; b<-if(fb) 12 else NULL
  aa<-if(fa) -1 else min(x)-1; bb<-if(fb)12 else max(x)
  # Edge-only data with an unattained probability boundary are covered separately.
  if(!fp && length(unique(x))==1 && aa<min(x)-1 && bb==max(x) && max(x)<12)next
  if(!fp && length(unique(x))==1 && aa==min(x)-1 && min(x)>0 && bb>max(x))next
  f<-mltbinom(x,a=a,b=b,size=12,prob=if(fp).4 else NULL)
  ll<-function(p) {
   # Enumerate the reference binomial masses: CDF subtraction in dtbinom
   # loses singleton probabilities near the probability endpoints.
   k<-0:12;lp<-dbinom(k,12,p,log=TRUE);w<-lp[k>aa & k<=bb]
   logz<-max(w)+log(sum(exp(w-max(w))))
   sum(dbinom(x,12,p,log=TRUE))-length(x)*logz
  }
  oracle<-if(fp)ll(.4)else max(vapply(c(.05,.4,.9),function(start)-optim(start,function(p)-ll(p),method="L-BFGS-B",lower=1e-8,upper=1-1e-8)$value,0.))
  expect_equal(attr(f,"logLik"),oracle,tolerance=1e-6)
  expect_identical(unname(f["a"]),as.numeric(aa));expect_identical(unname(f["b"]),as.numeric(bb))
  expect_setequal(attr(f,"fixed"),c("size",if(fa)"a",if(fb)"b",if(fp)"prob"))
 }
 expect_error(mltbinom(1:3,a=NULL,b=NULL,size=NULL),"Use `mltpois")
 expect_error(mltbinom(rep(3,4),a=NULL,b=NULL,size=12,prob=0),"Invalid")
})

test_that("tbinom: fixed-probability free-size profiles exhaust bounds subsets", {
 set.seed(93)
 for(x in c(lapply(c(20,100),function(n)extraDistr::rtbinom(n,12,.4,0,10)),list(c(1,2,2),c(1,3,3)))) {
  for(fa in c(TRUE,FALSE))for(fb in c(TRUE,FALSE)) {
   a<-if(fa)0 else NULL;b<-if(fb)10 else NULL
   aa<-if(fa)0 else min(x)-1;bb<-if(fb)10 else max(x)
   f<-mltbinom(x,a=a,b=b,size=NULL,prob=.4)
   oracle<-max(vapply(seq(max(x),200),function(k)sum(extraDistr::dtbinom(x,k,.4,aa,bb,log=TRUE)),0.))
   expect_equal(attr(f,"logLik"),oracle,tolerance=1e-7)
   expect_identical(unname(f["prob"]),.4)
   expect_setequal(attr(f,"fixed"),c(if(fa)"a",if(fb)"b","prob"))
  }
 }
 f<-mltbinom(1:4,a=NULL,b=Inf,size=NULL,prob=.4)
 oracle<-max(vapply(4:200,function(k)sum(extraDistr::dtbinom(1:4,k,.4,0,Inf,log=TRUE)),0.))
 expect_equal(attr(f,"logLik"),oracle,tolerance=1e-7)
 expect_error(mltbinom(rep(3,4),a=-1,b=NULL,size=NULL,prob=.4),"point mass")
 expect_equal(attr(mltbinom(rep(3,4),a=NULL,b=NULL,size=NULL,prob=.4),"logLik"),0)
 expect_error(mltbinom(1:3,a=NULL,b=NULL,size=NA_real_,prob=.4),"size")
})

test_that("tbinom: all four free-size probability profiles match reference oracles", {
  set.seed(9301)
  samples <- c(lapply(c(20,100), function(n) extraDistr::rtbinom(n,12,.4,0,10)),
               list(c(1,1,1,1,2,3,8,10), c(2,3,4), rep(c(2,3),10), rep(3,20)),
               lapply(c(20,100), function(n) extraDistr::rtpois(n,3,0,10)))
  for (x in samples) for (fa in c(FALSE,TRUE)) for (fb in c(FALSE,TRUE)) {
    a <- if (fa) 0 else NULL; b <- if (fb) 10 else NULL
    aa <- if (fa) 0 else min(x)-1; bb <- if (fb) 10 else max(x)
    grid <- unique(c(seq(max(x),160), round(exp(seq(log(161),log(5000),length.out=25)))))
    ll <- function(N) optimize(function(eta) {
      value <- sum(extraDistr::dtbinom(x,N,plogis(eta),aa,bb,log=TRUE))
      if(is.finite(value)) value else -1e100
    }, c(-25,25),maximum=TRUE,tol=1e-10)$objective
    # The exact limit density, optimized independently of any finite size.
    face <- optimize(function(eta) {
      value <- sum(extraDistr::dtpois(x,exp(eta),aa,bb,log=TRUE))
      if(is.finite(value)) value else -1e100
    }, c(-15,15),maximum=TRUE,tol=1e-10)$objective
    # Constant data have the exact upper bound zero, attained by N=max(x),
    # p=1. dtbinom can report positive log masses when its CDF subtraction
    # rounds a rare conditioning probability; do not optimize that artifact.
    finite <- if(length(unique(x))==1) 0 else max(vapply(grid,ll,0.))
    saturated <- length(unique(x))==1 || bb-max(0,floor(aa)+1)==1
    if (!saturated && finite < face-1e-7) {
      expect_error(mltbinom(x,a=a,b=b,size=NULL),"Use `mltpois")
    } else {
      f <- mltbinom(x,a=a,b=b,size=NULL)
      expect_gte(attr(f,"logLik"),finite-1e-6)
      expect_equal(attr(f,"logLik"),sum(extraDistr::dtbinom(x,f[["size"]],f[["prob"]],aa,bb,log=TRUE)),tolerance=1e-7)
      expect_equal(f[["size"]],floor(f[["size"]]))
      expect_identical(unname(f[c("a","b")]),as.numeric(c(aa,bb)))
      expect_setequal(attr(f,"fixed"),c(character(),if(fa)"a",if(fb)"b"))
      if (!saturated) expect_gt(attr(f,"logLik"),face)
    }
  }
})

test_that("tbinom: uncapped size, exact attained faces and invalid free designs", {
  x <- c(0,rep(1000,1000))
  f <- mltbinom(x,a=-Inf,b=Inf,size=NULL)
  expect_gt(f[["size"]],1e5)
  expect_equal(f[["prob"]],mean(x)/f[["size"]],tolerance=1e-14)
  expect_equal(attr(f,"logLik"),sum(dbinom(x,f[["size"]],f[["prob"]],log=TRUE)),tolerance=1e-6)
  for (a in list(NULL,0)) for (b in list(NULL,10)) {
    f <- mltbinom(rep(3,20),a=a,b=b,size=NULL)
    expect_equal(f[["size"]],3); expect_equal(f[["prob"]],1)
    expect_equal(attr(f,"logLik"),0)
  }
  f <- mltbinom(rep(c(2,3),10),a=1,b=3,size=NULL)
  expect_equal(f[["size"]],3)
  expect_equal(attr(f,"logLik"),-20*log(2))
  expect_error(mltbinom(1:4,a=2,b=10,size=NULL),"Invalid")
  expect_error(mltbinom(1:4,a=0,b=3,size=NULL),"Invalid")
  expect_error(mltbinom(1:4,a=NA_real_,b=10,size=NULL),"bounds")
  expect_error(mltbinom(1:4,a=Inf,b=Inf,size=NULL),"Invalid")
  expect_error(mltbinom(c(0,0,0,9),a=-1,b=Inf,size=NULL),"Use `mltpois")
  expect_error(mltbinom(c(0,0,0,9),a=-1,b=10,size=NULL),"Use `mltpois")
})

test_that("tbinom: a peak beyond the dense grid is refined over integer size", {
  x <- c(0,rep(100,100))
  f <- mltbinom(x,a=-1,b=2000,size=NULL)
  expect_gt(f[["size"]],1000)
  # Truncation above 2000 has negligible probability at this mean; the
  # reference still evaluates the actual conditioning event independently.
  for(N in c(200,500,1000,2000,3350,5000,10000)) {
    oracle <- sum(extraDistr::dtbinom(x,N,mean(x)/N,-1,2000,log=TRUE))
    expect_gte(attr(f,"logLik"),oracle-1e-7)
  }
})

test_that("tbinom: fractional held bounds are exact and work failures stop", {
  f <- mltbinom(c(2,3,3,4),a=1.25,b=4.75,size=NULL)
  expect_identical(unname(f[c("a","b")]),c(1.25,4.75))
  expect_setequal(attr(f,"fixed"),c("a","b"))
  expect_error(mltbinom(c(2000001,2000002),a=2000000,b=2000005,size=NULL),"work budget")
})
