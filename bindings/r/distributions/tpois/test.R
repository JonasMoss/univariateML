test_that("tpois agrees with independent reference likelihood searches", {
  set.seed(981)
  for (j in 1:20) {
    x <- rtpois(n = 40, lambda = 3, a = 0, b = 10)
    f <- mltpois(x,a=0,b=10)
    objective <- function(t) sum(dtpois(x = x, lambda = exp(t), a = 0, b = 10, log = TRUE))
    o <- optimize(objective,c(-8,8),maximum=TRUE,tol=1e-10)
    expect_equal(attr(f,"logLik"),o$objective,tolerance=1e-8)
    expect_equal(sum(dml(x,f,log=TRUE)),attr(f,"logLik"),tolerance=1e-10)
  }
  expect_error(mltpois(rep(1,3),a=0,b=10),"point mass")
  expect_error(mltpois(rep(10,3),a=0,b=10),"point mass")
  expect_error(mltpois(c(0,2),a=0,b=10),"outside")
  expect_equal(mltpois(rep(1,3),a=0,b=1)[["lambda"]],1)
  expect_error(mltpois(1:3),"requires")
  f <- mltpois(c(1,2,3,4),a=0,b=10)

  set.seed(2); a<-rml(10,f);set.seed(2);expect_identical(a,rml(10,f))
})

test_that("tpois fixed parameters and repaired extreme tails", {
 f<-mltpois(rep(1,3),a=0,b=1,lambda=2)
 expect_equal(attr(f,"logLik"),0,tolerance=1e-12)
 expect_equal(attr(logLik(f),"df"),0)
 f<-mltpois(c(101,102,103),a=100,b=110,lambda=2)
 expect_true(all(is.finite(dml(c(101,102,103),f,log=TRUE))))
 expect_true(is.finite(pml(109,f,lower.tail=FALSE,log.p=TRUE)))
 expect_true(qml(-100,f,lower.tail=FALSE,log.p=TRUE)<=110)
})
test_that("truncated Poisson overlaps ordinary and zero truncated fits", {
 x<-c(1,1,2,3,6)
 expect_equal(mltpois(x,a=0,b=Inf)[[1]],mlztpois(x)[[1]],tolerance=1e-10)
 expect_equal(mltpois(x,a=-Inf,b=Inf)[[1]],mlpois(x)[[1]],tolerance=1e-10)
})
test_that("untruncated zero-rate boundary is attained", {
 f<-mltpois(rep(0,3),a=-Inf,b=Inf)
 expect_equal(f[[1]],0)
 expect_equal(attr(f,"logLik"),0)
 expect_equal(dml(c(0,1),f),c(1,0))
})

test_that("truncated Poisson sweeps every hold against a multistart oracle", {
  skip_unless_tier("full")
  set.seed(813)
  samples <- list(sample(1:7,20,TRUE),sample(1:7,100,TRUE),c(1,3),c(1,1,2,4),c(0,1,0,1))
  for(x in samples) for(mask in 0:7) {
    hl <- bitwAnd(mask,1)>0; ha <- bitwAnd(mask,2)>0; hb <- bitwAnd(mask,4)>0
    a <- if(ha) -1 else min(x)-1
    b <- if(hb) 10 else max(x)
    args <- list(x=x,a=if(ha) a else NULL,b=if(hb) b else NULL)
    if(hl)args$lambda <- 2.3
    f <- do.call(mltpois,args)
    # Independent Poisson log weights avoid external CDF cancellation.
    ll <- function(t,aa=a,bb=b) {
      w <- dpois(seq(max(0,floor(aa)+1),floor(bb)),exp(t),log=TRUE)
      z <- max(w)+log(sum(exp(w-max(w))))
      sum(dpois(x,exp(t),log=TRUE)-z)
    }
    oracle <- if(hl) ll(log(2.3)) else max(vapply(c(-2,0,2),function(start)
      -optim(start,function(t) -ll(t),method="L-BFGS-B",lower=-8,upper=8,
             control=list(factr=1e4,pgtol=1e-9))$value,0.))
    expect_equal(as.numeric(logLik(f)),oracle,tolerance=1e-8)
    expect_identical(f[["a"]],as.double(a))
    expect_identical(f[["b"]],as.double(b))
    expect_setequal(attr(f,"fixed"),c(character(),if(ha)"a",if(hb)"b",if(hl)"lambda"))
    if(hl)expect_identical(f[["lambda"]],2.3)
    # Enlarging either free support bound cannot improve the likelihood.
    expect_gte(as.numeric(logLik(f)),ll(log(f[["lambda"]]),a-1,b+1)-1e-8)
  }
})
test_that("truncated Poisson null bounds, singleton and escape paths", {
  for(k in c(0,3)) {
    f <- mltpois(rep(k,3),a=NULL,b=NULL)
    expect_equal(unname(coef(f)),c(1,k-1,k))
    expect_equal(as.numeric(logLik(f)),0)
    expect_equal(attr(f,"fixed"),character())
  }
  expect_error(mltpois(rep(3,3),a=-1,b=NULL),"highest")
  expect_error(mltpois(rep(3,3),a=NULL,b=10),"lowest")
  expect_equal(mltpois(rep(0,3),a=NULL,b=10)[["lambda"]],0)
  expect_equal(as.numeric(logLik(mltpois(rep(0,3),a=NULL,b=NULL,lambda=0))),0)
  expect_error(mltpois(rep(3,3),a=NULL,b=NULL,lambda=0),"Invalid")
  for(args in list(list(a=2,b=NULL),list(a=NULL,b=2),list(a=NA_real_,b=10),list(a=-1,b=NaN),list(a=-1,b=10,lambda=NA_real_),list(a=-1,b=10,lambda=-1),list(a=-1,b=10,lambda=Inf)))
    expect_error(do.call(mltpois,c(list(x=1:4),args)))
})
