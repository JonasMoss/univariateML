test_that("tpois agrees with independent reference likelihood searches", {
  set.seed(981)
  for (j in 1:20) {
    x <- rtpois(n = 40, lambda = 3, a = 0, b = 10)
    f <- .modifier_tpois(x,a=0,b=10)
    objective <- function(t) sum(dtpois(x = x, lambda = exp(t), a = 0, b = 10, log = TRUE))
    o <- optimize(objective,c(-8,8),maximum=TRUE,tol=1e-10)
    expect_equal(attr(f,"logLik"),o$objective,tolerance=1e-8)
    expect_equal(sum(dml(x,f,log=TRUE)),attr(f,"logLik"),tolerance=1e-10)
  }
  expect_error(.modifier_tpois(rep(1,3),a=0,b=10),"point mass")
  expect_error(.modifier_tpois(rep(10,3),a=0,b=10),"point mass")
  expect_error(.modifier_tpois(c(0,2),a=0,b=10),"outside")
  expect_equal(.modifier_tpois(rep(1,3),a=0,b=1)[["lambda"]],1)
  expect_error(.modifier_tpois(1:3),"requires")
  f <- .modifier_tpois(c(1,2,3,4),a=0,b=10)

  set.seed(2); a<-rml(10,f);set.seed(2);expect_identical(a,rml(10,f))
})

test_that("tpois fixed parameters and repaired extreme tails", {
 f<-.modifier_tpois(rep(1,3),a=0,b=1,lambda=2)
 expect_equal(attr(f,"logLik"),0,tolerance=1e-12)
 expect_equal(attr(logLik(f),"df"),0)
 f<-.modifier_tpois(c(101,102,103),a=100,b=110,lambda=2)
 expect_true(all(is.finite(dml(c(101,102,103),f,log=TRUE))))
 expect_true(is.finite(pml(109,f,lower.tail=FALSE,log.p=TRUE)))
 expect_true(qml(-100,f,lower.tail=FALSE,log.p=TRUE)<=110)
})
test_that("truncated Poisson overlaps ordinary and zero truncated fits", {
 x<-c(1,1,2,3,6)
 expect_equal(.modifier_tpois(x,a=0,b=Inf)[[1]],.modifier_ztpois(x)[[1]],tolerance=1e-10)
 expect_equal(.modifier_tpois(x,a=-Inf,b=Inf)[[1]],mlpois(x)[[1]],tolerance=1e-10)
})
test_that("untruncated zero-rate boundary is attained", {
 f<-.modifier_tpois(rep(0,3),a=-Inf,b=Inf)
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
    f <- do.call(.modifier_tpois,args)
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
    f <- .modifier_tpois(rep(k,3),a=NULL,b=NULL)
    expect_equal(unname(coef(f)),c(1,k-1,k))
    expect_equal(as.numeric(logLik(f)),0)
    expect_equal(attr(f,"fixed"),character())
  }
  expect_error(.modifier_tpois(rep(3,3),a=-1,b=NULL),"highest")
  expect_error(.modifier_tpois(rep(3,3),a=NULL,b=10),"lowest")
  expect_equal(.modifier_tpois(rep(0,3),a=NULL,b=10)[["lambda"]],0)
  expect_equal(as.numeric(logLik(.modifier_tpois(rep(0,3),a=NULL,b=NULL,lambda=0))),0)
  expect_error(.modifier_tpois(rep(3,3),a=NULL,b=NULL,lambda=0),"Invalid")
  for(args in list(list(a=2,b=NULL),list(a=NULL,b=2),list(a=NA_real_,b=10),list(a=-1,b=NaN),list(a=-1,b=10,lambda=NA_real_),list(a=-1,b=10,lambda=-1),list(a=-1,b=10,lambda=Inf)))
    expect_error(do.call(.modifier_tpois,c(list(x=1:4),args)))
})

test_that("TASK-134 zero-rate conditional law respects the declared domain", {
  expect_equal(dtpois(0, 0, -1, 10), 1)
  expect_equal(ptpois(0, 0, -1, 10), 1)
  expect_equal(qtpois(.5, 0, -1, 10), 0)
  expect_true(is.nan(kernel_warning_value(dtpois(1, 0, 0, 10), "NaNs produced")))
  expect_true(is.nan(kernel_warning_value(ptpois(1, 0, 0, 10), "NaNs produced")))
  expect_true(is.nan(kernel_warning_value(qtpois(.5, 0, 0, 10), "NaNs produced")))
  expect_true(all(is.nan(kernel_warning_value(rtpois(3, 0, 0, 10), "NaNs produced"))))
})

test_that("TASK-134 finite conditioning designs retain quantile endpoints", {
  expect_equal(qtpois(c(0, .5, 1), 0, -1, 10), c(0, 0, 10))
  expect_equal(qtpois(c(0, 1), 0, -1, 10, lower.tail = FALSE), c(10, 0))
})

test_that("estimated large-count truncated-Poisson bounds retain logLik", {
  x <- c(1000000,1000001,1000003,1000008,1000011)
  fit <- .modifier_tpois(x,a=NULL,b=NULL)
  ll <- sum(dml(x,fit,log=TRUE))
  expect_lte(abs(as.numeric(logLik(fit))-ll),1e-10*max(1,abs(ll)))
})
