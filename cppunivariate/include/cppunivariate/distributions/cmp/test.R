test_that("CMP Poisson and geometric members have accurate tails", {
  for (l in c(.001,.5,4,100,1e6)) {
    k <- unique(pmax(0,floor(l+c(-5,-1,0,1,5)*sqrt(l))))
    expect_equal(dcmp(k,l,1,TRUE),dpois(k,l,TRUE),tolerance=1e-10)
    for (tail in c(TRUE,FALSE)) {
      expect_equal(pcmp(k,l,1,tail,TRUE),ppois(k,l,tail,TRUE),tolerance=1e-9)
      u <- c(1e-12,.1,.5,.9,1-1e-12)
      z <- qcmp(u,l,1,tail)
      expect_equal(z[2:4],qpois(u[2:4],l,tail))
      f <- pcmp(z,l,1,tail); prev <- pcmp(z-1,l,1,tail)
      expect_true(all(if(tail) f>=u & prev<u else f<=u & prev>u))
    }
  }
  for (l in c(.01,.8,.999999)) for (tail in c(TRUE,FALSE)) {
    expect_equal(pcmp(c(0,1,20,1e6),l,0,tail,TRUE),pgeom(c(0,1,20,1e6),1-l,tail,TRUE),tolerance=1e-11)
    expect_equal(qcmp(c(.01,.5,.99),l,0,tail),qgeom(c(.01,.5,.99),1-l,tail))
  }
  expect_equal(pcmp(0,1e-100,2,log.p=TRUE)/(-1e-100),1,tolerance=1e-12)
  expect_equal(dcmp(0:3,0,2),c(1,0,0,0))
  expect_equal(qcmp(c(0,1),0,2),c(0,0))
  expect_equal(qcmp(c(0,1),2,2),c(0,Inf))
  expect_equal(pcmp(c(-Inf,Inf),2,2),c(0,1))
})
test_that("CMP inversion preserves both tail inequalities", {
  for (l in c(.01,2,100)) for (n in c(.5,2,20)) for (tail in c(TRUE,FALSE)) {
    u <- c(-1000,-30,-5,-1,-.01)
    z <- qcmp(u,l,n,tail,TRUE)
    f <- pcmp(z,l,n,tail,TRUE); prev <- pcmp(z-1,l,n,tail,TRUE)
    expect_true(all(if(tail) f>=u & prev<u else f<=u & prev>u))
  }
})
test_that("CMP fitting agrees with a slow independent series oracle", {
 x <- c(0,1,1,2,2,2,3,3,4,5)
 objective <- function(z) {
   l <- exp(z[1]); n <- exp(z[2]); k <- 0:500
   w <- k*log(l)-n*lgamma(k+1); a <- max(w)
   -sum(x*log(l)-n*lgamma(x+1)-a-log(sum(exp(w-a))))
 }
 f <- mlcmp(x)
 o <- optim(log(c(4,1)),objective,method="BFGS",control=list(reltol=1e-13))
 expect_equal(log(as.numeric(f)),o$par,tolerance=1e-5)
 expect_equal(as.numeric(logLik(f)),-o$value,tolerance=1e-10)
 for (args in list(list(nu=1),list(lambda=4),list(lambda=4,nu=2),list(nu=0))) {
   g <- do.call(mlcmp,c(list(x),args))
   expect_setequal(attr(g,"fixed"),names(args))
   expect_equal(attr(logLik(g),"df"),2L-length(args))
   expect_equal(as.numeric(logLik(g)),sum(dml(x,g,log=TRUE)),tolerance=1e-10)
 }
 expect_equal(mlcmp(x,nu=1)[["lambda"]],mean(x),tolerance=1e-9)
 expect_error(mlcmp(c(0,0)),"point mass")
 expect_error(mlcmp(c(2,3)),"adjacent two-point")
 expect_error(mlcmp(c(0,0,0,10)),"geometric")
 expect_error(mlcmp(c(0,1),lambda=2),"adjacent two-point")
 expect_error(mlcmp(x,nu=-1),"nonnegative")
 expect_error(mlcmp(x,lambda=NA_real_),"positive")
})
test_that("CMP recycling, RNG and model generics work", {
 expect_warning(expect_equal(dcmp(.5,2,1),0),"non-integer")
 expect_warning(expect_true(is.nan(dcmp(1,1,0))),"NaNs")
 expect_warning(expect_true(is.nan(qcmp(2,2,1))),"NaNs")
 expect_identical(dcmp(NA_real_,2,1),NA_real_)
 expect_equal(dcmp(0:3,c(1,2),1),dpois(0:3,c(1,2)))
 expect_length(dcmp(numeric(),2,1),0)
 expect_length(rcmp(0,2,1),0)
 expect_length(rcmp(3,1:5,1),3)
 set.seed(78); x <- rcmp(100,4,2)
 set.seed(78); expect_identical(x,rcmp(100,4,2))
 f <- mlcmp(x,nu=2)
 expect_equal(pml(0:10,f),pcmp(0:10,f[["lambda"]],2))
 expect_equal(qml(c(.1,.5,.9),f),qcmp(c(.1,.5,.9),f[["lambda"]],2))
 expect_length(rml(20,f),20)
 b <- bootstrapml(f,reps=3,reducer=identity)
 expect_equal(dim(b),c(2L,3L))
 expect_equal(as.numeric(b[2,]),rep(2,3))
 expect_s3_class(model_select(x,models="cmp"),"univariateML")
 grDevices::pdf(tempfile(fileext=".pdf")); on.exit(grDevices::dev.off())
 expect_error(plot(f),NA)
})
