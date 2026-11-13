test_that("CMP Poisson and geometric members have accurate tails", {
  for (l in c(.001,.5,4,100,1e6)) {
    k <- unique(pmax(0,floor(l+c(-5,-1,0,1,5)*sqrt(l))))
    expect_equal(dcmp(x = k, lambda = l, nu = 1, log = TRUE),dpois(k,l,TRUE),tolerance=1e-10)
    for (tail in c(TRUE,FALSE)) {
      expect_equal(pcmp(q = k, lambda = l, nu = 1, lower.tail = tail, log.p = TRUE),ppois(k,l,tail,TRUE),tolerance=1e-9)
      u <- c(1e-12,.1,.5,.9,1-1e-12)
      z <- qcmp(p = u, lambda = l, nu = 1, lower.tail = tail)
      expect_equal(z[2:4],qpois(u[2:4],l,tail))
      f <- pcmp(q = z, lambda = l, nu = 1, lower.tail = tail); prev <- pcmp(q = z - 1, lambda = l, nu = 1, lower.tail = tail)
      e <- 64*.Machine$double.eps
      target <- if(tail) u*(1-e) else ifelse(1-u>4*e,u*(1+e),u)
      expect_true(all(if(tail) f>=target & prev<target else f<=target & prev>target))
    }
  }
  for (l in c(.01,.8,.999999)) for (tail in c(TRUE,FALSE)) {
    expect_equal(pcmp(q = c(0, 1, 20, 1e+06), lambda = l, nu = 0, lower.tail = tail, log.p = TRUE),pgeom(c(0,1,20,1e6),1-l,tail,TRUE),tolerance=1e-11)
    expect_equal(qcmp(p = c(0.01, 0.5, 0.99), lambda = l, nu = 0, lower.tail = tail),qgeom(c(.01,.5,.99),1-l,tail))
  }
  expect_equal(pcmp(q = 0, lambda = 1e-100, nu = 2, log.p = TRUE)/(-1e-100),1,tolerance=1e-12)
  expect_equal(dcmp(x = 0:3, lambda = 0, nu = 2),c(1,0,0,0))
  expect_equal(qcmp(p = c(0, 1), lambda = 0, nu = 2),c(0,0))
  expect_equal(qcmp(p = c(0, 1), lambda = 2, nu = 2),c(0,Inf))
  expect_equal(pcmp(q = c(-Inf, Inf), lambda = 2, nu = 2),c(0,1))
})
test_that("CMP inversion preserves both tail inequalities", {
  for (l in c(.01,2,100)) for (n in c(.5,2,20)) for (tail in c(TRUE,FALSE)) {
    u <- c(-1000,-30,-5,-1,-.01)
    z <- qcmp(p = u, lambda = l, nu = n, lower.tail = tail, log.p = TRUE)
    f <- pcmp(q = z, lambda = l, nu = n, lower.tail = tail, log.p = TRUE); prev <- pcmp(q = z - 1, lambda = l, nu = n, lower.tail = tail, log.p = TRUE)
    target <- u*if(tail) (1+64*.Machine$double.eps) else (1-64*.Machine$double.eps)
    expect_true(all(if(tail) f>=target & prev<target else f<=target & prev>target))
  }
})
test_that("CMP fitting agrees with a slow independent series oracle", {
  skip_unless_tier("full")
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
 expect_equal(as.numeric(mlcmp(c(0,0))),c(0,1))
 expect_error(mlcmp(c(2,3)),"adjacent two-point")
 expect_equal(mlcmp(c(0,0,0,10))[["nu"]],0)
 expect_error(mlcmp(c(0,1),lambda=2),"adjacent two-point")
 expect_error(mlcmp(x,nu=-1),"nonnegative")
 expect_error(mlcmp(x,lambda=NA_real_),"nonnegative")
})
test_that("CMP recycling, RNG and model generics work", {
 expect_warning(expect_equal(dcmp(x = 0.5, lambda = 2, nu = 1),0),"non-integer")
 expect_warning(expect_true(is.nan(dcmp(x = 1, lambda = 1, nu = 0))),"NaNs")
 expect_warning(expect_true(is.nan(qcmp(p = 2, lambda = 2, nu = 1))),"NaNs")
 expect_identical(dcmp(x = NA_real_, lambda = 2, nu = 1),NA_real_)
 expect_equal(dcmp(x = 0:3, lambda = c(1, 2), nu = 1),dpois(0:3,c(1,2)))
 expect_length(dcmp(x = numeric(), lambda = 2, nu = 1),0)
 expect_length(rcmp(n = 0, lambda = 2, nu = 1),0)
 expect_length(rcmp(n = 3, lambda = 1:5, nu = 1),3)
 set.seed(78); x <- rcmp(n = 100, lambda = 4, nu = 2)
 set.seed(78); expect_identical(x,rcmp(n = 100, lambda = 4, nu = 2))
 f <- mlcmp(x,nu=2)


 expect_length(rml(20,f),20)
 b <- bootstrapml(f,reps=3,reducer=identity)
 expect_equal(dim(b),c(2L,3L))
 expect_equal(as.numeric(b[2,]),rep(2,3))
 expect_s3_class(model_select(x,models="cmp"),"univariateML")
 grDevices::pdf(tempfile(fileext=".pdf")); on.exit(grDevices::dev.off())
 expect_error(plot(f),NA)
})

test_that("CMP returns held rates without log-exp rounding", {
  f <- mlcmp(c(0,1,2),lambda=.13,nu=1.3)
  expect_identical(f[["lambda"]],.13)
  expect_identical(f[["nu"]],1.3)
  expect_setequal(attr(f,"fixed"),c("lambda","nu"))
})

test_that("P3 CMP boundaries respect all holds and geometric normalizability", {
  for(nu in c(0,1,2)) {
    f<-mlcmp(c(0,0),lambda=0,nu=nu)
    expect_equal(as.numeric(f),c(0,nu)); expect_equal(attr(f,"logLik"),0)
    expect_error(mlcmp(c(0,1),lambda=0,nu=nu))
  }
  expect_error(mlcmp(c(0,1),lambda=1,nu=0))
  x<-c(0,0,0,10); f<-mlcmp(x)
  expect_equal(as.numeric(f),c(2.5/3.5,0))
  expect_equal(attr(f,"logLik"),sum(dgeom(x,1/3.5,log=TRUE)))
  f<-mlcmp(x,lambda=.5)
  expect_equal(as.numeric(f),c(.5,0))
  expect_equal(attr(f,"logLik"),sum(dgeom(x,.5,log=TRUE)))
})

test_that("CMP acceptance is binding neutral and stops unchecked loose solves", {
  x <- c(0,0,1,1,2,2,3,4)
  expect_error(mlcmp(x,reltol=.1),class="univariateML_numerical")
  for (args in list(list(),list(lambda=2),list(nu=1),list(lambda=2,nu=1))) {
    f <- do.call(mlcmp,c(list(x),args))
    expect_equal(as.numeric(logLik(f)),sum(dml(x,f,log=TRUE)),tolerance=1e-10)
  }
  expect_equal(mlcmp(c(0,0,0,10),lambda=.7)[["nu"]],0)
})

test_that("CMP acceptance budget exposes its witness and responds to iterlim", {
  x <- c(0,0,0,20000)
  stopped <- tryCatch(mlcmp(x,lambda=5000/5001,iterlim=1),error=identity)
  expect_s3_class(stopped,"univariateML_budget")
  expect_match(conditionMessage(stopped),"Raising iterlim")
  expect_equal(nrow(stopped$candidates),1)
  expect_true(is.finite(stopped$candidates$loglik_lower))
  expect_equal(mlcmp(x,lambda=5000/5001,iterlim=10)[["nu"]],0)
})

test_that("CMP huge mode is numerical without iterlim advice", {
  e <- tryCatch(mlcmp(c(0,1,2),lambda=exp(100),nu=.1),error=identity)
  expect_s3_class(e,"univariateML_numerical")
  expect_false(inherits(e,"univariateML_budget"))
  expect_false(grepl("Raising iterlim",conditionMessage(e)))
  expect_warning(expect_true(is.nan(dcmp(1,exp(100),.1))),"NaNs")
})

test_that("fully held CMP evaluator exhaustion is numerical at every iterlim", {
  e <- tryCatch(mlcmp(c(0,1,2),lambda=.999999,nu=1e-12,iterlim=1),error=identity)
  expect_s3_class(e,"univariateML_numerical")
  expect_false(inherits(e,"univariateML_budget"))
  expect_false(grepl("Raising iterlim",conditionMessage(e)))
})
