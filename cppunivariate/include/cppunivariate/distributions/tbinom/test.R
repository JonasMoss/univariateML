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
  expect_error(mltbinom(rep(1,3),a=0,b=1,size=12),"unidentified")
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
