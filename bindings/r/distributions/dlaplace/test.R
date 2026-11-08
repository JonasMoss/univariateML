test_that("discrete Laplace exact median profile and fixed subsets", {
 set.seed(192)
 for(j in 1:20) {
  x<-rdlaplace(n = 41, location = 2, scale = 0.6)
  f<-mldlaplace(x)
  oracle<-max(vapply(seq(min(x),max(x)),function(mu)optimize(function(p)sum(ddlaplace(x = x, location = mu, scale = p, log = TRUE)),c(.00001,.99999),maximum=TRUE,tol=1e-10)$objective,0.))
  expect_equal(attr(f,"logLik"),oracle,tolerance=1e-8)
  for(mu in c(-3,2,9)) {
   g<-mldlaplace(x,location=mu)
   o<-optimize(function(p)sum(ddlaplace(x = x, location = mu, scale = p, log = TRUE)),c(.00001,.99999),maximum=TRUE)
   expect_equal(attr(g,"logLik"),o$objective,tolerance=1e-7)
  }
  g<-mldlaplace(x,scale=.4)
  expect_equal(g[[1]],median(x))
  h<-mldlaplace(x,location=2,scale=.4)
  expect_equal(attr(h,"logLik"),sum(ddlaplace(x = x, location = 2, scale = 0.4, log = TRUE)))
 }
 expect_warning(f<-mldlaplace(c(-3,4)),"smallest integer median")
 expect_equal(f[[1]],-3)
 expect_error(mldlaplace(rep(2,4)),"point mass")
 expect_silent(mldlaplace(rep(2,4),location=0))
 expect_silent(mldlaplace(rep(2,4),scale=.3))
 expect_error(mldlaplace(1:3,location=.5),"integer")
 expect_error(mldlaplace(1:3,scale=1),"scale")
 f<-mldlaplace(c(-2,0,0,1,3))
 q<-qml(c(1e-6,.1,.5,.9,1-1e-6),f)
 expect_true(all(pml(q,f)>=c(1e-6,.1,.5,.9,1-1e-6)))
 expect_true(all(pml(q-1,f)<c(1e-6,.1,.5,.9,1-1e-6)))
 expect_true(is.finite(pml(10000,f,lower.tail=FALSE,log.p=TRUE)))
 set.seed(4);a<-rml(20,f);set.seed(4);expect_identical(a,rml(20,f))
})

test_that("discrete Laplace vector wrappers retain overflowing-distance log tails", {
  x <- c(-1e308,1e308)
  scale <- 1-2^-53
  expected <- rep(-2.2204460492503132285e292,2)
  expect_equal(ddlaplace(x,location=-x,scale=scale,log=TRUE)/expected,c(1,1),tolerance=3e-15)
  expect_equal(pdlaplace(x[1],location=-x[1],scale=scale,log.p=TRUE)/expected[1],1,tolerance=3e-15)
  expect_equal(pdlaplace(x[2],location=-x[2],scale=scale,lower.tail=FALSE,log.p=TRUE)/expected[2],1,tolerance=3e-15)
})
