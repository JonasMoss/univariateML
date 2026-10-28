test_that("invparalogis matches a multistart actuar oracle", {
  set.seed(871)
  x <- actuar::rinvparalogis(300, shape=1.3, scale=2.7)
  fit <- mlinvparalogis(x, gradtol=1e-10, iterlim=1000)
  objective <- function(p) -sum(actuar::dinvparalogis(x, shape=exp(p[1]), scale=exp(p[2]), log=TRUE))
  oracles <- lapply(c(.4,1,3), function(s) optim(rep(log(s), 2), objective, method="L-BFGS-B", lower=c(-5,-5), upper=c(5,5), control=list(factr=1e4,maxit=2000)))
  oracle <- oracles[[which.min(vapply(oracles, function(o) o$value, 0.0))]]
  expect_equal(as.numeric(fit),exp(oracle$par),tolerance=1e-4)
  expect_equal(attr(fit,"logLik"),-oracle$value,tolerance=1e-8)
})
test_that("invparalogis fixed scale matches actuar oracle", {
 x<-c(.03,.1,.4,.7,1,2,4,12)
 held<-3.7
 fit<-mlinvparalogis(x,scale=held)
 objective<-function(p) -sum(actuar::dinvparalogis(x,shape=exp(p),scale=held,log=TRUE))
 oracles<-lapply(c(.2,1,4),function(s) optim(log(s),objective,method="BFGS",control=list(reltol=1e-13)))
 oracle<-oracles[[which.min(vapply(oracles,function(o)o$value,0.0))]]
 expect_equal(unname(fit["shape"]),exp(oracle$par),tolerance=1e-6)
 expect_identical(unname(fit["scale"]),held)
 expect_identical(attr(fit,"fixed"),"scale")
 expect_equal(attr(logLik(fit),"df"),1)
 expect_equal(attr(fit,"logLik"),-oracle$value,tolerance=1e-10)
 expect_equal(actuar::dinvparalogis(x,shape=2.3,scale=held,log=TRUE),actuar::dparalogis(1/x,shape=2.3,scale=1/held,log=TRUE)-2*log(x))
 expect_length(attr(mlinvparalogis(x),"fixed"),0)
 expect_equal(attr(logLik(mlinvparalogis(x)),"df"),2)
 for(bad in c(0,-1,Inf,NaN)) expect_error(mlinvparalogis(x,scale=bad),"positive finite")
 expect_true(is.finite(attr(mlinvparalogis(rep(1,10),scale=held),"logLik")))
})
