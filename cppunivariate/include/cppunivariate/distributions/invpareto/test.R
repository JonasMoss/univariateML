test_that("invpareto matches a multistart actuar oracle", {
  set.seed(871)
  x <- actuar::rinvpareto(300, shape=1.3, scale=2.7)
  fit <- mlinvpareto(x, reltol=1e-10, iterlim=1000)
  objective <- function(p) -sum(actuar::dinvpareto(x, shape=exp(p[1]), scale=exp(p[2]), log=TRUE))
  oracles <- lapply(c(.4,1,3), function(s) optim(rep(log(s), 2), objective, method="BFGS", control=list(reltol=1e-12,maxit=2000)))
  oracle <- oracles[[which.min(vapply(oracles, function(o) o$value, 0.0))]]
  expect_equal(as.numeric(fit),exp(oracle$par),tolerance=1e-4)
  expect_equal(attr(fit,"logLik"),-oracle$value,tolerance=1e-8)
})
test_that("invpareto mapping and limit", {
 x<-c(.13,.7,2,11)
 expect_equal(actuar::dinvpareto(x,shape=2.3,scale=3,log=TRUE),extraDistr::dlomax(1/x,lambda=3,kappa=2.3,log=TRUE)-2*log(x))
 expect_error(mlinvpareto(rep(1,10)),"Use `mlinvexp`")
})
