test_that("invexp matches a multistart actuar oracle", {
  set.seed(871)
  x <- actuar::rinvexp(300, rate=2.7)
  fit <- mlinvexp(x)
  objective <- function(p) -sum(actuar::dinvexp(x, rate=exp(p[1]), log=TRUE))
  oracles <- lapply(c(.4,1,3), function(s) optim(rep(log(s), 1), objective, method="BFGS", control=list(reltol=1e-12,maxit=2000)))
  oracle <- oracles[[which.min(vapply(oracles, function(o) o$value, 0.0))]]
  expect_equal(as.numeric(fit),exp(oracle$par),tolerance=1e-4)
  expect_equal(attr(fit,"logLik"),-oracle$value,tolerance=1e-8)
})
test_that("invexp rate follows actuar convention", {
 x<-c(.13,.7,2,11)
 expect_equal(actuar::dinvexp(x,rate=3,log=TRUE),dexp(1/x,rate=1/3,log=TRUE)-2*log(x))
 expect_equal(unname(coef(mlinvexp(x))),mean(1/x))
})

test_that("inverse exponential rejects unrepresentable reciprocal moments", {
  expect_error(mlinvexp(c(1,5e-324)),"precision")
})
