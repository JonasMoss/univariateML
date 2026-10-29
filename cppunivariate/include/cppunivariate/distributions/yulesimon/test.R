test_that("yulesimon fits agree with a slow independent oracle", {
  samples <- list(c(1,1,2,3,10,100),rep(2,8),c(rep(1,1000),2),c(1,1000000000))
  for (x in samples) {
    objective <- function(t) {
      shape <- exp(t)
      # Base R log-beta is independent of the C++ gamma increment.
      NULL
      -sum(log(shape)+lbeta(x,shape+1))
    }
    oracle <- optimize(objective,c(-8,12),tol=1e-10)
    fit <- mlyulesimon(x)
    expect_equal(log(fit[["rho"]]),oracle$minimum,tolerance=2e-5)
    expect_equal(as.numeric(logLik(fit)),-oracle$objective,tolerance=1e-8)
    expect_equal(as.numeric(logLik(fit)),sum(dml(x,fit,log=TRUE)),tolerance=1e-12)
  }
  expect_error(mlyulesimon(rep(1,4)),"point mass at one")
  expect_error(mlyulesimon(c(0,2)))
  expect_error(mlyulesimon(c(1,2),iterlim=1),"converge")
  expect_error(mlyulesimon(c(1,2),reltol=0),"positive")
})
test_that("yulesimon density sums, direct tails and inverse inequalities agree", {
  for (shape in c(.1,1.0,10.0)) {
    k <- 1:1000
    mass <- exp(log(shape)+lbeta(k,shape+1))
    expect_equal(dyulesimon(k,shape),mass,tolerance=1e-11)
    expect_equal(pyulesimon(c(1,10,1000)+.75,shape),cumsum(dyulesimon(k,shape))[c(1,10,1000)],tolerance=2e-11)
    for (tail in c(TRUE,FALSE)) for (lg in c(TRUE,FALSE)) {
      u <- c(.001,.1,.5,.9,.999)
      q <- qyulesimon(if (lg) log(u) else u,shape,tail,lg)
      use <- q<2^53
      f <- pyulesimon(q[use],shape,tail,lg); prev <- pyulesimon(q[use]-1,shape,tail,lg)
      u <- if (lg) log(u) else u
      expect_true(all(if (tail) f>=u[use] & prev<u[use] else f<=u[use] & prev>u[use]))
    }
    expect_true(is.finite(pyulesimon(1e200,shape,FALSE,TRUE)))
  }
  expect_equal(qyulesimon(c(0,1),2),c(1,Inf))
  expect_equal(qyulesimon(c(-Inf,0),2,FALSE,TRUE),c(Inf,1))
  expect_equal(pyulesimon(c(-Inf,Inf),2),c(0,1))
  expect_equal(dyulesimon(c(0,Inf),2),c(0,0))
})
test_that("yulesimon recycling, warnings, RNG and model generics work", {
  expect_warning(expect_equal(dyulesimon(.5,2),0),"non-integer")
  expect_warning(expect_true(is.nan(dyulesimon(1,-1))),"NaNs")
  expect_warning(expect_true(is.nan(qyulesimon(2,2))),"NaNs")
  expect_identical(dyulesimon(NA_real_,2),NA_real_)
  expect_true(is.nan(pyulesimon(NaN,2)))
  expect_equal(dyulesimon(1:5,c(2,3)),mapply(dyulesimon,1:5,c(2,3,2,3,2)))
  expect_length(dyulesimon(numeric(),2),0)
  expect_length(ryulesimon(0,2),0)
  expect_length(ryulesimon(3,2:5),3)
  set.seed(78); x <- ryulesimon(10000,2)
  set.seed(78); expect_identical(x,ryulesimon(10000,2))
  observed <- tabulate(pmin(x,6),nbins=6)
  probs <- c(dyulesimon(1:5,2),pyulesimon(5,2,FALSE))
  expect_lt(sum((observed-10000*probs)^2/(10000*probs)),30)
  fit <- mlyulesimon(x)
  expect_equal(pml(1:5,fit),pyulesimon(1:5,fit[["rho"]]))
  expect_equal(qml(c(.1,.5,.9),fit),qyulesimon(c(.1,.5,.9),fit[["rho"]]))
  expect_length(rml(20,fit),20)
  expect_length(bootstrapml(fit,reps=3,reducer=identity),3)
  expect_s3_class(model_select(x,models="yulesimon"),"univariateML")
  grDevices::pdf(tempfile(fileext=".pdf")); on.exit(grDevices::dev.off())
  expect_error(plot(fit),NA)
})

test_that("Yule--Simon equals shifted beta negative binomial", {
  for (rho in c(.01,1,10)) {
    k <- c(1,2,10,100,10000)
    expect_equal(dyulesimon(k,rho,TRUE),extraDistr::dbnbinom(k-1,size=1,alpha=rho,beta=1,log=TRUE),tolerance=1e-10)
  }
  b <- mlbnbinom(c(0,0,1,2,9,99),size=1,beta=1)
  y <- mlyulesimon(c(1,1,2,3,10,100))
  expect_equal(b[["alpha"]],y[["rho"]],tolerance=1e-5)
  expect_equal(as.numeric(logLik(b)),as.numeric(logLik(y)),tolerance=1e-9)
  expect_equal(pyulesimon(1e200,1,FALSE,TRUE),-log(1e200),tolerance=1e-14)
  expect_equal(pyulesimon(1e100,2,FALSE,TRUE),log(2)-2*log(1e100),tolerance=1e-14)
  expect_equal(dyulesimon(1e100,1e-300,TRUE),log(1e-300)-log(1e100),tolerance=1e-14)
  expect_equal(pyulesimon(1e100,1e-300)/(1e-300*(log(1e100)+0.5772156649015329)),1,tolerance=1e-12)
})
test_that("Yule--Simon preserves extreme log probabilities and invalid domains", {
  expect_equal(pyulesimon(1,1e-300,FALSE,TRUE)/(-1e-300),1,tolerance=1e-14)
  expect_true(is.finite(qyulesimon(-1e-300,1,TRUE,TRUE)))
  expect_true(is.finite(qyulesimon(-1000,2,FALSE,TRUE)))
  expect_equal(qyulesimon(-1000,1e-300,TRUE,TRUE),1)
  expect_warning(dyulesimon(1,0),"NaNs")
  expect_warning(pyulesimon(1,Inf),"NaNs")
  expect_identical(qyulesimon(.5,NA_real_),NA_real_)
  expect_error(ryulesimon(-1,2),"invalid")
})
