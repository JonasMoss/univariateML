test_that("yulesimon fits agree with a slow independent oracle", {
  samples <- list(c(1,1,2,3,10,100),rep(2,8),c(rep(1,1000),2),c(1,1000000000))
  for (x in samples) {
    objective <- function(t) {
      shape <- exp(t)
      # Base R log-beta is independent of the C++ gamma increment.

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
    expect_equal(dyulesimon(x = k, rho = shape),mass,tolerance=1e-11)
    expect_equal(pyulesimon(q = c(1, 10, 1000) + 0.75, rho = shape),cumsum(dyulesimon(x = k, rho = shape))[c(1,10,1000)],tolerance=2e-11)
    for (tail in c(TRUE,FALSE)) for (lg in c(TRUE,FALSE)) {
      u <- c(.001,.1,.5,.9,.999)
      q <- qyulesimon(p = if (lg) log(u) else u, rho = shape, lower.tail = tail, log.p = lg)
      use <- q<2^53
      f <- pyulesimon(q = q[use], rho = shape, lower.tail = tail, log.p = lg); prev <- pyulesimon(q = q[use] - 1, rho = shape, lower.tail = tail, log.p = lg)
      u <- if (lg) log(u) else u
      expect_true(all(if (tail) f>=u[use] & prev<u[use] else f<=u[use] & prev>u[use]))
    }
    expect_true(is.finite(pyulesimon(q = 1e+200, rho = shape, lower.tail = FALSE, log.p = TRUE)))
  }
  expect_equal(qyulesimon(p = c(0, 1), rho = 2),c(1,Inf))
  expect_equal(qyulesimon(p = c(-Inf, 0), rho = 2, lower.tail = FALSE, log.p = TRUE),c(Inf,1))
  expect_equal(pyulesimon(q = c(-Inf, Inf), rho = 2),c(0,1))
  expect_equal(dyulesimon(x = c(0, Inf), rho = 2),c(0,0))
})
test_that("yulesimon recycling, warnings, RNG and model generics work", {
  expect_warning(expect_equal(dyulesimon(x = 0.5, rho = 2),0),"non-integer")
  expect_warning(expect_true(is.nan(dyulesimon(x = 1, rho = -1))),"NaNs")
  expect_warning(expect_true(is.nan(qyulesimon(p = 2, rho = 2))),"NaNs")
  expect_identical(dyulesimon(x = NA_real_, rho = 2),NA_real_)
  expect_true(is.nan(pyulesimon(q = NaN, rho = 2)))
  expect_equal(dyulesimon(x = 1:5, rho = c(2, 3)),mapply(dyulesimon,1:5,c(2,3,2,3,2)))
  expect_length(dyulesimon(x = numeric(), rho = 2),0)
  expect_length(ryulesimon(n = 0, rho = 2),0)
  expect_length(ryulesimon(n = 3, rho = 2:5),3)
  set.seed(78); x <- ryulesimon(n = 10000, rho = 2)
  set.seed(78); expect_identical(x,ryulesimon(n = 10000, rho = 2))
  observed <- tabulate(pmin(x,6),nbins=6)
  probs <- c(dyulesimon(x = 1:5, rho = 2),pyulesimon(q = 5, rho = 2, lower.tail = FALSE))
  expect_lt(sum((observed-10000*probs)^2/(10000*probs)),30)
  fit <- mlyulesimon(x)


  expect_length(rml(20,fit),20)
  expect_length(bootstrapml(fit,reps=3,reducer=identity),3)
  expect_s3_class(model_select(x,models="yulesimon"),"univariateML")
  grDevices::pdf(tempfile(fileext=".pdf")); on.exit(grDevices::dev.off())
  expect_error(plot(fit),NA)
})

test_that("Yule--Simon equals shifted beta negative binomial", {
  for (rho in c(.01,1,10)) {
    k <- c(1,2,10,100,10000)
    expect_equal(dyulesimon(x = k, rho = rho, log = TRUE),dbnbinom(x = k - 1, size = 1, alpha = rho, beta = 1, log = TRUE),tolerance=1e-10)
  }
  b <- mlbnbinom(c(0,0,1,2,9,99),size=1,beta=1)
  y <- mlyulesimon(c(1,1,2,3,10,100))
  expect_equal(b[["alpha"]],y[["rho"]],tolerance=1e-5)
  expect_equal(as.numeric(logLik(b)),as.numeric(logLik(y)),tolerance=1e-9)
  expect_equal(pyulesimon(q = 1e+200, rho = 1, lower.tail = FALSE, log.p = TRUE),-log(1e200),tolerance=1e-14)
  expect_equal(pyulesimon(q = 1e+100, rho = 2, lower.tail = FALSE, log.p = TRUE),log(2)-2*log(1e100),tolerance=1e-14)
  expect_equal(dyulesimon(x = 1e+100, rho = 1e-300, log = TRUE),log(1e-300)-log(1e100),tolerance=1e-14)
  expect_equal(pyulesimon(q = 1e+100, rho = 1e-300)/(1e-300*(log(1e100)+0.5772156649015329)),1,tolerance=1e-12)
})
test_that("Yule--Simon preserves extreme log probabilities and invalid domains", {
  expect_equal(pyulesimon(q = 1, rho = 1e-300, lower.tail = FALSE, log.p = TRUE)/(-1e-300),1,tolerance=1e-14)
  expect_true(is.finite(qyulesimon(p = -1e-300, rho = 1, lower.tail = TRUE, log.p = TRUE)))
  expect_true(is.finite(qyulesimon(p = -1000, rho = 2, lower.tail = FALSE, log.p = TRUE)))
  expect_equal(qyulesimon(p = -1000, rho = 1e-300, lower.tail = TRUE, log.p = TRUE),1)
  expect_warning(dyulesimon(x = 1, rho = 0),"NaNs")
  expect_warning(pyulesimon(q = 1, rho = Inf),"NaNs")
  expect_identical(qyulesimon(p = 0.5, rho = NA_real_),NA_real_)
  expect_error(ryulesimon(n = -1, rho = 2),"invalid")
})

test_that("yulesimon held coefficient evaluates the independent likelihood", {
  set.seed(810)
  samples <- list(sample(1:8,20,TRUE),sample(1:15,100,TRUE),c(1,2),rep(3,4),rep(1,5))
  held <- 2.3
  for(x in samples) {
    f <- mlyulesimon(x,rho=held)
    expect_identical(f[["rho"]],held)
    expect_identical(attr(f,"fixed"),"rho")
    expect_equal(as.numeric(logLik(f)),sum(log(held)+lbeta(x,held+1)),tolerance=1e-10)
    expect_equal(attr(logLik(f),"df"),0L)
  }
  for(v in c(NA_real_,NaN,Inf,0,-1)) expect_error(mlyulesimon(c(1,2),rho=v))
})
