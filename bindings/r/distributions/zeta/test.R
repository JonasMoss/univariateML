test_that("zeta fits agree with a slow independent oracle", {
  skip_unless_tier("full")
  samples <- list(c(1,1,2,3,10,100),rep(2,8),c(rep(1,1000),2),c(1,1000000000))
  for (x in samples) {
    objective <- function(t) {
      shape <- 1+exp(t)
      # Independent 10000-term Euler--Maclaurin normalizer.
      z <- sum((1:10000)^(-shape)) + 10000^(1-shape)/(shape-1) - .5*10000^(-shape) + shape/12*10000^(-shape-1)
      shape*sum(log(x))+length(x)*log(z)
    }
    oracle <- optimize(objective,c(-8,12),tol=1e-10)
    fit <- mlzeta(x)
    expect_equal(log(fit[["s"]]-1),oracle$minimum,tolerance=2e-6)
    expect_equal(as.numeric(logLik(fit)),-oracle$objective,tolerance=1e-8)
    expect_equal(as.numeric(logLik(fit)),sum(dml(x,fit,log=TRUE)),tolerance=1e-12)
  }
  expect_error(mlzeta(rep(1,4)),"point mass at one")
  expect_error(mlzeta(c(0,2)))
  expect_error(mlzeta(c(1,2),iterlim=1),"converge")
  expect_error(mlzeta(c(1,2),reltol=0),"positive")
})
test_that("zeta density sums, direct tails and inverse inequalities agree", {
  for (shape in c(1.1,2.0,10.0)) {
    k <- 1:1000
    expect_equal(dzeta(x = k, s = shape)/dzeta(x = 1, s = shape),k^(-shape),tolerance=1e-12)
    expect_equal(pzeta(q = c(1, 10, 1000) + 0.75, s = shape),cumsum(dzeta(x = k, s = shape))[c(1,10,1000)],tolerance=2e-11)
    for (tail in c(TRUE,FALSE)) for (lg in c(TRUE,FALSE)) {
      u <- c(.001,.1,.5,.9,.999)
      q <- qzeta(p = if (lg) log(u) else u, s = shape, lower.tail = tail, log.p = lg)
      use <- q<2^53
      f <- pzeta(q = q[use], s = shape, lower.tail = tail, log.p = lg); prev <- pzeta(q = q[use] - 1, s = shape, lower.tail = tail, log.p = lg)
      u <- if (lg) log(u) else u
      expect_true(all(if (tail) f>=u[use] & prev<u[use] else f<=u[use] & prev>u[use]))
    }
    expect_true(is.finite(pzeta(q = 1e+200, s = shape, lower.tail = FALSE, log.p = TRUE)))
  }
  expect_equal(qzeta(p = c(0, 1), s = 2),c(1,Inf))
  expect_equal(qzeta(p = c(-Inf, 0), s = 2, lower.tail = FALSE, log.p = TRUE),c(Inf,1))
  expect_equal(pzeta(q = c(-Inf, Inf), s = 2),c(0,1))
  expect_equal(dzeta(x = c(0, Inf), s = 2),c(0,0))
})
test_that("zeta recycling, warnings, RNG and model generics work", {
  expect_warning(expect_equal(dzeta(x = 0.5, s = 2),0),"non-integer")
  expect_warning(expect_true(is.nan(dzeta(x = 1, s = -1))),"NaNs")
  expect_warning(expect_true(is.nan(qzeta(p = 2, s = 2))),"NaNs")
  expect_identical(dzeta(x = NA_real_, s = 2),NA_real_)
  expect_true(is.nan(pzeta(q = NaN, s = 2)))
  expect_equal(dzeta(x = 1:5, s = c(2, 3)),mapply(dzeta,1:5,c(2,3,2,3,2)))
  expect_length(dzeta(x = numeric(), s = 2),0)
  expect_length(rzeta(n = 0, s = 2),0)
  expect_length(rzeta(n = 3, s = 2:5),3)
  set.seed(78); x <- rzeta(n = 10000, s = 2)
  set.seed(78); expect_identical(x,rzeta(n = 10000, s = 2))
  observed <- tabulate(pmin(x,6),nbins=6)
  probs <- c(dzeta(x = 1:5, s = 2),pzeta(q = 5, s = 2, lower.tail = FALSE))
  expect_lt(sum((observed-10000*probs)^2/(10000*probs)),30)
  fit <- mlzeta(x)


  expect_length(rml(20,fit),20)
  expect_length(bootstrapml(fit,reps=3,reducer=identity),3)
  expect_s3_class(model_select(x,models="zeta"),"univariateML")
  grDevices::pdf(tempfile(fileext=".pdf")); on.exit(grDevices::dev.off())
  expect_error(plot(fit),NA)
})

test_that("zeta has correct huge tails and the infinite Zipf face", {
  expect_equal(dzeta(x = 1, s = 2),6/pi^2,tolerance=1e-14)
  expect_equal(pzeta(q = 1e+200, s = 2, lower.tail = FALSE, log.p = TRUE),-log(1e200)-log(pi^2/6),tolerance=1e-14)
  expect_equal(pzeta(q = 1e+100, s = 3, lower.tail = FALSE, log.p = TRUE),-2*log(1e100)-log(2)-log(1.2020569031595943),tolerance=1e-14)
  for (s in c(2,3,10))
    expect_equal(dzipf(x = 1:10, N = 1e+09, s = s),dzeta(x = 1:10, s = s),tolerance=2e-9)
  expect_true(is.finite(pzeta(q = 1e+308, s = 1 + 1e-12, lower.tail = FALSE, log.p = TRUE)))
  expect_true(is.finite(pzeta(q = 1, s = 1000, lower.tail = FALSE, log.p = TRUE)))
  expect_equal(qzeta(p = -1000, s = 2, lower.tail = FALSE, log.p = TRUE),Inf)
})
test_that("zeta preserves extreme log probabilities and invalid domains", {
  expect_equal(qzeta(p = -1000, s = 2, lower.tail = TRUE, log.p = TRUE),1)
  expect_true(is.finite(qzeta(p = -1e-300, s = 3, lower.tail = TRUE, log.p = TRUE)))
  expect_warning(dzeta(x = 1, s = 1),"NaNs")
  expect_warning(pzeta(q = 1, s = Inf),"NaNs")
  expect_identical(qzeta(p = 0.5, s = NA_real_),NA_real_)
  expect_error(rzeta(n = -1, s = 2),"invalid")
})

test_that("zeta held coefficient evaluates the independent likelihood", {
  set.seed(810)
  samples <- list(sample(1:8,20,TRUE),sample(1:15,100,TRUE),c(1,2),rep(3,4),rep(1,5))
  held <- 2.3
  for(x in samples) {
    f <- mlzeta(x,s=held)
    expect_identical(f[["s"]],held)
    expect_identical(attr(f,"fixed"),"s")
    expect_equal(as.numeric(logLik(f)),-held*sum(log(x))-length(x)*log(sum((1:10000)^-held)+10000^(1-held)/(held-1)-.5*10000^-held+held/12*10000^(-held-1)),tolerance=1e-10)
    expect_equal(attr(logLik(f),"df"),0L)
  }
  for(v in c(NA_real_,NaN,Inf,0,-1)) expect_error(mlzeta(c(1,2),s=v))
})
