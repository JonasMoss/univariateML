context("mlzipf")

## Warning when all observations are 1 (MLE not unique)
expect_warning(
  mlzipf(c(1, 1, 1)),
  "All observations are equal to 1\\. The maximum likelihood estimator is not unique\\."
)

## qml inverts pml: F(q - 1) < p <= F(q). sads::qzipf 0.6.5 is off by one;
## see univariateML_reference_fixes and KNOWN_ISSUES.md.
test_that("qml for zipf is the discrete inverse of pml", {
  fit <- mlzipf(corbet)
  p <- c(0, 0.1, 0.4, 0.5, 0.9, 0.995, 1)
  q <- qml(p, fit)
  cdf <- c(0, pml(seq_len(fit[["N"]]), fit))
  expect_true(all(cdf[q] < p | q == 1))
  expect_true(all(p <= cdf[q + 1]))
  expect_equal(qml(log(0.4), fit, log.p = TRUE), qml(0.4, fit))
  expect_equal(qml(0.6, fit, lower.tail = FALSE), qml(0.4, fit))
  expect_warning(expect_true(all(is.nan(qml(c(-0.1, 1.1), fit)))), "NaNs")
})

test_that("Zipf returns the attained uniform face at the count ceiling", {
  x <- c(.Machine$integer.max-1,.Machine$integer.max)
  f <- mlzipf(x)
  expect_equal(f[["s"]],0)
  expect_equal(attr(f,"logLik"),-length(x)*log(max(x)))
})
test_that("large-support Zipf likelihood agrees with a slow sum oracle", {
  x <- c(rep(1,9),100000L)
  f <- mlzipf(x)
  objective <- function(t) {s<-exp(t); s*sum(log(x))+length(x)*log(sum((1:max(x))^-s))}
  o <- optimize(objective,c(-5,3),tol=1e-11)
  expect_equal(attr(f,"logLik"),-o$objective,tolerance=1e-8)
  expect_equal(f[["s"]],exp(o$minimum),tolerance=1e-6)
})

test_that("Zipf d/p/q/r agree with finite sums on tails and log scales", {
  for (N in c(1, 2, 10, 40)) for (s in c(0, .1, 1, 2, 10)) {
    w <- (1:N)^-s; w <- w/sum(w)
    expect_equal(dzipf(1:N, N, s), w, tolerance = 2e-13)
    expect_equal(dzipf(1:N, N, s, log = TRUE), log(w), tolerance = 2e-13)
    q <- seq(-.5, N+1, by=.5)
    lower <- vapply(q, function(k) sum(w[seq_len(N) <= k]), 0)
    upper <- vapply(q, function(k) sum(w[seq_len(N) > k]), 0)
    expect_equal(pzipf(q, N, s), lower, tolerance=2e-13)
    expect_equal(pzipf(q, N, s, FALSE), upper, tolerance=2e-13)
    expect_equal(pzipf(q, N, s, log.p=TRUE), log(lower), tolerance=2e-13)
    expect_equal(pzipf(q, N, s, FALSE, TRUE), log(upper), tolerance=2e-13)
    u <- c(1e-12,.01,.3,.5,.9,1-1e-12)
    for (tail in c(TRUE,FALSE)) for (lg in c(TRUE,FALSE)) {
      z <- qzipf(if (lg) log(u) else u,N,s,tail,lg)
      f <- pzipf(z,N,s,tail); prev <- pzipf(z-1,N,s,tail)
      expect_true(all(if (tail) f>=u & prev<u else f<=u & prev>u))
    }
  }
})
test_that("Zipf quantiles at CDF atoms use the smallest support point", {
  expect_equal(qzipf((1:10)/10,10,0),1:10)
  for (s in c(0,1)) for (tail in c(TRUE,FALSE)) for (lg in c(TRUE,FALSE))
    expect_equal(qzipf(pzipf(1:9,10,s,tail,lg),10,s,tail,lg),1:9)
})
test_that("Zipf large supports and tiny upper tails remain accurate", {
  N <- 1e9
  expect_equal(pzipf(c(1,1e6,N-1),N,0), c(1,1e6,N-1)/N)
  expect_equal(pzipf(N-1,N,0,FALSE),1/N,tolerance=1e-13)
  # Integral-test bracket, independent of Euler--Maclaurin.
  s <- 2
  lo <- 1 + sum((2:100000)^-s) + 1/100001 - 1/(N+1)
  hi <- 1 + sum((2:100000)^-s) + 1/100000 - 1/N
  h <- 1/dzipf(1,N,s)
  expect_true(h>=lo && h<=hi)
  # s=1 tail has elementary integral bounds at billion-scale endpoints.
  tail <- pzipf(N-10000,N,1,FALSE)
  h <- 1/dzipf(1,N,1)
  expect_true(tail*h>=log((N+1)/(N-9999)))
  expect_true(tail*h<=log(N/(N-10000)))
  expect_equal(pzipf(N-1,N,100,FALSE,TRUE),dzipf(N,N,100,TRUE),tolerance=1e-13)
  expect_equal(qzipf(-1000,N,2,FALSE,TRUE),N)
  expect_equal(qzipf(.5,.Machine$integer.max,0),1073741824)
})
test_that("Zipf R conventions, recycling and RNG work", {
  expect_warning(expect_equal(dzipf(.5,10,1),0),"non-integer")
  expect_warning(expect_true(is.nan(dzipf(1,0,1))),"NaNs")
  expect_warning(expect_true(is.nan(qzipf(2,10,1))),"NaNs")
  expect_identical(dzipf(NA_real_,10,1),NA_real_)
  expect_true(is.nan(pzipf(NaN,10,1)))
  expect_identical(dzipf(1,NA_real_,1),NA_real_)
  expect_equal(pzipf(c(-Inf,Inf),10,1),c(0,1))
  expect_equal(dzipf(c(-Inf,Inf),10,1),c(0,0))
  expect_equal(dzipf(1:4,c(4,5),c(0,1)),mapply(dzipf,1:4,c(4,5,4,5),c(0,1,0,1)))
  expect_length(dzipf(numeric(),10,1),0)
  expect_length(rzipf(0,10,1),0)
  expect_length(rzipf(3,c(5,6,7,8),0),3)
  set.seed(72); a <- rzipf(20000,5,.5)
  set.seed(72); expect_identical(a,rzipf(20000,5,.5))
  expect_lt(sum((tabulate(a,5)-20000*dzipf(1:5,5,.5))^2/(20000*dzipf(1:5,5,.5))),25)
})
test_that("own Zipf functions work through every fitted-model route", {
  x <- rep(1:5,40); f <- mlzipf(x)
  expect_equal(dml(1:5,f),rep(.2,5))
  expect_equal(pml(1:5,f),(1:5)/5)
  expect_equal(qml(c(.1,.3,.9),f),c(1,2,5))
  expect_equal(as.numeric(logLik(f)),sum(dml(x,f,log=TRUE)))
  expect_true(all(rml(100,f) %in% 1:5))
  set.seed(32); expect_equal(dim(bootstrapml(f,reps=3)),c(2L,2L))
  expect_s3_class(model_select(x,models="zipf"),"univariateML")
  grDevices::pdf(tempfile(fileext=".pdf")); on.exit(grDevices::dev.off())
  expect_error(plot(f),NA)
})
test_that("Zipf agrees with sads where its implementation is correct", {
  skip_if_not_installed("sads")
  expect_equal(dzipf(1:10,10,1),sads::dzipf(1:10,10,1))
  expect_equal(pzipf(1:10,10,1),sads::pzipf(1:10,10,1))
})
