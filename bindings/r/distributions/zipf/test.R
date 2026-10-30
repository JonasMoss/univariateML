context("mlzipf")

## Warning when all observations are 1 (MLE not unique)
expect_warning(
  mlzipf(c(1, 1, 1)),
  "All observations are equal to 1\\. The maximum likelihood estimator is not unique\\."
)

## qml inverts pml: F(q - 1) < p <= F(q). sads::qzipf 0.6.5, the former
## reference, is off by one (KNOWN_ISSUES.md).
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
    expect_equal(dzipf(x = 1:N, N = N, s = s), w, tolerance = 2e-13)
    expect_equal(dzipf(x = 1:N, N = N, s = s, log = TRUE), log(w), tolerance = 2e-13)
    q <- seq(-.5, N+1, by=.5)
    lower <- vapply(q, function(k) sum(w[seq_len(N) <= k]), 0)
    upper <- vapply(q, function(k) sum(w[seq_len(N) > k]), 0)
    expect_equal(pzipf(q = q, N = N, s = s), lower, tolerance=2e-13)
    expect_equal(pzipf(q = q, N = N, s = s, lower.tail = FALSE), upper, tolerance=2e-13)
    expect_equal(pzipf(q = q, N = N, s = s, log.p = TRUE), log(lower), tolerance=2e-13)
    expect_equal(pzipf(q = q, N = N, s = s, lower.tail = FALSE, log.p = TRUE), log(upper), tolerance=2e-13)
    u <- c(1e-12,.01,.3,.5,.9,1-1e-12)
    for (tail in c(TRUE,FALSE)) for (lg in c(TRUE,FALSE)) {
      z <- qzipf(p = if (lg) log(u) else u, N = N, s = s, lower.tail = tail, log.p = lg)
      f <- pzipf(q = z, N = N, s = s, lower.tail = tail); prev <- pzipf(q = z - 1, N = N, s = s, lower.tail = tail)
      expect_true(all(if (tail) f>=u & prev<u else f<=u & prev>u))
    }
  }
})
test_that("Zipf quantiles at CDF atoms use the smallest support point", {
  expect_equal(qzipf(p = (1:10)/10, N = 10, s = 0),1:10)
  for (s in c(0,1)) for (tail in c(TRUE,FALSE)) for (lg in c(TRUE,FALSE))
    expect_equal(qzipf(pzipf(q = 1:9, N = 10, s = s, lower.tail = tail, log.p = lg),10,s,tail,lg),1:9)
})
test_that("Zipf large supports and tiny upper tails remain accurate", {
  N <- 1e9
  expect_equal(pzipf(q = c(1, 1e+06, N - 1), N = N, s = 0), c(1,1e6,N-1)/N)
  expect_equal(pzipf(q = N - 1, N = N, s = 0, lower.tail = FALSE),1/N,tolerance=1e-13)
  # Integral-test bracket, independent of Euler--Maclaurin.
  s <- 2
  lo <- 1 + sum((2:100000)^-s) + 1/100001 - 1/(N+1)
  hi <- 1 + sum((2:100000)^-s) + 1/100000 - 1/N
  h <- 1/dzipf(x = 1, N = N, s = s)
  expect_true(h>=lo && h<=hi)
  # s=1 tail has elementary integral bounds at billion-scale endpoints.
  tail <- pzipf(q = N - 10000, N = N, s = 1, lower.tail = FALSE)
  h <- 1/dzipf(x = 1, N = N, s = 1)
  expect_true(tail*h>=log((N+1)/(N-9999)))
  expect_true(tail*h<=log(N/(N-10000)))
  expect_equal(pzipf(q = N - 1, N = N, s = 100, lower.tail = FALSE, log.p = TRUE),dzipf(x = N, N = N, s = 100, log = TRUE),tolerance=1e-13)
  expect_equal(qzipf(p = -1000, N = N, s = 2, lower.tail = FALSE, log.p = TRUE),N)
  expect_equal(qzipf(p = 0.5, N = .Machine$integer.max, s = 0),1073741824)
})
test_that("Zipf R conventions, recycling and RNG work", {
  expect_warning(expect_equal(dzipf(x = 0.5, N = 10, s = 1),0),"non-integer")
  expect_warning(expect_true(is.nan(dzipf(x = 1, N = 0, s = 1))),"NaNs")
  expect_warning(expect_true(is.nan(qzipf(p = 2, N = 10, s = 1))),"NaNs")
  expect_identical(dzipf(x = NA_real_, N = 10, s = 1),NA_real_)
  expect_true(is.nan(pzipf(q = NaN, N = 10, s = 1)))
  expect_identical(dzipf(x = 1, N = NA_real_, s = 1),NA_real_)
  expect_equal(pzipf(q = c(-Inf, Inf), N = 10, s = 1),c(0,1))
  expect_equal(dzipf(x = c(-Inf, Inf), N = 10, s = 1),c(0,0))
  expect_equal(dzipf(x = 1:4, N = c(4, 5), s = c(0, 1)),mapply(dzipf,1:4,c(4,5,4,5),c(0,1,0,1)))
  expect_length(dzipf(x = numeric(), N = 10, s = 1),0)
  expect_length(rzipf(n = 0, N = 10, s = 1),0)
  expect_length(rzipf(n = 3, N = c(5, 6, 7, 8), s = 0),3)
  set.seed(72); a <- rzipf(n = 20000, N = 5, s = 0.5)
  set.seed(72); expect_identical(a,rzipf(n = 20000, N = 5, s = 0.5))
  expect_lt(sum((tabulate(a,5)-20000*dzipf(x = 1:5, N = 5, s = 0.5))^2/(20000*dzipf(x = 1:5, N = 5, s = 0.5))),25)
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
test_that("zipf matches frozen sads values (reference.json)", {
  expect_reference_values("zipf")
})

test_that("Zipf all hold subsets match enumerated multistart reference fits", {
  set.seed(811)
  samples <- list(sample(1:8,20,TRUE),sample(1:8,100,TRUE),c(1,2),c(1,1,2,4),rep(4,5),pmin(rzeta(n = 20, s = 2),10))
  for(x in samples) for(mask in 0:3) {
    hn <- bitwAnd(mask,1)>0; hs <- bitwAnd(mask,2)>0
    args <- list(x=x)
    if(hn) args$N <- 10
    if(hs) args$s <- .7
    fit <- do.call(mlzipf,args)
    ll <- function(N,s) -s*sum(log(x))-length(x)*log(sum(seq_len(N)^-s))
    Ns <- if(hn) 10 else max(x):(max(x)+4)
    oracle <- max(vapply(Ns,function(N) {
      if(hs) return(ll(N,.7))
      max(ll(N,0),vapply(c(-3,0,2),function(start) {
        o <- optim(start,function(t) -ll(N,exp(t)),method="BFGS",control=list(reltol=1e-12))
        -o$value
      },0.))
    },0.))
    expect_equal(as.numeric(logLik(fit)),oracle,tolerance=1e-8)
    expect_setequal(attr(fit,"fixed"),names(args)[-1])
    if(hn) expect_identical(fit[["N"]],10)
    if(hs) expect_identical(fit[["s"]],.7)
  }
  expect_error(mlzipf(rep(1,3),N=5),"point mass")
  expect_equal(mlzipf(rep(1,3),s=.7)[["N"]],1)
  expect_warning(expect_equal(mlzipf(rep(1,3),N=1)[["s"]],1),"not unique")
  expect_equal(mlzipf(rep(1,3),N=1,s=0)[["s"]],0)
  for(args in list(list(N=2),list(N=4.5),list(N=Inf),list(N=NA_real_),list(s=-1),list(s=Inf),list(s=NA_real_)))
    expect_error(do.call(mlzipf,c(list(x=1:4),args)))
})
