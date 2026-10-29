test_that("mldgamma agrees with independent reference optimizers", {
  set.seed(617)
  samples <- list(c(0,0,0,1,2,6), c(1,1,3,8,12), c(0,2),
                  extraDistr::rdgamma(80, .4, .3),
                  extraDistr::rdgamma(150, 30, 3))
  for (x in samples) {
    f <- mldgamma(x)
    objective <- function(t) {
      a <- exp(t[1]); b <- exp(t[2])
      z <- -sum(extraDistr::ddgamma(x,a,b,log=TRUE))
      if (is.finite(z)) z else 1e100
    }
    starts <- expand.grid(a=c(-1,0,2),b=c(-1,0,1))
    fits <- apply(starts,1,function(t) optim(t,objective,method="Nelder-Mead",control=list(maxit=3000,reltol=1e-12))$value)
    expect_equal(as.numeric(logLik(f)), -min(fits),tolerance=2e-6)
    expect_equal(sum(univariateML_to_function(f,"d")(x,log=TRUE)),as.numeric(logLik(f)),tolerance=1e-9)
    for (hold in c("shape","rate")) {
      held <- setNames(list(unname(f[hold])),hold)
      ff <- do.call(mldgamma,c(list(x),held))
      expect_equal(as.numeric(logLik(ff)),as.numeric(logLik(f)),tolerance=1e-7)
      expect_equal(attr(ff,"fixed"),hold)
      expect_equal(attr(logLik(ff),"df"),1L)
    }
  }
})

test_that("mldgamma detects escapes and validates all held subsets", {
  expect_error(mldgamma(rep(3,10)),"point mass")
  expect_error(mldgamma(c(rep(3,7),rep(4,3))),"mlbinom")
  expect_error(mldgamma(c(0,1)),"mlbinom")
  expect_error(mldgamma(c(0,.5,2)),"Non-integral")
  expect_error(mldgamma(c(0,1,2),rate=0),"held")
  expect_error(mldgamma(c(0,1,2),shape=Inf),"held")
  expect_error(mldgamma(c(0,1,2),reltol=0),"control")
  expect_error(mldgamma(c(0,1,2),iterlim=0),"control")
  expect_error(mldgamma(c(0,1,2),iterlim=1),"converge")
  ff <- mldgamma(c(3,3),shape=2,rate=1)
  expect_equal(attr(logLik(ff),"df"),0L)
  expect_equal(as.numeric(logLik(ff)),sum(univariateML_to_function(ff,"d")(c(3,3),log=TRUE)))
  expect_error(mldgamma(c(0,0),shape=2),"point mass")
  expect_error(mldgamma(c(0,0),rate=2),"point mass")
  expect_error(mldgamma(c(-1,2)))
  expect_true(is.finite(logLik(mldgamma(c(3,3),shape=2))))
  expect_true(is.finite(logLik(mldgamma(c(3,3),rate=2))))
})

test_that("dgamma repaired tails, integer quantiles and seeded sampling", {
  f <- mldgamma(c(0,1,2),shape=2,rate=1)
  xs <- c(0,1,10,40,1000)
  lp <- univariateML_to_function(f,"d")(xs,log=TRUE)
  expect_true(all(is.finite(lp)))
  # Interval probability checked by independent integration on a scaled kernel.
  for (x in c(0,10,40,1000)) {
    anchor <- stats::dgamma(x+.5,2,rate=1,log=TRUE)
    oracle <- anchor+log(integrate(function(z) exp(stats::dgamma(z,2,rate=1,log=TRUE)-anchor),x,x+1,rel.tol=1e-10)$value)
    expect_equal(univariateML_to_function(f,"d")(x,log=TRUE),oracle,tolerance=1e-9)
  }
  expect_equal(univariateML_to_function(f,"p")(.5),univariateML_to_function(f,"p")(0))
  expect_true(is.finite(univariateML_to_function(f,"p")(1000,lower.tail=FALSE,log.p=TRUE)))
  for (lower in c(TRUE,FALSE)) for (logp in c(TRUE,FALSE)) {
    u <- c(1e-12,.1,.5,.9,1-1e-12)
    q <- univariateML_to_function(f,"q")(if (logp) log(u) else u,lower.tail=lower,log.p=logp)
    expect_equal(q,floor(q))
    if (lower) { expect_true(all(univariateML_to_function(f,"p")(q)>=u-1e-15)); expect_true(all(univariateML_to_function(f,"p")(q-1)<u)) }
    else { expect_true(all(univariateML_to_function(f,"p")(q,lower.tail=FALSE)<=u+1e-15)); expect_true(all(univariateML_to_function(f,"p")(q-1,lower.tail=FALSE)>u)) }
  }
  set.seed(123); a <- univariateML_to_function(f,"r")(20)
  set.seed(123); b <- extraDistr::rdgamma(20,2,1)
  expect_equal(a,b)
  expect_equal(dml(.5,f),0)
  expect_equal(pml(0,f),univariateML_to_function(f,"p")(0))
  expect_equal(qml(.5,f),univariateML_to_function(f,"q")(.5))
  expect_equal(univariateML_to_function(f,"d")(0),extraDistr::ddgamma(0,2,1),tolerance=1e-13)
})

test_that("gamma held subsets have independent scalar optima", {
  x <- c(0,0,0,1,3,7,12)
  ref <- univariateML_reference_fixes[["extraDistr::ddgamma"]]$replacement
  for (a in c(.2,2,20)) {
    f <- mldgamma(x,shape=a)
    oracle <- optimize(function(t) -sum(ref(x,a,exp(t),log=TRUE)),c(-8,5),tol=1e-10)
    expect_equal(as.numeric(logLik(f)),-oracle$objective,tolerance=1e-8)
  }
  for (r in c(.1,1,10)) {
    f <- mldgamma(x,rate=r)
    oracle <- optimize(function(t) -sum(ref(x,exp(t),r,log=TRUE)),c(-8,6),tol=1e-10)
    expect_equal(as.numeric(logLik(f)),-oracle$objective,tolerance=1e-8)
  }
  expect_equal(unname(mldgamma(x,shape=1)["rate"]),log1p(1/mean(x)),tolerance=1e-9)
  f <- mldgamma(c(0,1),shape=2,rate=1)
  expect_equal(univariateML_to_function(f,"q")(c(0,1)),c(0,Inf))
  expect_equal(univariateML_to_function(f,"d")(c(-1,Inf)),c(0,0))
  expect_true(is.na(univariateML_to_function(f,"d")(NA_real_)))
  expect_equal(univariateML_to_function(f,"d")(c(0,1),shape=c(2,3)),c(univariateML_to_function(f,"d")(0),univariateML_to_function(f,"d")(1,shape=3)))
  expect_true(is.finite(univariateML_to_function(f,"d")(1,rate=1e-300,log=TRUE)))
  expect_warning(expect_true(is.nan(univariateML_to_function(f,"d")(0,shape=0))),"NaNs")
})

test_that("dgamma: P3 unfinished optimizer stops", {
  g <- jsonlite::fromJSON(file.path(Sys.getenv("UMVL_DISTRIBUTIONS_DIR"), "dgamma", "golden.json"))
  x <- g$data
  expect_error(mldgamma(x, iterlim=1, reltol=1e-10),
               "[Ff]ail|converge|unresolved|exhausted|score check")
})
