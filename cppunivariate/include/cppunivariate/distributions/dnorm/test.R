test_that("mldnorm agrees with independent reference optimizers", {
  set.seed(617)
  samples <- list(c(0,0,0,1,2,6), c(1,1,3,8,12), c(0,2),
                  rdnorm(n = 80, mean = -3, sd = 2),
                  rdnorm(n = 150, mean = 10, sd = 0.8))
  for (x in samples) {
    f <- mldnorm(x)
    objective <- function(t) {
      a <- t[1]; b <- exp(t[2])
      z <- -sum(ddnorm(x = x, mean = a, sd = b, log = TRUE))
      if (is.finite(z)) z else 1e100
    }
    starts <- expand.grid(a=c(-1,0,2),b=c(-1,0,1))
    fits <- apply(starts,1,function(t) optim(t,objective,method="Nelder-Mead",control=list(maxit=3000,reltol=1e-12))$value)
    expect_equal(as.numeric(logLik(f)), -min(fits),tolerance=2e-6)
    expect_equal(sum(univariateML_to_function(f,"d")(x,log=TRUE)),as.numeric(logLik(f)),tolerance=1e-9)
    for (hold in c("mean","sd")) {
      held <- setNames(list(unname(f[hold])),hold)
      ff <- do.call(mldnorm,c(list(x),held))
      expect_equal(as.numeric(logLik(ff)),as.numeric(logLik(f)),tolerance=1e-7)
      expect_equal(attr(ff,"fixed"),hold)
      expect_equal(attr(logLik(ff),"df"),1L)
    }
  }
})

test_that("mldnorm detects escapes and validates all held subsets", {
  expect_error(mldnorm(rep(3,10)),"point mass")
  expect_error(mldnorm(c(rep(3,7),rep(4,3))),"mlbinom")
  expect_error(mldnorm(c(0,1)),"mlbinom")
  expect_error(mldnorm(c(0,.5,2)),"Non-integral")
  expect_error(mldnorm(c(0,1,2),sd=0),"held")
  expect_error(mldnorm(c(0,1,2),mean=Inf),"held")
  expect_error(mldnorm(c(0,1,2),reltol=0),"control")
  expect_error(mldnorm(c(0,1,2),iterlim=0),"control")
  expect_error(mldnorm(c(0,1,2),iterlim=1),"converge")
  ff <- mldnorm(c(3,3),mean=2,sd=1)
  expect_equal(attr(logLik(ff),"df"),0L)
  expect_equal(as.numeric(logLik(ff)),sum(univariateML_to_function(ff,"d")(c(3,3),log=TRUE)))
  expect_error(mldnorm(c(3,4),mean=4),"zero-scale")
  expect_error(mldnorm(c(3,3),mean=3.5),"zero-scale")
  expect_true(is.finite(logLik(mldnorm(c(3,3),mean=2))))
  expect_equal(unname(mldnorm(c(3,3),sd=1)["mean"]),3.5,tolerance=1e-8)
  expect_equal(unname(mldnorm(c(3,3),mean=3.5,sd=1)["sd"]),1)
})

test_that("dnorm log tails, integer quantiles and seeded sampling", {
  f <- mldnorm(c(0,1,2),mean=0,sd=1)
  xs <- c(-1000,-40,-10,0,1,10,40,1000)
  lp <- univariateML_to_function(f,"d")(xs,log=TRUE)
  expect_true(all(is.finite(lp)))
  # Interval probability checked by independent integration on a scaled kernel.
  for (x in c(0,10,40,1000)) {
    anchor <- stats::dnorm(x+.5,0,1,log=TRUE)
    oracle <- anchor+log(integrate(function(z) exp(stats::dnorm(z,0,1,log=TRUE)-anchor),x,x+1,rel.tol=1e-10)$value)
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
  set.seed(123); b <- rdnorm(n = 20, mean = 0, sd = 1)
  expect_equal(a,b)
  expect_equal(dml(.5,f),0)
  expect_equal(pml(0,f),univariateML_to_function(f,"p")(0))
  expect_equal(qml(.5,f),univariateML_to_function(f,"q")(.5))
  expect_equal(univariateML_to_function(f,"d")(0),ddnorm(x = 0, mean = 0, sd = 1),tolerance=1e-13)
})

test_that("normal held subsets have independent scalar optima", {
  x <- c(-3,-2,0,0,4,5)
  for (mu in c(-8,0,10)) {
    f <- mldnorm(x,mean=mu)
    oracle <- optimize(function(t) -sum(ddnorm(x = x, mean = mu, sd = exp(t), log = TRUE)),c(-1,5),tol=1e-10)
    expect_equal(as.numeric(logLik(f)),-oracle$objective,tolerance=1e-8)
  }
  for (s in c(.5,2,10)) {
    f <- mldnorm(x,sd=s)
    # Use the stable runtime reference correction in far-tail oracle evaluations.
    ref <- ddnorm
    oracle <- optimize(function(t) -sum(ref(x = x, mean = t, sd = s, log = TRUE)),c(-10,10),tol=1e-10)
    expect_equal(as.numeric(logLik(f)),-oracle$objective,tolerance=1e-8)
  }
  f <- mldnorm(c(0,1),mean=0,sd=1)
  expect_equal(univariateML_to_function(f,"q")(c(-1000,-10),log.p=TRUE),c(-45,-4))
  expect_equal(univariateML_to_function(f,"q")(c(0,1)),c(-Inf,Inf))
  ref <- univariateML_to_function(f,"d")
  expect_equal(ref(x = c(-Inf, Inf), mean = 0, sd = 1),c(0,0))
  expect_true(is.na(ref(x = NA_real_, mean = 0, sd = 1)))
  expect_equal(ref(x = c(0, 1), mean = c(0, 2), sd = 1),c(ref(x = 0, mean = 0, sd = 1),ref(x = 1, mean = 2, sd = 1)))
  expect_true(is.finite(ref(x = 0, mean = 0, sd = 1e+300, log = TRUE)))
  expect_warning(expect_true(is.nan(ref(x = 0, mean = 0, sd = 0))),"NaNs")
})
