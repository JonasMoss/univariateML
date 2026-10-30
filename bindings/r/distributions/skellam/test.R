test_that("Skellam faces, recycling and RNG work", {
  k <- -15:15

  expect_equal(dskellam(x = 0:5, mu1 = 2, mu2 = 0), dpois(0:5, 2))
  expect_equal(dskellam(x = -(0:5), mu1 = 0, mu2 = 2), dpois(0:5, 2))
  expect_equal(dskellam(x = c(-1, 0, 1), mu1 = 0, mu2 = 0), c(0, 1, 0))
  expect_equal(pskellam(q = -5:5, mu1 = 2, mu2 = 0), ppois(-5:5, 2))
  expect_equal(pskellam(q = -5:5, mu1 = 0, mu2 = 2), ppois(-(-5:5)-1, 2, lower.tail = FALSE))
  expect_equal(pskellam(q = k, mu1 = 4, mu2 = 3) + pskellam(q = k, mu1 = 4, mu2 = 3, lower.tail = FALSE), rep(1, length(k)))
  expect_equal(pskellam(q = k, mu1 = 4, mu2 = 3)-pskellam(q = k - 1, mu1 = 4, mu2 = 3), dskellam(x = k, mu1 = 4, mu2 = 3), tolerance = 1e-13)

  expect_equal(pskellam(.9, 4, 3), pskellam(0, 4, 3))
  expect_warning(expect_true(is.nan(dskellam(x = 0, mu1 = -1, mu2 = 1))), "NaNs")
  expect_warning(expect_equal(dskellam(x = 0.5, mu1 = 1, mu2 = 1), 0), "non-integer")
  expect_warning(expect_true(is.nan(qskellam(p = 2, mu1 = 1, mu2 = 1))), "NaNs")
  expect_equal(dskellam(x = c(NA, NaN, 0), mu1 = 1, mu2 = 1)[1:2], c(NA_real_, NaN))
  expect_length(dskellam(x = numeric(), mu1 = 1, mu2 = 1), 0)
  expect_equal(dskellam(x = 0:2, mu1 = c(1, 2), mu2 = 3), c(dskellam(x = 0, mu1 = 1, mu2 = 3),dskellam(x = 1, mu1 = 2, mu2 = 3),dskellam(x = 2, mu1 = 1, mu2 = 3)))
  set.seed(93); a <- rskellam(n = 100, mu1 = 4, mu2 = 3)
  set.seed(93); expect_equal(rskellam(n = 100, mu1 = 4, mu2 = 3), a)
  set.seed(3); z <- rskellam(n = 20000, mu1 = 4, mu2 = 3)
  expect_equal(mean(z), 1, tolerance = 0.07)
  expect_equal(var(z), 7, tolerance = 0.2)
  for (lower in c(TRUE,FALSE)) for (lp in c(-.001,-1,-20,-1000)) {
    q <- qskellam(p = lp, mu1 = 4, mu2 = 3, lower.tail = lower, log.p = TRUE)
    f <- pskellam(q = q, mu1 = 4, mu2 = 3, lower.tail = lower, log.p = TRUE)
    prev <- pskellam(q = q - 1, mu1 = 4, mu2 = 3, lower.tail = lower, log.p = TRUE)
    expect_true(if(lower) f>=lp else f<=lp)
    expect_true(if(lower) prev<lp else prev>lp)
  }
})

test_that("Skellam likelihood agrees with independent reference optimizations", {
  samples <- list(c(-4,-2,-1,0,1,2,5,8), c(-10,-3,0,1,1,4,12), c(0,0,1,2,8,20), c(1,1,1,1), c(-2,-2,-2), c(0,0,0), c(-1,1))
  for(x in samples) {
    fit <- mlskellam(x)
    expect_equal(unname(fit[1]-fit[2]), mean(x), tolerance=1e-8)
    expect_equal(as.numeric(logLik(fit)), sum(dskellam(x = x, mu1 = fit[1], mu2 = fit[2], log = TRUE)))
    oracle <- function(t) {
      if(any(!is.finite(t)) || any(t < -24 | t > log(100))) return(1e100)
      -sum(dskellam(x = x, mu1 = exp(t[1]), mu2 = exp(t[2]), log = TRUE))
    }
    for(start in list(c(.1,.1),c(2,4),c(20,20))) {
      o <- optim(log(start),oracle,method="Nelder-Mead",control=list(maxit=1500,reltol=1e-11))
      expect_gte(as.numeric(logLik(fit))+1e-7*length(x), -o$value)
    }
  }
  x <- samples[[1]]
  for (held in c("mu1","mu2")) {
    fit <- do.call(mlskellam,c(list(x),setNames(list(3),held)))
    oracle <- function(t) -sum(if(held=="mu1") dskellam(x = x, mu1 = 3, mu2 = exp(t), log = TRUE) else dskellam(x = x, mu1 = exp(t), mu2 = 3, log = TRUE))
    o <- optimize(oracle, c(-10,5),tol=1e-10)
    expect_equal(as.numeric(logLik(fit)), -o$objective,tolerance=1e-8)
    expect_equal(attr(fit,"fixed"), held)
    expect_equal(attr(logLik(fit),"df"),1L)
  }
  expect_equal(as.numeric(mlskellam(rep(2,4))), c(2,0))
  expect_equal(as.numeric(mlskellam(rep(-2,4))), c(0,2))
  expect_equal(as.numeric(mlskellam(c(0,0))), c(0,0))
  expect_error(mlskellam(x,mu1=0),"zero likelihood")
  expect_error(mlskellam(x,mu1=-1),"nonnegative")
  expect_error(mlskellam(x,mu2=Inf),"nonnegative")
  expect_error(mlskellam(x,iterlim=1),"converge")
  expect_error(mlskellam(x,reltol=0),"positive")
  fit <- mlskellam(x,mu1=4,mu2=3)
  expect_equal(attr(logLik(fit),"df"),0L)



  set.seed(2); a <- rml(10,fit);set.seed(2);expect_equal(a,rml(10,fit))
})

test_that("Skellam tails agree with independent Poisson-mixture summation", {
  for(rates in list(c(.01,20),c(20,.01),c(3,4),c(1000,1000))) {
    a <- rates[1]; b <- rates[2]
    j <- 0:qpois(1-1e-14,b)
    for(k in c(-30,-1,0,1,30)) {
      ref <- sum(dpois(j,b)*ppois(k+j,a))
      expect_equal(pskellam(q = k, mu1 = a, mu2 = b),ref,tolerance=3e-12)
    }
  }
  expect_equal(dskellam(x = c(-1, 0, 1), mu1 = 0, mu2 = 0),c(0,1,0))
  expect_true(is.finite(dskellam(x = 0, mu1 = 1000, mu2 = 1000, log = TRUE)))
  expect_equal(dskellam(x = 0, mu1 = 1e+12, mu2 = 1e+12, log = TRUE),-15.081022681448863,tolerance=1e-13)
})

test_that("Skellam integrates with plotting, bootstrap and model selection", {
  x <- c(-4,-2,-1,0,1,2,5,8)
  fit <- mlskellam(x,mu1=4)
  set.seed(14)
  b <- bootstrapml(fit,reps=3,reducer=identity)
  expect_equal(as.numeric(b[1,]),rep(4,3))
  expect_s3_class(model_select(x,models="skellam"),"univariateML")
  file <- tempfile(fileext=".pdf")
  grDevices::pdf(file)
  expect_error(plot(mlskellam(x)),NA)
  grDevices::dev.off()
  unlink(file)
})

test_that("Skellam endpoints and extreme logs retain their tail convention", {
  expect_equal(pskellam(q = c(-Inf, Inf), mu1 = 4, mu2 = 3),c(0,1))
  expect_equal(pskellam(q = c(-Inf, Inf), mu1 = 4, mu2 = 3, lower.tail = FALSE, log.p = TRUE),c(0,-Inf))
  expect_equal(qskellam(p = c(0, 1), mu1 = 4, mu2 = 3),c(-Inf,Inf))
  expect_equal(qskellam(p = c(-Inf, 0), mu1 = 4, mu2 = 3, log.p = TRUE),c(-Inf,Inf))
  expect_equal(qskellam(p = c(0, 1), mu1 = 4, mu2 = 3, lower.tail = FALSE),c(Inf,-Inf))
  expect_equal(qskellam(p = c(0, 0.2, 1), mu1 = 0, mu2 = 0),c(0,0,0))
  expect_equal(pskellam(q = c(-1, 0, 1), mu1 = 0, mu2 = 0),c(0,1,1))
  for(rates in list(c(4,0),c(0,4),c(4,3))) for(lower in c(TRUE,FALSE)) {
    for(lp in c(-1e-20,-1000)) {
      q <- qskellam(p = lp, mu1 = rates[1], mu2 = rates[2], lower.tail = lower, log.p = TRUE)
      f <- pskellam(q = q, mu1 = rates[1], mu2 = rates[2], lower.tail = lower, log.p = TRUE)
      prev <- pskellam(q = q - 1, mu1 = rates[1], mu2 = rates[2], lower.tail = lower, log.p = TRUE)
      expect_true(if(lower) f>=lp else f<=lp)
      expect_true(if(lower) prev<lp else prev>lp)
    }
  }
})

test_that("Skellam held zero rates use exact signed Poisson fits", {
  expect_equal(as.numeric(mlskellam(c(1,3),mu2=0)),c(2,0))
  expect_equal(as.numeric(mlskellam(c(-1,-3),mu1=0)),c(0,2))
  expect_equal(as.numeric(mlskellam(c(0,0),mu1=.1)),c(.1,0))
  expect_error(mlskellam(c(-1,1),mu1=0,mu2=0),"zero likelihood")
  expect_equal(as.numeric(logLik(mlskellam(c(0,0),mu1=0,mu2=0))),0)
})

test_that("Skellam nonconstant underdispersed samples attain Poisson faces", {
  x <- c(1,1,2,2)
  expect_equal(as.numeric(mlskellam(x)),c(1.5,0))
  expect_equal(as.numeric(mlskellam(-x)),c(0,1.5))
  expect_equal(as.numeric(logLik(mlskellam(x))),sum(dpois(x,1.5,log=TRUE)))
})

test_that("skellam: P3 unfinished optimizer stops", {
  g <- jsonlite::fromJSON(file.path(Sys.getenv("UMVL_DISTRIBUTIONS_DIR"), "skellam", "golden.json"))
  x <- g$data
  expect_error(mlskellam(x, iterlim=1, reltol=1e-10),
               "[Ff]ail|converge|unresolved|exhausted|score check")
})
