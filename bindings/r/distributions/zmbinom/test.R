test_that("zmbinom agrees with a multi-start reference-density oracle", {
  skip_unless_tier("full")
  set.seed(13109)
  x <- do.call(rzmbinom, list(250, size = 8, prob = 0.4, p0 = 0.3))
  fit <- mlzmbinom(x, size = NULL)
  density <- dzmbinom
  ll <- function(p) sum(do.call(density, c(list(x = x), as.list(p), list(log = TRUE))))
  expect_equal(attr(fit, "logLik"), ll(setNames(as.numeric(fit), names(fit))), tolerance=1e-8)
  oracle <- function(held = NULL) {
    free <- setdiff(names(fit), names(held))
    objective <- function(t) {
      p <- setNames(as.numeric(fit), names(fit)); p[names(held)] <- held
      p[free] <- vapply(seq_along(free), function(j) if(free[j] %in% c("size", "lambda")) exp(t[j]) else plogis(t[j]), numeric(1))
      val <- -ll(p); if(is.finite(val)) val else 1e100
    }
    t0 <- vapply(free, function(v) if(v %in% c("size","lambda")) log(fit[v]) else qlogis(fit[v]), numeric(1))
    min(vapply(c(-.7,0,.7), function(delta) optim(t0+delta,objective,method="BFGS",control=list(maxit=2000,reltol=1e-12))$value,numeric(1)))
  }
  best <- min(vapply(seq.int(max(x), max(80, fit["size"]+20)), function(s) oracle(c(size=s)), numeric(1)))
  expect_gte(attr(fit,"logLik"), -best-1e-6)
  fixed <- mlzmbinom(x, size=15)
  expect_identical(attr(fixed,"fixed"), "size")
  expect_equal(unname(fixed["size"]), 15)
  expect_equal(attr(logLik(fixed),"df"), 2L)
  expect_gte(attr(fixed,"logLik"), -oracle(c(size=15))-1e-6)
  for(bad in c(-1,Inf,NaN,0,.5)) expect_error(mlzmbinom(x,size=bad),"size")
  expect_error(mlzmbinom_(integer(), size=NULL),"nonempty")
  expect_error(mlzmbinom_(c(-1L,1L), size=NULL),"count")
  expect_warning(zero <- mlzmbinom(c(0,0,0), size = NULL),"unidentified")
  expect_equal(unname(zero["p0"]),1)
  expect_warning(two <- mlzmbinom(c(0,0,1,1), size = NULL),"mlbinom")
  expect_equal(attr(two,"logLik"),attr(mlbinom(c(0,0,1,1),size=1),"logLik"))
  expect_equal(unname(mlzmbinom(x[x>0], size = NULL)["p0"]),0)
})

test_that("zmbinom declares its infinite-size escape", {
 x <- c(0,0,1,1,1,2,3,7,12)
 expect_error(mlzmbinom(x, size = NULL), "Use `mlzmpois`")
 expect_s3_class(mlzmbinom(x,size=20), "univariateML")
})

test_that("zmbinom preserves fixed size at attained boundaries", {
 expect_warning(zero <- mlzmbinom(c(0,0),size=5),"unidentified")
 expect_equal(unname(zero["size"]),5)
 expect_identical(attr(zero,"fixed"),"size")
 expect_equal(attr(logLik(zero),"df"),2L)
 expect_warning(one <- mlzmbinom(c(0,1,1),size=5),"mlbinom")
 expect_equal(unname(one["size"]),5)
})

 test_that("zmbinom: new holds match reference-density integer and multistart oracles", {
  skip_unless_tier("full")
  set.seed(810)
  samples <- c(lapply(c(20,100),function(n) rzmbinom(n = n, size = 8, prob = 0.4, p0 = 0.3)),list(c(1,2,2),rep(2,4),c(1,1,2,3)))
  for(x in samples) {
    for(fixed_size in c(TRUE,FALSE)) {
      held <- list(prob=.4)
      for (mixture in c(FALSE,TRUE)) {
      held <- list(prob=.4); if(mixture) held$p0 <- .3
      args <- c(list(x=x,size=if(fixed_size) 8 else NULL),held)
      fit <- suppressWarnings(do.call(mlzmbinom,args))
      sizes <- if(fixed_size) 8 else seq(max(x),200)
      ll <- function(size, mixture=.3) sum(dzmbinom(x = x, size = size, prob = 0.4, p0 = mixture, log = TRUE))
      oracle <- max(vapply(sizes,function(size) {
        if(mixture) return(ll(size))
        max(c(ll(size,0),ll(size,1),vapply(c(.05,.4,.9),function(start) -optim(start,function(p) -ll(size,p),method="L-BFGS-B",lower=1e-9,upper=1-1e-9)$value,0.)))
      },0.))
      expect_equal(attr(fit,"logLik"),oracle,tolerance=1e-6)
      for(name in names(held)) expect_identical(unname(fit[name]),held[[name]])
      expect_setequal(attr(fit,"fixed"),c(if(fixed_size) "size",names(held)))
      }
    }
  }
  for(p in c(-.1,1.1,NA_real_,Inf)) expect_error(mlzmbinom(c(1,2),size=8,prob=p),"prob")
  expect_error(mlzmbinom(c(1,2),size=8,prob=1))
  expect_error(mlzmbinom(c(1,2),size=NULL,prob=0))
})

test_that("zmbinom: held p0 profiles the remaining probability", {
 set.seed(22)
 for(n in c(20,100)) for(x in list(rbinom(n,8,.4),c(rep(0,n/2),rbinom(n/2,8,.4)),c(0,1,2,2,3),rep(0,n))) {
   for(mix in c(0,.2,.8)) {
     if(mix==0 && any(x==0)) {expect_error(mlzmbinom(x,size=8,p0=mix));next}
     f<-suppressWarnings(mlzmbinom(x,size=8,p0=mix))
     ll<-function(p)sum(dzmbinom(x = x, size = 8, prob = p, p0 = mix, log = TRUE))
     oracle<-max(c(ll(0),ll(1),vapply(c(.02,.2,.5,.9,.99),function(start) -optim(start,function(p)-ll(p),method="L-BFGS-B",lower=1e-9,upper=1-1e-9)$value,0.)))
     expect_equal(attr(f,"logLik"),oracle,tolerance=1e-6)
     expect_identical(unname(f["p0"]),mix)
   }
 }
 for(v in c(-1,Inf,NA_real_)) expect_error(mlzmbinom(1:3,size=8,p0=v),"p0")
 expect_error(mlzmbinom(1:3,size=8,p0=1))
})

test_that("zmbinom: free-size held-mixture fits and Poisson escapes", {
 for(x in list(c(0,1,2,2,3),c(1,2,3,3,4))) {
  f<-mlzmbinom(x,size=NULL,p0=.2)
  ll<-function(k,p)sum(dzmbinom(x = x, size = k, prob = p, p0 = 0.2, log = TRUE))
  oracle<-max(vapply(seq(max(x),50),function(k)max(c(ll(k,0),ll(k,1),vapply(c(.1,.5,.9),function(start)-optim(start,function(p)-ll(k,p),method="L-BFGS-B",lower=1e-8,upper=1-1e-8)$value,0.))),0.))
  expect_equal(attr(f,"logLik"),oracle,tolerance=1e-6)
 }
 expect_error(mlzmbinom(c(1,1,1,2,3,7,12),size=NULL,p0=.2),"mlzmpois")
 f<-mlzmbinom(rep(0,3),size=8,prob=.4,p0=.6)
 expect_identical(unname(f[c("prob","p0")]),c(.4,.6))
 expect_error(mlzmbinom(1:3,size=8,prob=.4,iterlim=0))
})

test_that("zmbinom: P3 unfinished optimizer stops", {
  x <- c(1,2,3,3,5)
  expect_error(mlzmbinom(x, iterlim=1, reltol=1e-10, size=NULL),
               "[Ff]ail|converge|unresolved|exhausted|score check")
})

test_that("zmbinom: free size and probability agree with a reference-density oracle (n=20, n=100, ties, near-Poisson)", {
  skip_unless_tier("full")
  # TASK-85/87: zmbinom delegates its free-size search entirely to
  # ztbinom_mle on the positive-value subset (mle.hpp); this exercises
  # that delegation on the same n=20/100, ties and near-Poisson cases as
  # ztbinom's own analogous test.
  set.seed(85087)
  oracle_ll <- function(x, size) {
    k <- sum(x > 0); n <- length(x); p0 <- (n - k) / n
    pos <- x[x > 0]
    objective <- function(p) -sum(dztbinom(x = pos, size = size, prob = p, log = TRUE))
    base <- if (n - k > 0) (n - k) * log(p0) else 0
    base <- base + if (k > 0) k * log1p(-p0) else 0
    base - optimize(objective, c(1e-9, 1 - 1e-9))$objective
  }
  oracle <- function(x, sizes) max(vapply(sizes, function(s) oracle_ll(x, s), 0))

  samples <- list(
    n20 = rzmbinom(n = 20, size = 8, prob = 0.4, p0 = 0.3),
    n100 = rzmbinom(n = 100, size = 8, prob = 0.4, p0 = 0.3),
    ties = c(0, 0, 2, 2, 2, 2, 3, 4, 5, 5),
    near_poisson = rzmbinom(n = 100, size = 500, prob = 0.02, p0 = 0.2)
  )
  for (x in samples) {
    if (!any(x == 0)) next  # p0 is held at 0 (identifiability) and unaffected by this fix
    # The near-Poisson sample can legitimately hit the infinite-size
    # escape instead of a finite fit (R6: a data-dependent "use
    # mlzmpois", not a failure); accept either outcome.
    fit <- tryCatch(suppressWarnings(mlzmbinom(x, size = NULL)), error = identity)
    if (inherits(fit, "error")) {
      expect_match(conditionMessage(fit), "mlzmpois")
      next
    }
    expect_true(is.finite(attr(fit, "logLik")))
    best <- oracle(x, unique(pmax(max(x), round(unname(fit["size"]) * c(.25,.5,.9,1,1.1,2,4)))))
    expect_gte(attr(fit, "logLik") + 1e-6, best)
    expect_equal(attr(fit, "logLik"),
                 sum(dzmbinom(x = x, size = unname(fit["size"]), prob = unname(fit["prob"]), p0 = unname(fit["p0"]), log = TRUE)),
                 tolerance = 1e-8)
  }
})

test_that("zmbinom attained point member evaluates and samples exactly", {
  fit <- mlzmbinom(rep(5,4),size=5,prob=1,p0=0)
  expect_equal(dml(c(4,5),fit),c(0,1))
  expect_equal(pml(c(4,5),fit),c(0,1))
  expect_equal(qml(.5,fit),5)
  expect_equal(rml(10,fit),rep(5,10))
})

test_that("zmbinom J7a stop transports the held zero probability", {
  e <- tryCatch(mlzmbinom(c(0,0,1,1,1,2,3,7,12),size=NULL,p0=.2),error=identity)
  expect_s3_class(e,"univariateML_unresolved")
  expect_equal(e$loglik_upper,Inf)
  expect_equal(e$candidates$p0,rep(.2,nrow(e$candidates)))
})


test_that("zmbinom unresolved size-face comparisons stop as unresolved", {
  e <- tryCatch(mlzmbinom(c(0,0,1,1,1,2,3,7,12),size=NULL,p0=.2),error=identity)
  expect_identical(e$kind,"unresolved")
  expect_identical(class(e)[1],"univariateML_unresolved")
})
