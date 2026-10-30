test_that("zib agrees with a multi-start reference-density oracle", {
  set.seed(12670)
  x <- do.call(rzib, list(250, size = 8, prob = 0.4, pi = 0.4))
  fit <- mlzib(x, size = NULL)
  density <- dzib
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
  fixed <- mlzib(x, size=15)
  expect_identical(attr(fixed,"fixed"), "size")
  expect_equal(unname(fixed["size"]), 15)
  expect_equal(attr(logLik(fixed),"df"), 2L)
  expect_gte(attr(fixed,"logLik"), -oracle(c(size=15))-1e-6)
  for(bad in c(-1,Inf,NaN,0,.5)) expect_error(mlzib(x,size=bad),"size")
  expect_error(mlzib_(integer(), size=NULL),"nonempty")
  expect_error(mlzib_(c(-1L,1L), size=NULL),"count")
})

test_that("zib declares its infinite-size escape", {
 x <- c(0,0,0,0,0,0,0,0,1,1,1,2,3,7,12)
 expect_error(mlzib(x, size = NULL), "Use `mlzip`")
 expect_s3_class(mlzib(x,size=20), "univariateML")
})

test_that("zib preserves fixed size at attained boundaries", {
 zero <- mlzib(c(0,0),size=5)
 expect_equal(unname(zero["size"]),5)
 expect_identical(attr(zero,"fixed"),"size")
 expect_equal(attr(logLik(zero),"df"),2L)
 one <- mlzib(c(1,1),size=5)
 expect_equal(unname(one["size"]),5)
})

test_that("zib accepts fixed zero size for all-zero data", {
 fit <- mlzib(c(0,0),size=0)
 expect_equal(unname(fit["size"]),0)
 expect_identical(attr(fit,"fixed"),"size")
 expect_equal(attr(logLik(fit),"df"),2L)
 expect_equal(attr(fit,"logLik"),0)
})

 test_that("zib: new holds match reference-density integer and multistart oracles", {
  set.seed(810)
  samples <- c(lapply(c(20,100),function(n) rzib(n = n, size = 8, prob = 0.4, pi = 0.3)),list(c(1,2,2),rep(2,4),c(1,1,2,3)))
  for(x in samples) {
    for(fixed_size in c(TRUE,FALSE)) {
      held <- list(prob=.4)
      for (mixture in c(FALSE,TRUE)) {
      held <- list(prob=.4); if(mixture) held$pi <- .3
      args <- c(list(x=x,size=if(fixed_size) 8 else NULL),held)
      fit <- suppressWarnings(do.call(mlzib,args))
      sizes <- if(fixed_size) 8 else seq(max(x),200)
      ll <- function(size, mixture=.3) sum(dzib(x = x, size = size, prob = 0.4, pi = mixture, log = TRUE))
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
  for(p in c(-.1,1.1,NA_real_,Inf)) expect_error(mlzib(c(1,2),size=8,prob=p),"prob")
  expect_error(mlzib(c(1,2),size=8,prob=1))
  expect_error(mlzib(c(1,2),size=NULL,prob=0))
})

test_that("zib: held pi profiles the remaining probability", {
 set.seed(22)
 for(n in c(20,100)) for(x in list(rbinom(n,8,.4),c(rep(0,n/2),rbinom(n/2,8,.4)),c(0,1,2,2,3),rep(0,n))) {
   for(mix in c(0,.2,.8)) {
     f<-suppressWarnings(mlzib(x,size=8,pi=mix))
     ll<-function(p)sum(dzib(x = x, size = 8, prob = p, pi = mix, log = TRUE))
     oracle<-max(c(ll(0),ll(1),vapply(c(.02,.2,.5,.9,.99),function(start) -optim(start,function(p)-ll(p),method="L-BFGS-B",lower=1e-9,upper=1-1e-9)$value,0.)))
     expect_equal(attr(f,"logLik"),oracle,tolerance=1e-6)
     expect_identical(unname(f["pi"]),mix)
   }
 }
 for(v in c(-1,Inf,NA_real_)) expect_error(mlzib(1:3,size=8,pi=v),"pi")
 expect_error(mlzib(1:3,size=8,pi=1))
})

test_that("zib: free-size held-mixture fits and Poisson escapes", {
 for(x in list(c(0,1,2,2,3),c(1,2,3,3,4))) {
  f<-mlzib(x,size=NULL,pi=.2)
  ll<-function(k,p)sum(dzib(x = x, size = k, prob = p, pi = 0.2, log = TRUE))
  oracle<-max(vapply(seq(max(x),50),function(k)max(c(ll(k,0),ll(k,1),vapply(c(.1,.5,.9),function(start)-optim(start,function(p)-ll(k,p),method="L-BFGS-B",lower=1e-8,upper=1-1e-8)$value,0.))),0.))
  expect_equal(attr(f,"logLik"),oracle,tolerance=1e-6)
 }
 expect_error(mlzib(c(1,1,1,2,3,7,12),size=NULL,pi=0),"mlzip")
 f<-mlzib(rep(0,3),size=8,prob=.4,pi=.6)
 expect_identical(unname(f[c("prob","pi")]),c(.4,.6))
 expect_error(mlzib(1:3,size=8,prob=.4,iterlim=0))
})

test_that("zib: P3 unfinished optimizer stops", {
  g <- jsonlite::fromJSON(file.path(Sys.getenv("UMVL_DISTRIBUTIONS_DIR"), "zib", "golden.json"))
  x <- g$data
  expect_error(mlzib(x, iterlim=1, reltol=1e-10, size=NULL),
               "[Ff]ail|converge|unresolved|exhausted|score check")
})

test_that("zib: free size, probability and mixture agree with a reference-density oracle (n=20, n=100, ties, near-Poisson)", {
  # TASK-85/87: same exponential-grid caveat as ztbinom's analogous test;
  # cross-checks the specific cases the task calls out.
  set.seed(85087)
  oracle_ll <- function(x, size) {
    objective <- function(par) {
      p <- plogis(par[1]); mix <- plogis(par[2])
      val <- -sum(dzib(x = x, size = size, prob = p, pi = mix, log = TRUE))
      if (is.finite(val)) val else 1e100
    }
    best <- Inf
    for (start in list(c(-2,-2), c(0,0), c(2,-2), c(-2,2), c(2,2))) {
      o <- optim(start, objective, method = "BFGS", control = list(reltol = 1e-12, maxit = 2000))
      if (o$value < best) best <- o$value
    }
    # Boundary mixture (pi=0, i.e. plain ztbinom*zero-mass) and pi=1
    # (point mass at zero) are not reached by the logit reparametrization.
    ll0 <- sum(dzib(x = x, size = size, prob = max(mean(x)/size, 1e-09), pi = 0, log = TRUE))
    max(-best, ll0)
  }
  oracle <- function(x, sizes) max(vapply(sizes, function(s) oracle_ll(x, s), 0))

  samples <- list(
    n20 = rzib(n = 20, size = 8, prob = 0.4, pi = 0.3),
    n100 = rzib(n = 100, size = 8, prob = 0.4, pi = 0.3),
    ties = c(0, 0, 2, 2, 2, 2, 3, 4, 5, 5),
    near_poisson = rzib(n = 100, size = 500, prob = 0.02, pi = 0.2)
  )
  for (x in samples) {
    # The near-Poisson sample can legitimately hit the infinite-size
    # escape instead of a finite fit (R6: a data-dependent "use mlzip",
    # not a failure); accept either outcome.
    fit <- tryCatch(suppressWarnings(mlzib(x, size = NULL)), error = identity)
    if (inherits(fit, "error")) {
      expect_match(conditionMessage(fit), "mlzip")
      next
    }
    expect_true(is.finite(attr(fit, "logLik")))
    best <- oracle(x, unique(pmax(max(x), round(unname(fit["size"]) * c(.25,.5,.9,1,1.1,2,4)))))
    expect_gte(attr(fit, "logLik") + 1e-5, best)
    expect_equal(attr(fit, "logLik"),
                 sum(dzib(x = x, size = unname(fit["size"]), prob = unname(fit["prob"]), pi = unname(fit["pi"]), log = TRUE)),
                 tolerance = 1e-8)
  }
})

test_that("zib attained point member evaluates and samples exactly", {
  fit <- mlzib(rep(5,4),size=5,prob=1,pi=0)
  expect_equal(dml(c(4,5),fit),c(0,1))
  expect_equal(pml(c(4,5),fit),c(0,1))
  expect_equal(qml(.5,fit),5)
  expect_equal(rml(10,fit),rep(5,10))
})
