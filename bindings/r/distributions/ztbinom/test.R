test_that("ztbinom agrees with a multi-start reference-density oracle", {
  set.seed(13116)
  x <- do.call(rztbinom, list(250, size = 8, prob = 0.4))
  fit <- mlztbinom(x, size = NULL)
  density <- dztbinom
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
  fixed <- mlztbinom(x, size=15)
  expect_identical(attr(fixed,"fixed"), "size")
  expect_equal(unname(fixed["size"]), 15)
  expect_equal(attr(logLik(fixed),"df"), 1L)
  expect_gte(attr(fixed,"logLik"), -oracle(c(size=15))-1e-6)
  for(bad in c(-1,Inf,NaN,0,.5)) expect_error(mlztbinom(x,size=bad),"size")
  expect_error(mlztbinom_(integer(), size=NULL),"nonempty")
  expect_error(mlztbinom_(c(-1L,1L), size=NULL),"count")
  expect_error(mlztbinom(c(0,1,2), size = NULL))
  expect_equal(attr(mlztbinom(rep(1,4), size = NULL),"logLik"),0)
})

test_that("ztbinom declares its infinite-size escape", {
 x <- c(1,1,1,2,3,7,12)
 expect_error(mlztbinom(x, size = NULL), "Use `mlztpois`")
 expect_s3_class(mlztbinom(x,size=20), "univariateML")
})

test_that("ztbinom preserves fixed size at attained boundaries", {
 zero <- mlztbinom(c(1,1),size=5)
 expect_equal(unname(zero["size"]),5)
 expect_identical(attr(zero,"fixed"),"size")
 expect_equal(attr(logLik(zero),"df"),1L)
 one <- mlztbinom(c(1,1),size=5)
 expect_equal(unname(one["size"]),5)
})

 test_that("ztbinom: new holds match reference-density integer and multistart oracles", {
  set.seed(810)
  samples <- c(lapply(c(20,100),function(n) rztbinom(n = n, size = 8, prob = 0.4)),list(c(1,2,2),rep(2,4),c(1,1,2,3)))
  for(x in samples) {
    for(fixed_size in c(TRUE,FALSE)) {
      held <- list(prob=.4)


      args <- c(list(x=x,size=if(fixed_size) 8 else NULL),held)
      fit <- suppressWarnings(do.call(mlztbinom,args))
      sizes <- if(fixed_size) 8 else seq(max(x),200)
      ll <- function(size, mixture=0) sum(dztbinom(x = x, size = size, prob = 0.4, log = TRUE))
      oracle <- max(vapply(sizes,function(size) {
        return(ll(size))

      },0.))
      expect_equal(attr(fit,"logLik"),oracle,tolerance=1e-6)
      for(name in names(held)) expect_identical(unname(fit[name]),held[[name]])
      expect_setequal(attr(fit,"fixed"),c(if(fixed_size) "size",names(held)))

    }
  }
  for(p in c(-.1,1.1,NA_real_,Inf)) expect_error(mlztbinom(c(1,2),size=8,prob=p),"prob")
  expect_error(mlztbinom(c(1,2),size=8,prob=1))
  expect_error(mlztbinom(c(1,2),size=NULL,prob=0))
})

test_that("ztbinom: held endpoints are attained exactly when feasible", {
 for(p in c(0,1)) {
  f<-mlztbinom(rep(if(p==0)1 else 8,3),size=8,prob=p)
  expect_identical(unname(f["prob"]),p);expect_equal(attr(f,"logLik"),0)
 }
 expect_error(mlztbinom(1:3,size=8,prob=.4,reltol=0))
})

test_that("ztbinom: P3 unfinished optimizer stops", {
  x <- c(1,2,3,3,5)
  expect_error(mlztbinom(x, iterlim=1, reltol=1e-10, size=NULL),
               "[Ff]ail|converge|unresolved|exhausted|score check")
})

test_that("ztbinom: free size and probability agree with a reference-density oracle (n=20, n=100, ties, near-Poisson)", {
  # TASK-85/87: the free size+prob search is an exponential grid, not a
  # certified unimodality result (see mle.hpp); this independently
  # cross-checks it over the specific cases the task calls out, rather
  # than relying only on the n=250 case already covered above.
  set.seed(85087)
  oracle_ll <- function(x, size) {
    objective <- function(p) -sum(dztbinom(x = x, size = size, prob = p, log = TRUE))
    -optimize(objective, c(1e-9, 1 - 1e-9))$objective
  }
  oracle <- function(x, sizes) max(vapply(sizes, function(s) oracle_ll(x, s), 0))

  samples <- list(
    n20 = rztbinom(n = 20, size = 8, prob = 0.4),
    n100 = rztbinom(n = 100, size = 8, prob = 0.4),
    ties = c(2, 2, 2, 2, 3, 4, 5, 5),
    near_poisson = rztbinom(n = 100, size = 500, prob = 0.02)
  )
  for (x in samples) {
    # The near-Poisson sample can legitimately hit the infinite-size
    # escape instead of a finite fit (R6: a data-dependent "use
    # mlztpois", not a failure); accept either outcome.
    fit <- tryCatch(mlztbinom(x, size = NULL), error = identity)
    if (inherits(fit, "error")) {
      expect_match(conditionMessage(fit), "mlztpois")
      next
    }
    expect_true(is.finite(attr(fit, "logLik")))
    best <- oracle(x, unique(pmax(max(x), round(unname(fit["size"]) * c(.25,.5,.9,1,1.1,2,4)))))
    expect_gte(attr(fit, "logLik") + 1e-6, best)
    # Reference-density agreement at the fitted point itself.
    expect_equal(attr(fit, "logLik"),
                 sum(dztbinom(x = x, size = unname(fit["size"]), prob = unname(fit["prob"]), log = TRUE)),
                 tolerance = 1e-8)
  }
})

test_that("ztbinom attained point member evaluates and samples exactly", {
  fit <- mlztbinom(rep(5,4),size=5,prob=1)
  expect_equal(dml(c(4,5),fit),c(0,1))
  expect_equal(pml(c(4,5),fit),c(0,1))
  expect_equal(qml(.5,fit),5)
  expect_equal(rml(10,fit),rep(5,10))
})
