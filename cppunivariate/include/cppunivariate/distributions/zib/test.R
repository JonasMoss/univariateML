test_that("zib agrees with a multi-start reference-density oracle", {
  set.seed(12670)
  x <- do.call(extraDistr::rzib, list(250, size = 8, prob = 0.4, pi = 0.4))
  fit <- mlzib(x, size = NULL)
  density <- extraDistr::dzib
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
