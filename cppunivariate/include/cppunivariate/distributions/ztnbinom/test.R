test_that("ztnbinom agrees with a multi-start reference-density oracle", {
  set.seed(13226)
  x <- do.call(actuar::rztnbinom, list(250, size = 2, prob = 0.4))
  fit <- mlztnbinom(x)
  density <- actuar::dztnbinom
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
  best <- oracle()
  expect_gte(attr(fit,"logLik"), -best-1e-6)
  fixed <- mlztnbinom(x, size=2.5)
  expect_identical(attr(fixed,"fixed"), "size")
  expect_equal(unname(fixed["size"]), 2.5)
  expect_equal(attr(logLik(fixed),"df"), 1L)
  expect_gte(attr(fixed,"logLik"), -oracle(c(size=2.5))-1e-6)
  for(bad in c(-1,Inf,NaN)) expect_error(mlztnbinom(x,size=bad),"size")
  expect_error(mlztnbinom_(integer()),"nonempty")
  expect_error(mlztnbinom_(c(-1L,1L)),"count")
  expect_error(mlztnbinom(c(0,1,2)))
  expect_equal(attr(mlztnbinom(rep(1,4)),"logLik"),0)
})

test_that("ztnbinom declares its infinite-size escape", {
 x <- c(1,2,2,3,3,3)
 expect_error(mlztnbinom(x), "Use `mlztpois`")
 expect_s3_class(mlztnbinom(x,size=20), "univariateML")
})

test_that("ztnbinom returns the attained logarithmic member", {
 x <- c(1,1,1,1,1,2,3,10,30); fit <- mlztnbinom(x)
 expect_equal(unname(fit["size"]),0)
 fixed <- mlztnbinom(x,size=0)
 expect_identical(attr(fixed,"fixed"),"size")
 expect_equal(attr(fixed,"logLik"),attr(fit,"logLik"))
})

test_that("ztnbinom preserves fixed size at attained boundaries", {
 zero <- mlztnbinom(c(1,1),size=5)
 expect_equal(unname(zero["size"]),5)
 expect_identical(attr(zero,"fixed"),"size")
 expect_equal(attr(logLik(zero),"df"),1L)
 one <- mlztnbinom(c(1,1),size=5)
 expect_equal(unname(one["size"]),5)
})

test_that("ztnbinom fixed zero size agrees with the reference oracle", {
 x <- c(1,1,1,2,3,10,30)
 fit <- mlztnbinom(x,size=0)
 objective <- function(t) -sum(actuar::dztnbinom(x,size=0,prob=plogis(t[1]),log=TRUE))
 best <- min(vapply(c(-1,0,1),function(s)optim(rep(s,1),objective,method="BFGS",control=list(reltol=1e-12))$value,numeric(1)))
 expect_equal(attr(fit,"logLik"),-best,tolerance=1e-7)
 expect_equal(sum(actuar::dztnbinom(x,size=0,prob=fit["prob"],log=TRUE)),attr(fit,"logLik"),tolerance=1e-10)
})
