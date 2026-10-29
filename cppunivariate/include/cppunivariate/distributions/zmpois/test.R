test_that("zmpois agrees with a multi-start reference-density oracle", {
  set.seed(13019)
  x <- do.call(actuar::rzmpois, list(250, lambda = 2, p0 = 0.3))
  fit <- mlzmpois(x)
  density <- actuar::dzmpois
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
  expect_error(mlzmpois_(integer()),"nonempty")
  expect_error(mlzmpois_(c(-1L,1L)),"count")
  expect_warning(zero <- mlzmpois(c(0,0,0)),"unidentified")
  expect_equal(unname(zero["p0"]),1)
  expect_warning(two <- mlzmpois(c(0,0,1,1)),"mlbinom")
  expect_equal(attr(two,"logLik"),attr(mlbinom(c(0,0,1,1),size=1),"logLik"))
  expect_equal(unname(mlzmpois(x[x>0])["p0"]),0)
})
