# Independent oracle: ordinary reference density, joint log-parameter optim,
# multiple starts; no production scale profile is used.
ht_oracle <- function(x, sigma = NULL) {
  objective <- function(p) -sum(extraDistr::dht(x, exp(p[1]), if(is.null(sigma)) exp(p[2]) else sigma, log=TRUE))
  starts <- if (is.null(sigma)) expand.grid(log(c(.02,.1,.5,2,10,100)),log(c(.1,1,10)*median(x))) else matrix(log(c(.02,.1,.5,2,10,100)),ncol=1)
  fits <- lapply(seq_len(nrow(starts)),function(i) optim(as.numeric(starts[i,]), objective, method=if(is.null(sigma)) "Nelder-Mead" else "BFGS", control=list(maxit=4000,reltol=1e-12)))
  fits[[which.min(vapply(fits, `[[`, numeric(1), "value"))]]
}
test_that("half-t global profile agrees with independent multistart density oracle", {
  set.seed(213)
  samples <- list(c(.12773,12.5979,3.91782,2.58658,.138212), c(.001,.01,.1,1,10,100), extraDistr::rht(80,.5,2), extraDistr::rht(100,3,1), c(.0001,.2,.3,.4,5,1000))
  for(x in samples) {
    f <- mlht(x); o <- ht_oracle(x)
    expect_equal(attr(f,"logLik"), -o$value, tolerance=1e-8)
    expect_equal(log(unname(coef(f))),o$par,tolerance=2e-4)
    expect_equal(sum(dml(x,f,log=TRUE)),attr(f,"logLik"),tolerance=1e-10)
    expect_equal(mlht(x*3)[["nu"]], f[["nu"]], tolerance=1e-5)
    expect_equal(mlht(x*3)[["sigma"]], 3*f[["sigma"]], tolerance=1e-5)
  }
})
test_that("half-t held subsets have their own existence conditions", {
  x <- c(0,1,2,10)
  for(nu in c(.5,1,3,100)) {
    f <- mlht(x,nu=nu)
    o <- optimize(function(t) -sum(extraDistr::dht(x,nu,exp(t),log=TRUE)),c(-20,20),tol=1e-11)
    expect_equal(log(f[["sigma"]]),o$minimum,tolerance=1e-6)
    expect_equal(attr(f,"fixed"),"nu")
    expect_equal(attr(logLik(f),"df"),1L)
  }
  f <- mlht(c(.001,.01,.1,1,10,100),sigma=2)
  o <- ht_oracle(c(.001,.01,.1,1,10,100),sigma=2)
  expect_equal(attr(f,"logLik"),-o$value,tolerance=1e-8)
  expect_equal(f[["sigma"]],2)
  expect_equal(attr(f,"fixed"),"sigma")
  f <- mlht(c(0,0,0),nu=1,sigma=2)
  expect_equal(attr(f,"fixed"),c("nu","sigma"))
  expect_equal(attr(logLik(f),"df"),0L)
  expect_equal(attr(f,"logLik"),sum(extraDistr::dht(c(0,0,0),1,2,log=TRUE)))
  # Zeros are safe when scale is held and df alone is estimated.
  f <- mlht(c(0,.001,100),sigma=1)
  expect_true(f[["nu"]]>0)
  expect_error(mlht(c(0,1,2)),"unbounded.*sigma -> 0")
  expect_error(mlht(c(0,1),nu=1),"finite unattained")
  expect_error(mlht(c(0,1),nu=.5),"unbounded")
  expect_error(mlht(c(0,0),nu=3),"unbounded")
  expect_error(mlht(c(1,1,1)),"mlhnorm\\(x, mu = 0\\)")
  expect_error(mlht(c(0,0),sigma=1),"mlhnorm")
  for(v in c(0,-1,Inf,NaN)) {
    expect_error(mlht(c(1,2),nu=v),"positive finite")
    expect_error(mlht(c(1,2),sigma=v),"positive finite")
  }
  expect_error(mlht(c(-1,2)),"support")
  expect_error(mlht(c(1,Inf)))
  expect_error(mlht(numeric()))
  expect_error(mlht(1,nu=1e-300),"failed")
})
test_that("half-t reference tails and seeded RNG", {
  f <- mlht(c(1,2),nu=3,sigma=2)
  expect_equal(dml(c(-1,0,1,10),f),extraDistr::dht(c(-1,0,1,10),3,2))
  for(lower in c(TRUE,FALSE)) for(logs in c(TRUE,FALSE)) {
    u <- c(0,.001,.5,.999,1)
    expect_equal(pml(qml(if(logs)log(u)else u,f,lower,logs),f,lower),u,tolerance=1e-10)
  }
  for(lower in c(TRUE,FALSE)) for(lp in if(lower) -100 else c(-100,-1000))
    expect_equal(pml(qml(lp,f,lower,TRUE),f,lower,TRUE),lp,tolerance=1e-10)
  expect_true(is.finite(dml(1e200,f,log=TRUE)))
  set.seed(7); a <- rml(30,f)
  set.seed(7); expect_identical(a,extraDistr::rht(30,3,2))
  set.seed(1); boot <- bootstrapml(f,reps=3,reducer=identity)
  expect_true(all(boot["nu",]==3 & boot["sigma",]==2))
})
