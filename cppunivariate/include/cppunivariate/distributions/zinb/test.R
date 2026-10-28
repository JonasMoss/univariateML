context("mlzinb")

zinb_oracle <- function(x, size = NULL) {
  objective <- function(t) {
    r <- if (is.null(size)) exp(t[1]) else size
    p <- plogis(t[if (is.null(size)) 2 else 1])
    pi <- plogis(t[if (is.null(size)) 3 else 2])
    -sum(extraDistr::dzinb(x, r, p, pi, log = TRUE))
  }
  min(vapply(c(.2, 1, 5, 20), function(r) {
    start <- c(if (is.null(size)) log(r), qlogis(.4), qlogis(.2))
    optim(start, objective, method = "BFGS", control = list(maxit=2000, reltol=1e-12))$value
  }, numeric(1)))
}

test_that("profiled ZINB agrees with multistart external oracle", {
  set.seed(718)
  for (r in c(.5, 2, 5)) {
    x <- extraDistr::rzinb(1500, r, .35, .3)
    fit <- mlzinb(x)
    expect_equal(attr(fit,"logLik"), -zinb_oracle(x), tolerance=1e-7)
    fixed <- mlzinb(x, size=1.5)
    expect_equal(unname(fixed["size"]), 1.5)
    expect_equal(attr(fixed,"fixed"), "size")
    expect_equal(attr(logLik(fixed),"df"), 2L)
    expect_equal(attr(logLik(fit),"df"), 3L)
    expect_equal(attr(fixed,"logLik"), -zinb_oracle(x,1.5), tolerance=1e-7)
  }
})

test_that("ZINB faces and fixed-size subset have the declared behavior", {
  # Underdispersed positive counts force the ZIP face; no moment test in fitter.
  expect_error(mlzinb(c(rep(0,50),rep(4,50))), "Use `mlzip`")
  expect_error(mlzinb(rep(2,100)), "Use `mlpois`")
  expect_s3_class(mlzinb(rep(2,100),size=1.5),"univariateML")
  for (bad in c(0,-1,Inf,-Inf,NaN))
    expect_error(mlzinb(0:4,size=bad), "size must")
})

test_that("all-zero representative supports external probability evaluation", {
  expect_warning(fit <- mlzinb(rep(0,10)), "not unique")
  expect_equal(as.numeric(fit), c(1,1,0))
  expect_equal(dml(c(0,1),fit),c(1,0))
  expect_equal(pml(c(-1,0,1),fit),c(0,1,1))
  expect_equal(qml(c(0,.5,1),fit),c(0,0,0))
  expect_warning(fixed <- mlzinb(rep(0,10),size=.5), "not unique")
  expect_equal(as.numeric(fixed),c(.5,1,0))
})
