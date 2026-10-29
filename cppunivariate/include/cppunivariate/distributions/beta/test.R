context("mlbeta")

## Data generation.
set.seed(313)
small_data <- runif(100)
tiny_data <- rbeta(10, 2, 7)

## Checks if the ML is correct.
mle1 <- nlm(function(p) {
  -mean(dbeta(small_data, p[1], p[2], log = TRUE))
}, p = c(1, 1))$estimate

mle2 <- nlm(function(p) {
  -mean(dbeta(tiny_data, p[1], p[2], log = TRUE))
}, p = c(1, 1))$estimate

expect_equal(mle1, as.numeric(mlbeta(small_data)), tolerance = 1e-5)
expect_equal(mle2, as.numeric(mlbeta(tiny_data)), tolerance = 1e-5)

## Finds errors with na and data out of bounds.
expect_error(mlbeta(c(0, tiny_data)))
expect_error(mlbeta(c(tiny_data, 1)))
expect_error(mlbeta(c(tiny_data, NA)))

test_that("constant data have no attained beta MLE", {
  expect_error(mlbeta(rep(.4, 5)), "does not exist")
  expect_error(mlbeta(.4), "does not exist")
})

test_that("beta fits data at both ends of the open support", {
  x <- c(rep(1e-10, 5), rep(1 - 1e-10, 5))
  f <- mlbeta(x)
  oracle <- optim(log(c(.05, .05)), function(p) -sum(dbeta(x, exp(p[1]), exp(p[2]), log = TRUE)), method = "BFGS", control = list(reltol = 1e-12))
  expect_equal(attr(f, "logLik"), -oracle$value, tolerance = 1e-8)
  expect_equal(attr(f, "logLik"), sum(dbeta(x, f[[1]], f[[2]], log = TRUE)), tolerance = 1e-10)
})

test_that("P2 every held subset agrees with independent multistart density fits", {
  set.seed(8129)
  samples <- list(rbeta(20,2,3), rbeta(100,2,3),
                  runif(20,.01,.99), runif(100,.01,.99),
                  c(.2,.2,.6,.8), c(.2,.8), rep(.4,5), .4,
                  c(1e-5, .01, .5, .99, 1-1e-5))
  values <- c(shape1=2, shape2=3)
  for (values in list(c(shape1=2,shape2=3),c(shape1=.2,shape2=.3),c(shape1=1,shape2=1),c(shape1=20,shape2=30))) {
  for (x in samples) for (mask in 1:3) {
    held <- names(values)[as.logical(intToBits(mask)[1:2])]
    free <- setdiff(names(values),held)
    fit <- do.call(mlbeta,c(list(x=x),as.list(values[held])))
    expect_identical(attr(fit,"fixed"),held)
    expect_identical(unname(coef(fit)[held]),unname(values[held]))
    expect_equal(attr(logLik(fit),"df"),2L-length(held))
    ll <- function(p) suppressWarnings(sum(dbeta(x,p[1],p[2],log=TRUE)))
    expect_equal(as.numeric(logLik(fit)),ll(coef(fit)),tolerance=1e-9)
    if (length(free)) {
      objective <- function(z) {
        p <- values
        p[free] <- exp(z)
        v <- -ll(p)
        if (is.finite(v)) v else 1e100
      }
      refs <- vapply(c(-3,0,3), function(start)
        nlminb(start,objective,control=list(iter.max=1000,rel.tol=1e-13))$objective,0.0)
      expect_lt(abs(as.numeric(logLik(fit))+min(refs)),1e-6*length(x))
    }
  }
  }
  for (p in names(values)) for (bad in c(NA_real_,Inf,-Inf,0,-1))
    expect_error(do.call(mlbeta,c(list(x=c(.2,.8)),setNames(list(bad),p))))
  for (edge in c(0,1)) for (mask in 1:3) {
    held <- names(values)[as.logical(intToBits(mask)[1:2])]
    expect_error(do.call(mlbeta,c(list(x=c(edge,.4)),as.list(values[held]))))
  }
})

test_that("P2 held shape solves stop on exhausted brackets", {
  expect_error(mlbeta(c(.2,.8),shape2=2,iterlim=1),"failed")
})
