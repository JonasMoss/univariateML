context("mlexp")

## Data generation.
set.seed(313)
small_data <- rexp(100, 2)
tiny_data <- rexp(10, 3)

## Finds errors with na and data out of bounds.
expect_error(mlexp(c(-.1, tiny_data)))
expect_error(mlexp(c(tiny_data, NA)))

## Checks that na.rm works as intended.
expect_equal(
  coef(mlexp(small_data)),
  coef(mlexp(c(small_data, NA), na.rm = TRUE))
)

## Is the log-likelihood correct?
est <- mlexp(small_data, na.rm = TRUE)
expect_equal(
  sum(dexp(small_data, est, log = TRUE)),
  attr(est, "logLik")
)

## Check class.
expect_equal(attr(est, "model"), "Exponential")
expect_equal(class(est), "univariateML")

## Check support.
expect_equal(class(attr(est, "support"))[[1]], "numeric")

test_that("exponential rejects all zero and avoids sum overflow", {
  expect_error(mlexp(rep(0, 5)), "does not exist")
  f <- mlexp(c(1e308, 1e308))
  expect_equal(f[["rate"]] / 1e-308, 1)
  expect_true(is.finite(attr(f, "logLik")))
})

test_that("P2 held subsets agree with multistart reference likelihoods", {
  skip_unless_tier("full")
  set.seed(81029)
  id <- "exp"
  pn <- c("rate")
  held_default <- c(1.0)
  names(held_default) <- pn
  density <- stats::dexp
  ref <- function(x,p) {
    suppressWarnings(do.call(density,c(list(x=x),as.list(p),list(log=TRUE))))
  }
  samples <- list(n20=ml_simulate(paste0("ml",id),20),
                  n100=ml_simulate(paste0("ml",id),100))
  samples$ties <- rep(samples$n20[1:5],each=4)
  samples$small <- samples$n20[1:2]
  samples$singleton <- samples$n20[1]
  samples$constant <- rep(samples$n20[1],5)

  subsets <- lapply(1:(2^length(pn)-1),function(mask)
    pn[as.logical(intToBits(mask)[seq_along(pn)])])
  for (x in samples) for (held in subsets) {
    hv <- held_default
    args <- c(list(x=x),as.list(hv[held]))
    f <- do.call(get(paste0("ml",id)),args)
    expect_identical(unname(as.numeric(f[held])),unname(hv[held]))
    expect_setequal(attr(f,"fixed"),held)
    p <- setNames(as.numeric(f),pn)
    expect_equal(as.numeric(logLik(f)),sum(ref(x,p)),tolerance=1e-8)
    free <- setdiff(pn,held)
    decode <- function(z) {
      q <- hv
      q[free] <- exp(z)
      q
    }
    cost <- function(z) {
      if(any(!is.finite(z)) || any(abs(z)>40)) return(1e100)
      v <- suppressWarnings(-sum(ref(x,decode(z))))
      if(is.finite(v)) v else 1e100
    }
    if(length(free)) {
      start <- log(hv[free])
      oracles <- lapply(c(-3,-1,0,1,3),function(d)
        optim(start+d,cost,method="BFGS",control=list(maxit=1000,reltol=1e-12))$value)
      best <- min(unlist(oracles))
    } else best <- -sum(ref(x,hv))
    expect_equal(as.numeric(logLik(f)),-best,tolerance=1e-6)
  }
  for (p in pn) for (bad in c(NA_real_,NaN,Inf,-Inf,if(p=="meanlog") numeric() else c(0,-1))) {
    expect_error(do.call(get(paste0("ml",id)),c(list(x=samples$n20),setNames(list(bad),p))))
  }
})

test_that("P2 held existence boundaries", {
expect_error(mlexp(rep(0,3)),"does not exist|finite|failed|unbounded")
expect_true(is.finite(logLik(mlexp(rep(0,3),rate=2))))
})

test_that("right-censoring bounds below support carry no exposure", {
  x <- cbind(c(-1,2,3), c(Inf,2,Inf))
  zero <- x; zero[1,1] <- 0
  one <- x; one[1,1] <- 1
  fits <- list(function(z) mlexp(z), function(z) mlweibull(z, shape=2),
               function(z) mlrayleigh(z), function(z) mllomax(z, lambda=1),
               function(z) mlpareto(z, b=1))
  flat <- cbind(c(-1,0), c(Inf,Inf))
  for (i in seq_along(fits)) {
    f <- fits[[i]](x); reference <- fits[[i]](if(i==5L) one else zero)
    expect_equal(as.numeric(f), as.numeric(reference), tolerance=1e-13)
    expect_equal(as.numeric(logLik(f)), as.numeric(logLik(reference)), tolerance=1e-13)
    uninformative <- fits[[i]](flat)
    expect_equal(as.numeric(logLik(uninformative)), 0)
    expect_false(attr(uninformative,"unique"))
  }
  p <- mlpareto(cbind(c(.5,2),c(Inf,2)), b=1)
  expect_equal(unname(p[["a"]]), 1/log(2), tolerance=1e-13)
  e <- tryCatch(mlpareto(cbind(c(.5,1),c(Inf,1)), b=1), error=identity)
  expect_s3_class(e,"univariateML_nomle")
  e <- tryCatch(mlpareto(cbind(c(.5,2),c(.5,Inf)), b=1), error=identity)
  expect_s3_class(e,"univariateML_invalid")
})

test_that("held-scale censored Weibull owns its point limit and small interior root", {
  right <- function(x) cbind(x,rep(Inf,length(x)))
  e <- tryCatch(mlweibull(right(c(.25,.5)),scale=1),error=identity)
  expect_s3_class(e,"univariateML_limit")
  expect_identical(e$model,"unavailable:point-mass")
  expect_equal(e$estimates,c(point=1))
  expect_equal(e$limit_loglik,0)
  e <- tryCatch(mlweibull(right(c(.5,2)),scale=1),error=identity)
  expect_s3_class(e,"univariateML_nomle")
  f <- mlweibull(right(c(.25,2)),scale=1)
  expect_equal(unname(f[["shape"]]),1/3,tolerance=1e-10)
  f <- mlweibull(right(c(1.5,2/3)),scale=1)
  expect_gt(unname(f[["shape"]]),0)
  expect_true(attr(f,"certified"))
})
