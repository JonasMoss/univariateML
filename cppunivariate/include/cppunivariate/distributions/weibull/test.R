context("mlweibull")

## Data generation.
set.seed(313)
small_data <- rweibull(100, 1, 1)
tiny_data <- rweibull(10, 3, 7)

## Checks if the ML is correct.
mle1 <- nlm(function(p) {
  -mean(dweibull(small_data, p[1], p[2], log = TRUE))
}, p = c(1, 1))$estimate

mle2 <- nlm(function(p) {
  -mean(dweibull(tiny_data, p[1], p[2], log = TRUE))
}, p = c(3, 7))$estimate

expect_equal(mle1, as.numeric(mlweibull(small_data)), tolerance = 1e-5)
expect_equal(mle2, as.numeric(mlweibull(tiny_data)), tolerance = 1e-5)

## Checks warning with small iterlim.
expect_warning(mlweibull(tiny_data, iterlim = 1))

## Finds errors with na and data out of bounds.
expect_error(mlweibull(c(tiny_data, NA)))
expect_error(mlweibull(c(0, tiny_data)))

## Checks that na.rm works as intended.
expect_equal(
  coef(mlweibull(small_data)),
  coef(mlweibull(c(small_data, NA), na.rm = TRUE))
)

## Is the log-likelihood correct?
est <- mlweibull(small_data, na.rm = TRUE)
expect_equal(
  sum(dweibull(small_data, est[1], est[2], log = TRUE)),
  attr(est, "logLik")
)

## Check class.
expect_equal(attr(est, "model"), "Weibull")
expect_equal(class(est), "univariateML")

## Check support.
expect_equal(class(attr(est, "support"))[[1]], "numeric")

## Check support.
expect_equal(class(attr(est, "support"))[[1]], "numeric")

## Explicit shape starts are now used and validated.
x_start <- simulate_default("mlweibull", 100)
a_start <- mlweibull(x_start, reltol = 1e-12)
b_start <- mlweibull(x_start, shape0 = 1.2 * coef(a_start)[["shape"]], reltol = 1e-12)
expect_equal(coef(b_start), coef(a_start), tolerance = 1e-8)
for (bad_start in c(0, -1, Inf, NA_real_)) {
  expect_error(mlweibull(x_start, shape0 = bad_start), "positive finite")
}

test_that("constant data have no attained weibull MLE", {
  expect_error(mlweibull(rep(2, 5)), "does not exist")
  expect_error(mlweibull(2), "does not exist")
})

test_that("weibull near ties have finite concentrated fits", {
  x <- 1+(0:4)*.Machine$double.eps
  f <- mlweibull(x)
  expect_gt(f[["shape"]],1e14)
  expect_true(is.finite(attr(f,"logLik")))
  expect_gt(attr(f,"logLik"),100)
})

test_that("P2 held subsets agree with multistart reference likelihoods", {
  set.seed(81029)
  id <- "weibull"
  pn <- c("shape","scale")
  held_default <- c(3.0,4.0)
  names(held_default) <- pn
  density <- stats::dweibull
  ref <- function(x,p) {
    suppressWarnings(do.call(density,c(list(x=x),as.list(p),list(log=TRUE))))
  }
  samples <- list(n20=simulate_default(paste0("ml",id),20),
                  n100=simulate_default(paste0("ml",id),100))
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
expect_error(mlweibull(rep(2,3),scale=2),"unbounded")
expect_true(is.finite(logLik(mlweibull(rep(2,3),shape=2))))
})

test_that("P2 held solver respects iterlim", {
  expect_error(suppressWarnings(mlweibull(c(2,3),scale=3,iterlim=1)))
})

test_that("P2 tiny held Weibull shape has the geometric-mean scale limit", {
  f <- mlweibull(c(1,4,16),shape=1e-200)
  expect_identical(f[["shape"]],1e-200)
  expect_equal(f[["scale"]],4)
})

test_that("P2 failed held solver is distinguished from constant-data collapse", {
  expect_error(suppressWarnings(mlweibull(rep(2,3),scale=3,iterlim=1)),"failed to converge")
})

test_that("P2 analytic held shape needs no iterative search", {
  expect_silent(mlweibull(c(1,2),shape=2,iterlim=1))
})
