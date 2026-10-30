context("mllinvweibull")

## Data generation.
set.seed(313)
small_data <- rinvweibull(n = 100, shape = 7, scale = 1/3)
tiny_data <- rinvweibull(n = 10, shape = 1, scale = 1)

## Finds errors with na and data out of bounds.
expect_error(mlinvweibull(c(tiny_data, NA)))
expect_error(mlinvweibull(c(tiny_data, -1)))
expect_error(mlinvweibull(c(tiny_data, 0)))

# Check correctness
expect_equal(mlweibull(1 / small_data)[1], mlinvweibull(small_data)[1])

## Checks that na.rm works as intended.
expect_equal(
  coef(mlinvweibull(small_data)),
  coef(mlinvweibull(c(small_data, NA), na.rm = TRUE))
)

## Is the log-likelihood correct?
est <- mlinvweibull(small_data, na.rm = TRUE)
expect_equal(
  sum(dinvweibull(small_data, est[1], scale = est[2], log = TRUE)),
  attr(est, "logLik")
)

## Check class.
expect_equal(attr(est, "model"), "Inverse Weibull")
expect_equal(class(est), "univariateML")

# Check names.
expect_equal(names(est), c("shape", "scale"))

## Check support.
expect_equal(class(attr(est, "support"))[[1]], "numeric")

## Explicit shape starts are now used and validated.
x_start <- simulate_default("mlinvweibull", 100)
a_start <- mlinvweibull(x_start, reltol = 1e-12)
b_start <- mlinvweibull(x_start, shape0 = 1.2 * coef(a_start)[["shape"]], reltol = 1e-12)
expect_equal(coef(b_start), coef(a_start), tolerance = 1e-8)
for (bad_start in c(0, -1, Inf, NA_real_)) {
  expect_error(mlinvweibull(x_start, shape0 = bad_start), "positive finite")
}

test_that("constant data have no attained invweibull MLE", {
  expect_error(mlinvweibull(rep(2, 5)), "does not exist")
  expect_error(mlinvweibull(2), "does not exist")
})

test_that("invweibull near ties have finite concentrated fits", {
  x <- 1+(0:4)*.Machine$double.eps
  f <- mlinvweibull(x)
  expect_gt(f[["shape"]],1e14)
  expect_true(is.finite(attr(f,"logLik")))
  expect_gt(attr(f,"logLik"),100)
})

test_that("P2 held subsets agree with multistart reference likelihoods", {
  set.seed(81029)
  id <- "invweibull"
  pn <- c("shape","scale")
  held_default <- c(2.0,3.0)
  names(held_default) <- pn
  density <- dinvweibull
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
expect_error(mlinvweibull(rep(.5,3),scale=0.5),"unbounded")
expect_true(is.finite(logLik(mlinvweibull(rep(.5,3),shape=2))))
})

test_that("P2 held solver respects iterlim", {
  expect_error(suppressWarnings(mlinvweibull(c(2,3),scale=1/3,iterlim=1)))
})

test_that("P2 failed held solver is distinguished from constant-data collapse", {
  expect_error(suppressWarnings(mlinvweibull(rep(2,3),scale=1/3,iterlim=1)),"failed to converge")
})

test_that("P2 analytic held shape needs no iterative search", {
  expect_silent(mlinvweibull(c(1,2),shape=2,iterlim=1))
})

test_that("invweibull: P3 unfinished optimizer stops", {
  g <- jsonlite::fromJSON(file.path(Sys.getenv("UMVL_DISTRIBUTIONS_DIR"), "invweibull", "golden.json"))
  x <- g$data
  expect_error(mlinvweibull(x, iterlim=1, reltol=1e-10),
               "[Ff]ail|converge|unresolved|exhausted|score check")
})
