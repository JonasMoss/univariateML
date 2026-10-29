context("mlllogis")

## Data generation.
set.seed(313)
small_data <- actuar::rllogis(100, 2, 3)
tiny_data <- actuar::rllogis(10, 1, 1)

## Finds errors with na and data out of bounds.
expect_error(mlllogis(c(tiny_data, NA)))
expect_error(mlllogis(c(tiny_data, -1)))

# Check correctness
obj_1 <- mllogis(log(tiny_data))
obj_2 <- mlllogis(tiny_data)

expect_equal(
  unname(obj_1[1]),
  unname(-log(obj_2)[2])
)

expect_equal(
  unname(obj_1[2]),
  unname(1 / obj_2[1])
)

## Checks that na.rm works as intended.
expect_equal(
  coef(mlllogis(small_data)),
  coef(mlllogis(c(small_data, NA), na.rm = TRUE))
)

## Is the log-likelihood correct?
est <- mlllogis(small_data, na.rm = TRUE)
expect_equal(
  sum(actuar::dllogis(small_data, est[1], est[2], log = TRUE)),
  attr(est, "logLik")
)

## Check class.
expect_equal(attr(est, "model"), "Loglogistic")
expect_equal(class(est), "univariateML")

# Check names.
expect_equal(names(est), c("shape", "rate"))

test_that("llogis handles a seeded sample with a large outlier", {
  set.seed(7)
  x <- c(actuar::rllogis(199, 2, 1), 1e3)
  objective <- function(p) -sum(actuar::dllogis(x, exp(p[1]), exp(p[2]), log = TRUE))
  oracle <- optim(log(c(2, 1)), objective,
    method = "Nelder-Mead", control = list(reltol = 1e-14, maxit = 10000))
  expected <- exp(oracle$par)
  fit <- mlllogis(x)
  expect_true(all(is.finite(fit)))
  expect_true(all(abs(as.numeric(fit) - expected) <= 1e-4 * abs(expected)))
  expect_gte(attr(fit, "logLik"), -oracle$value - 1e-6)
})

test_that("constant log-logistic data have no attained MLE", {
  expect_error(mlllogis(rep(1,5)),"does not exist")
})

test_that("P2 held subsets agree with multistart reference likelihoods", {
  set.seed(81029)
  id <- "llogis"
  pn <- c("shape","rate")
  held_default <- c(1.0,2.0)
  names(held_default) <- pn
  density <- actuar::dllogis
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
expect_error(mlllogis(rep(.5,3),rate=2),"unbounded")
expect_true(is.finite(logLik(mlllogis(rep(.5,3),shape=2))))
})

test_that("P2 held solver respects iterlim", {
  expect_error(suppressWarnings(mlllogis(c(2,3),rate=1,iterlim=1)))
})

test_that("P2 failed held solver is distinguished from constant-data collapse", {
  expect_error(suppressWarnings(mlllogis(rep(2,3),rate=1,iterlim=1)),"failed to converge")
})
