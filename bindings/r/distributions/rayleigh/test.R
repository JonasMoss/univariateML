context("mlrayleigh")

## Data generation.
set.seed(313)
small_data <- rrayleigh(n = 100, sigma = 2)
tiny_data <- rrayleigh(n = 10, sigma = 3)

## Finds errors with na and data out of bounds.
expect_error(mlrayleigh(c(-.1, tiny_data)))
expect_error(mlrayleigh(c(tiny_data, NA)))

## Checks that na.rm works as intended.
expect_equal(
  coef(mlrayleigh(small_data)),
  coef(mlrayleigh(c(small_data, NA), na.rm = TRUE))
)

## Is the log-likelihood correct?
est <- mlrayleigh(small_data, na.rm = TRUE)
expect_equal(
  sum(drayleigh(x = small_data, sigma = est, log = TRUE)),
  attr(est, "logLik")
)

## Check class.
expect_equal(attr(est, "model"), "Rayleigh")
expect_equal(class(est), "univariateML")

## Check support.
expect_equal(class(attr(est, "support"))[[1]], "numeric")

test_that("Rayleigh fits extreme scales and rejects all zero", {
  for (s in c(1e-300, 1e300)) {
    f <- mlrayleigh(c(1, 2, 3) * s)
    expect_equal(f[["sigma"]] / s, sqrt(7/3))
    expect_true(is.finite(attr(f, "logLik")))
  }
  expect_error(mlrayleigh(rep(0, 5)), "identically zero")
})

test_that("any zero makes the Rayleigh likelihood zero", {
  for (x in list(c(0,0,1,2), c(0,1), c(0,0)))
    expect_error(mlrayleigh(x), "identically zero")
  for (x in list(c(.1, .2, 1), c(1, 2, 5))) {
    fit <- mlrayleigh(x)
    oracle <- optimize(function(s) -sum(drayleigh(x = x, sigma = s, log = TRUE)), c(.01,10))
    expect_equal(as.numeric(logLik(fit)), -oracle$objective, tolerance=1e-8)
  }
})

test_that("P2 held subsets agree with multistart reference likelihoods", {
  skip_unless_tier("full")
  set.seed(81029)
  id <- "rayleigh"
  pn <- c("sigma")
  held_default <- c(2.0)
  names(held_default) <- pn
  density <- drayleigh
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
expect_error(mlrayleigh(c(0,1),sigma=2),"identically zero")
expect_error(mlrayleigh(0,sigma=2),"identically zero")
})
