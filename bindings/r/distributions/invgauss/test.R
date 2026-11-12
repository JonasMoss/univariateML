context("mlinvgauss")

## Data generation.
set.seed(313)
tiny_data <- rinvgauss(n = 10, mean = 1, shape = 7)
small_data <- rinvgauss(n = 100, mean = 10, shape = 3)
medium_data <- rinvgauss(n = 1000, mean = 1/2, shape = 2)
large_data <- rinvgauss(n = 10000, mean = 20, shape = 13)

## Checks if the ML is correct.

mle1 <- suppressWarnings(nlm(function(p) {
  -sum(dinvgauss(x = tiny_data, mean = p[1], shape = p[2], log = TRUE))
}, p = c(1, 7)))

mle2 <- suppressWarnings(nlm(function(p) {
  -sum(dinvgauss(x = small_data, mean = p[1], shape = p[2], log = TRUE))
}, p = c(10, 3)))

mle3 <- suppressWarnings(nlm(function(p) {
  -sum(dinvgauss(x = medium_data, mean = p[1], shape = p[2], log = TRUE))
}, p = c(1 / 2, 2)))

mle4 <- suppressWarnings(nlm(function(p) {
  -sum(dinvgauss(x = large_data, mean = p[1], shape = p[2], log = TRUE))
}, p = c(20, 13)))

## Checks estimates.
expect_equal(mle1$estimate, as.numeric(mlinvgauss(tiny_data)),
  tolerance = 1e-5
)
expect_equal(mle2$estimate, as.numeric(mlinvgauss(small_data)),
  tolerance = 1e-5
)
expect_equal(mle3$estimate, as.numeric(mlinvgauss(medium_data)),
  tolerance = 1e-5
)
expect_equal(mle4$estimate, as.numeric(mlinvgauss(large_data)),
  tolerance = 1e-5
)

## Checks logLiks.
expect_equal(-mle1$minimum, attr(mlinvgauss(tiny_data), "logLik"),
  tolerance = 1e-5
)
expect_equal(-mle2$minimum, attr(mlinvgauss(small_data), "logLik"),
  tolerance = 1e-5
)
expect_equal(-mle3$minimum, attr(mlinvgauss(medium_data), "logLik"),
  tolerance = 1e-5
)
expect_equal(-mle4$minimum, attr(mlinvgauss(large_data), "logLik"),
  tolerance = 1e-5
)

## Finds errors with na and data out of bounds.
expect_error(mlinvgauss(c(tiny_data, 0)))
expect_error(mlinvgauss(c(tiny_data, NA)))

## Checks that na.rm works as intended.
expect_equal(
  coef(mlinvgauss(small_data)),
  coef(mlinvgauss(c(small_data, NA), na.rm = TRUE))
)

## Check class.
est <- mlinvgauss(small_data, na.rm = TRUE)
expect_equal(attr(est, "model"), "Inverse Gaussian")
expect_equal(class(est), "univariateML")

test_that("inverse Gaussian resolves scale extremes and near ties", {
  x <- c(.5, 1, 2, 4)
  f <- mlinvgauss(x)
  for (s in c(1e-300, 1e300)) {
    g <- mlinvgauss(x*s)
    expect_equal(as.numeric(g)/s, as.numeric(f), tolerance=1e-8)
    expect_equal(attr(g,"logLik")+length(x)*log(s), attr(f,"logLik"), tolerance=1e-8)
  }
  g <- mlinvgauss(1+(0:4)*.Machine$double.eps)
  expect_true(all(is.finite(g)))
  expect_error(mlinvgauss(rep(1,5)), "constant data")
})

test_that("P2 held subsets agree with multistart reference likelihoods", {
  skip_unless_tier("full")
  set.seed(81029)
  id <- "invgauss"
  pn <- c("mean","shape")
  held_default <- c(3.0,4.0)
  names(held_default) <- pn
  density <- dinvgauss
  ref <- function(x,p) {
    suppressWarnings(do.call(density,c(list(x=x),as.list(p),list(log=TRUE))))
  }
  samples <- list(n20=ml_simulate(paste0("ml",id),20),
                  n100=ml_simulate(paste0("ml",id),100))
  samples$ties <- rep(samples$n20[1:5],each=4)
  samples$small <- samples$n20[1:2]
  samples$singleton <- samples$n20[1]
  samples$constant <- rep(samples$n20[1],5)
  samples$limit <- 1/rnorm(100)^2
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
expect_error(mlinvgauss(rep(2,3),mean=2),"constant data")
expect_true(is.finite(logLik(mlinvgauss(rep(2,3),shape=2))))
})

test_that("concentrated inverse Gaussian logLik evaluates the public point", {
  x <- c(2.4377209513444593,2.4377209513468974,2.437720951349335,
         2.4377209513517726,2.4377209513566482)
  fit <- mlinvgauss(x)
  ll <- sum(dml(x,fit,log=TRUE))
  expect_lte(abs(as.numeric(logLik(fit))-ll),1e-10*max(1,abs(ll)))
  expect_lte(abs(as.numeric(logLik(fit))-123.89205063011403),1e-10*123.89205063011403)
})
