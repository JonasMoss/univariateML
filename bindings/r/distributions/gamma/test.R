context("mlgamma")

## Data generation.
set.seed(313)
small_data <- stats::rgamma(100, 1, 1)
tiny_data <- stats::rgamma(10, 3, 7)
data3 <- c(0, tiny_data)

## Checks if the ML is correct.
gamma_density_oracle <- function(x) {
  fit <- optim(log(c(1,1)), function(p)
    -mean(stats::dgamma(x, exp(p[1]), scale=exp(p[2]), log=TRUE)),
    method="BFGS", control=list(reltol=1e-14,maxit=1000,ndeps=c(1e-5,1e-5)))
  exp(fit$par)
}
mle1 <- gamma_density_oracle(small_data)
mle2 <- gamma_density_oracle(tiny_data)

expect_equal(mle1, as.numeric(mlgamma(small_data)), tolerance = 1e-5)
expect_equal(mle2, as.numeric(mlgamma(tiny_data)), tolerance = 1e-5)

## Checks warning with small iterlim.
## expect_warning(mlgamma(tiny_data, iterlim = 1))

## Finds errors with na and data out of bounds.
expect_error(mlgamma(c(tiny_data, NA)))
expect_error(mlgamma(c(tiny_data, 0)))

## Checks that na.rm works as intended.
expect_equal(
  coef(mlgamma(small_data)),
  coef(mlgamma(c(small_data, NA), na.rm = TRUE))
)

## Is the log-likelihood correct?
est <- mlgamma(small_data, na.rm = TRUE)
expect_equal(
  sum(stats::dgamma(small_data, est[1], scale = est[2], log = TRUE)),
  attr(est, "logLik")
)

## Check class.
expect_equal(attr(est, "model"), "Gamma")
expect_equal(class(est), "univariateML")

## Check support.
expect_equal(class(attr(est, "support"))[[1]], "numeric")

test_that("constant data have no attained gamma MLE", {
  expect_error(mlgamma(rep(2, 5)), "does not exist")
  expect_error(mlgamma(2), "does not exist")
})

test_that("gamma resolves near ties and extreme scales", {
  x <- 1 + (0:4) * .Machine$double.eps
  f <- mlgamma(x)
  expect_gt(f[["shape"]], 1e29)
  expect_true(is.finite(attr(f, "logLik")))
  expect_equal(f[["shape"]] * f[["scale"]], mean(x))
  y <- c(.2, .8, 1, 2, 4)
  base <- mlgamma(y)
  for (s in c(1e-300, 1e300)) {
    f <- mlgamma(y * s)
    expect_equal(f[["shape"]], base[["shape"]], tolerance = 1e-8)
    expect_equal(f[["scale"]], base[["scale"]] * s, tolerance = 1e-8)
    expect_equal(attr(f, "logLik") + length(y)*log(s), attr(base, "logLik"), tolerance = 1e-8)
  }
})

test_that("P2 held subsets agree with multistart reference likelihoods", {
  skip_unless_tier("full")
  set.seed(81029)
  id <- "gamma"
  pn <- c("shape","scale")
  held_default <- c(2.0,2.0)
  names(held_default) <- pn
  density <- stats::dgamma
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
expect_true(is.finite(logLik(mlgamma(2,shape=2))))
expect_true(is.finite(logLik(mlgamma(2,scale=2))))
})

test_that("P2 held solver respects iterlim", {
  expect_error(suppressWarnings(mlgamma(c(2,3),scale=2,iterlim=1)))
})

test_that("gamma: P3 unfinished optimizer stops", {
  g <- jsonlite::fromJSON(file.path(Sys.getenv("UMVL_DISTRIBUTIONS_DIR"), "gamma", "golden.json"))
  x <- g$data
  expect_error(mlgamma(x, iterlim=1, abstol=1e-10),
               "[Ff]ail|converge|unresolved|exhausted|score check")
})
