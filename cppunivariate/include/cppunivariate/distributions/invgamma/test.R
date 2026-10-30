context("mlinvgamma")

## Data generation.
set.seed(313)
small_data <- rinvgamma(n = 100, alpha = 1, beta = 1)
tiny_data <- rinvgamma(n = 10, alpha = 3, beta = 7)
data3 <- c(0, tiny_data)

## Checks if the ML is correct.
mle1 <- nlm(function(p) {
  -mean(dinvgamma(x = small_data, alpha = p[1], beta = p[2], log = TRUE))
}, p = c(1, 1))$estimate

mle2 <- nlm(function(p) {
  -mean(dinvgamma(x = tiny_data, alpha = p[1], beta = p[2], log = TRUE))
}, p = c(1, 1))$estimate

expect_equal(mle1, as.numeric(mlinvgamma(small_data)), tolerance = 1e-4)
expect_equal(mle2, as.numeric(mlinvgamma(tiny_data)), tolerance = 1e-4)

## Checks warning with small iterlim.
## expect_warning(mlinvgamma(tiny_data, iterlim = 1))

## Finds errors with na and data out of bounds.
expect_error(mlinvgamma(c(tiny_data, NA)))
expect_error(mlinvgamma(c(tiny_data, 0)))

## Checks that na.rm works as intended.
expect_equal(
  coef(mlinvgamma(small_data)),
  coef(mlinvgamma(c(small_data, NA), na.rm = TRUE))
)

## Is the log-likelihood correct?
est <- mlinvgamma(small_data, na.rm = TRUE)
expect_equal(
  sum(dinvgamma(x = small_data, alpha = est[1], beta = est[2], log = TRUE)),
  attr(est, "logLik")
)

## Check class.
expect_equal(attr(est, "model"), "InvGamma")
expect_equal(class(est), "univariateML")

test_that("constant data have no attained invgamma MLE", {
  expect_error(mlinvgamma(rep(2, 5)), "does not exist")
  expect_error(mlinvgamma(2), "does not exist")
})

test_that("P2 held subsets agree with multistart reference likelihoods", {
  set.seed(81029)
  id <- "invgamma"
  pn <- c("alpha","beta")
  held_default <- c(3.0,4.0)
  names(held_default) <- pn
  density <- dinvgamma
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
expect_true(is.finite(logLik(mlinvgamma(2,alpha=2))))
expect_true(is.finite(logLik(mlinvgamma(2,beta=2))))
})

test_that("P2 held solver respects iterlim", {
  expect_error(suppressWarnings(mlinvgamma(c(2,3),beta=2,iterlim=1)))
})

test_that("invgamma: P3 unfinished optimizer stops", {
  g <- jsonlite::fromJSON(file.path(Sys.getenv("UMVL_DISTRIBUTIONS_DIR"), "invgamma", "golden.json"))
  x <- g$data
  expect_error(mlinvgamma(x, iterlim=1, abstol=1e-10),
               "[Ff]ail|converge|unresolved|exhausted|score check")
})
