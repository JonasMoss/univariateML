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
  skip_unless_tier("full")
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

# Independent 180/360-digit raw-model references: TASK-101.7.5 evidence.
test_that("raw transform audit case 3759", {
  x <- c(1.471602327564204, 1.4716023275656755, 1.4716023275671473, 1.471602327568619, 1.4716023275715622)
  fit <- mlinvgamma(x, abstol = 1e-10, iterlim = 200)
  expect_lte(abs(unname(fit["alpha"]) / (3.3782209017569924e+23) - 1), 1e-10)
  expect_lte(abs(unname(fit["beta"]) / (4.9713977420625715e+23) - 1), 1e-10)
  expect_lte(abs(as.numeric(logLik(fit)) - (126.41556407783901)) / max(1, abs(126.41556407783901)), 1e-10)
})
test_that("raw transform audit case 3762", {
  x <- c(1.471602327564204, 1.4716023275656755, 1.4716023275671473, 1.471602327568619, 1.4716023275715622)
  fit <- mlinvgamma(x, abstol = 1e-10, iterlim = 200)
  expect_lte(abs(unname(fit["alpha"]) / (3.3782209017569924e+23) - 1), 1e-10)
  expect_lte(abs(unname(fit["beta"]) / (4.9713977420625715e+23) - 1), 1e-10)
  expect_lte(abs(as.numeric(logLik(fit)) - (126.41556407783901)) / max(1, abs(126.41556407783901)), 1e-10)
})
test_that("raw transform audit case 10510", {
  x <- c(0.999999999998, 0.999999999999, 1.0, 1.000000000001, 1.000000000003)
  fit <- mlinvgamma(x, abstol = 1e-10, iterlim = 200)
  expect_lte(abs(unname(fit["alpha"]) / (3.3783453797065964e+23) - 1), 1e-10)
  expect_lte(abs(unname(fit["beta"]) / (3.378345379707272e+23) - 1), 1e-10)
  expect_lte(abs(as.numeric(logLik(fit)) - (128.34741532364208)) / max(1, abs(128.34741532364208)), 1e-10)
})
test_that("raw transform audit case 10509", {
  x <- c(0.99999998, 0.99999999, 1.0, 1.00000001, 1.00000003)
  fit <- mlinvgamma(x, abstol = 1e-10, iterlim = 200)
  expect_lte(abs(unname(fit["alpha"]) / (3378378418017940.0) - 1), 1e-10)
  expect_lte(abs(unname(fit["beta"]) / (3378378424774695.5) - 1), 1e-10)
  expect_lte(abs(as.numeric(logLik(fit)) - (82.29573790223182)) / max(1, abs(82.29573790223182)), 1e-10)
})
test_that("raw transform audit case 3758", {
  x <- c(1.4716022981351007, 1.471602312851124, 1.4716023275671473, 1.4716023422831706, 1.4716023717152171)
  fit <- mlinvgamma(x, abstol = 1e-10, iterlim = 200)
  expect_lte(abs(unname(fit["alpha"]) / (3378378416524510.5) - 1), 1e-10)
  expect_lte(abs(unname(fit["beta"]) / (4971629551103341.0) - 1), 1e-10)
  expect_lte(abs(as.numeric(logLik(fit)) - (80.36397877160313)) / max(1, abs(80.36397877160313)), 1e-10)
})
test_that("raw transform audit case 3761", {
  x <- c(1.4716022981351007, 1.471602312851124, 1.4716023275671473, 1.4716023422831706, 1.4716023717152171)
  fit <- mlinvgamma(x, abstol = 1e-10, iterlim = 200)
  expect_lte(abs(unname(fit["alpha"]) / (3378378416524510.5) - 1), 1e-10)
  expect_lte(abs(unname(fit["beta"]) / (4971629551103341.0) - 1), 1e-10)
  expect_lte(abs(as.numeric(logLik(fit)) - (80.36397877160313)) / max(1, abs(80.36397877160313)), 1e-10)
})

test_that("held-rate shape beyond binary64 stops", {
  expect_error(mlinvgamma(c(1e-300, 2e-300), beta = 1e300), "fitted coefficients")
})

test_that("finite public rate survives an overflowing reciprocal reference", {
  fit <- mlinvgamma(c(1e-309, 1, 1, 1, 1), abstol = 1e-10, iterlim = 200)
  expect_gt(fit[["beta"]], 0)
  expect_true(is.finite(as.numeric(logLik(fit))))
})
