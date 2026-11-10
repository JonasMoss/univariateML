context("mllgamma")

## Data generation.
set.seed(313)
small_data <- rlgamma(n = 100, shapelog = 2, ratelog = 3)
tiny_data <- rlgamma(n = 10, shapelog = 2, ratelog = 1)

## Finds errors with na and data out of bounds.
expect_error(mllgamma(c(tiny_data, NA)))
expect_error(mllgamma(c(tiny_data, 0)))
expect_error(mllgamma(c(tiny_data, -1)))

# Check correctness
expect_equal(
  unname(mlgamma(log(small_data))[1]),
  unname(mllgamma(small_data)[1])
)

## Checks that na.rm works as intended.
expect_equal(
  coef(mllgamma(small_data)),
  coef(mllgamma(c(small_data, NA), na.rm = TRUE))
)

## Is the log-likelihood correct?
est <- mllgamma(small_data, na.rm = TRUE)
expect_equal(
  sum(dlgamma(x = small_data, shapelog = est[1], ratelog = est[2], log = TRUE)),
  attr(est, "logLik")
)

## Check class.
expect_equal(attr(est, "model"), "Log-gamma")
expect_equal(class(est), "univariateML")

# Check names.
expect_equal(names(est), c("shapelog", "ratelog"))

## Check support.
expect_equal(class(attr(est, "support"))[[1]], "numeric")

test_that("constant data have no attained lgamma MLE", {
  expect_error(mllgamma(rep(2, 5)), "does not exist")
  expect_error(mllgamma(2), "does not exist")
})

test_that("P2 held subsets agree with multistart reference likelihoods", {
  skip_unless_tier("full")
  set.seed(81029)
  id <- "lgamma"
  pn <- c("shapelog","ratelog")
  held_default <- c(2.0,3.0)
  names(held_default) <- pn
  density <- dlgamma
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
expect_true(is.finite(logLik(mllgamma(2,shapelog=2))))
expect_true(is.finite(logLik(mllgamma(2,ratelog=2))))
})

test_that("P2 held solver respects iterlim", {
  expect_error(suppressWarnings(mllgamma(exp(c(1,2)),ratelog=2,iterlim=1)))
})

test_that("lgamma: P3 unfinished gamma delegate stops", {
  expect_error(mllgamma(c(1.1,2,4,10),abstol=1e-12,iterlim=1), "IterationFailure")
})

test_that("lgamma R6 collapse diagnostics and held exceptions", {
  expect_error(mllgamma(rep(2,3)),"does not exist")
  expect_true(is.finite(logLik(mllgamma(rep(2,3),shapelog=2))))
  expect_true(is.finite(logLik(mllgamma(rep(2,3),ratelog=2))))
})

# Independent 180/360-digit raw-model references: TASK-101.7.5 evidence.
test_that("raw transform audit case 4435", {
  x <- c(1.953632517309603, 1.9536325173115567, 1.9536325173135103, 1.9536325173154638, 1.9536325173193712)
  fit <- mllgamma(x, abstol = 1e-10, iterlim = 200)
  expect_lte(abs(unname(fit["shapelog"]) / (1.515167188134916e+23) - 1), 1e-10)
  expect_lte(abs(unname(fit["ratelog"]) / (2.2624888036389032e+23) - 1), 1e-10)
  expect_lte(abs(as.numeric(logLik(fit)) - (124.99901060523791)) / max(1, abs(124.99901060523791)), 1e-10)
})
test_that("raw transform audit case 4438", {
  x <- c(1.953632517309603, 1.9536325173115567, 1.9536325173135103, 1.9536325173154638, 1.9536325173193712)
  fit <- mllgamma(x, abstol = 1e-10, iterlim = 200)
  expect_lte(abs(unname(fit["shapelog"]) / (1.515167188134916e+23) - 1), 1e-10)
  expect_lte(abs(unname(fit["ratelog"]) / (2.2624888036389032e+23) - 1), 1e-10)
  expect_lte(abs(as.numeric(logLik(fit)) - (124.99901060523791)) / max(1, abs(124.99901060523791)), 1e-10)
})
test_that("raw transform audit case 4434", {
  x <- c(1.9536324782408598, 1.953632497777185, 1.9536325173135103, 1.9536325368498355, 1.9536325759224857)
  fit <- mllgamma(x, abstol = 1e-10, iterlim = 200)
  expect_lte(abs(unname(fit["shapelog"]) / (1515153157062830.0) - 1), 1e-10)
  expect_lte(abs(unname(fit["ratelog"]) / (2262467845304841.0) - 1), 1e-10)
  expect_lte(abs(as.numeric(logLik(fit)) - (78.94728556929056)) / max(1, abs(78.94728556929056)), 1e-10)
})
test_that("raw transform audit case 4437", {
  x <- c(1.9536324782408598, 1.953632497777185, 1.9536325173135103, 1.9536325368498355, 1.9536325759224857)
  fit <- mllgamma(x, abstol = 1e-10, iterlim = 200)
  expect_lte(abs(unname(fit["shapelog"]) / (1515153157062830.0) - 1), 1e-10)
  expect_lte(abs(unname(fit["ratelog"]) / (2262467845304841.0) - 1), 1e-10)
  expect_lte(abs(as.numeric(logLik(fit)) - (78.94728556929056)) / max(1, abs(78.94728556929056)), 1e-10)
})
test_that("raw transform audit case 10557", {
  x <- c(1.99999996, 1.99999998, 2.0, 2.00000002, 2.00000006)
  fit <- mllgamma(x, abstol = 1e-10, iterlim = 200)
  expect_lte(abs(unname(fit["shapelog"]) / (1623152109407427.5) - 1), 1e-10)
  expect_lte(abs(unname(fit["ratelog"]) / (2341713492093799.5) - 1), 1e-10)
  expect_lte(abs(as.numeric(logLik(fit)) - (78.83000201013296)) / max(1, abs(78.83000201013296)), 1e-10)
})
test_that("raw transform audit case 4442", {
  x <- c(5.262181501175243, 1.9536325173135103, 2.538689273183437, 1.1505091839279815, 1953637.7794950113)
  fit <- mllgamma(x, abstol = 1e-10, iterlim = 200)
  expect_lte(abs(unname(fit["shapelog"]) / (0.5551927127152927) - 1), 1e-10)
  expect_lte(abs(unname(fit["ratelog"]) / (0.15519193754309563) - 1), 1e-10)
  expect_lte(abs(as.numeric(logLik(fit)) - (-28.521381431876165)) / max(1, abs(-28.521381431876165)), 1e-10)
})
