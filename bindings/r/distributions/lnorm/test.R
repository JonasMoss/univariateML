context("mllnorm")

## Data generation.
set.seed(313)
small_data <- stats::rlnorm(100, 2, 3)
tiny_data <- stats::rlnorm(10, 0, 1)

## Finds errors with na and data out of bounds.
expect_error(mllnorm(c(tiny_data, NA)))
expect_error(mllnorm(c(tiny_data, -1)))
expect_error(mllnorm(c(tiny_data, 0)))

## Checks that na.rm works as intended.
expect_equal(
  coef(mllnorm(small_data)),
  coef(mllnorm(c(small_data, NA), na.rm = TRUE))
)

## Is the log-likelihood correct?
est <- mllnorm(small_data, na.rm = TRUE)
expect_equal(
  sum(stats::dlnorm(small_data, est[1], est[2], log = TRUE)),
  attr(est, "logLik")
)

## Check class.
expect_equal(attr(est, "model"), "Lognormal")
expect_equal(class(est), "univariateML")

## Check support.
expect_equal(class(attr(est, "support"))[[1]], "numeric")

test_that("lnorm handles a seeded sample with a large outlier", {
  set.seed(7)
  x <- c(stats::rlnorm(199), 1e3)
  objective <- function(p) -sum(stats::dlnorm(x, p[1], exp(p[2]), log = TRUE))
  oracle <- optim(c(mean(log(x)), log(sd(log(x)))), objective,
    method = "Nelder-Mead", control = list(reltol = 1e-14, maxit = 10000))
  expected <- c(oracle$par[1], exp(oracle$par[2]))
  fit <- mllnorm(x)
  expect_true(all(is.finite(fit)))
  expect_true(all(abs(as.numeric(fit) - expected) <= 1e-4 * abs(expected)))
  expect_gte(attr(fit, "logLik"), -oracle$value - 1e-6)
})

test_that("constant data have no attained lnorm MLE", {
  expect_error(mllnorm(rep(2, 5)), "does not exist")
  expect_error(mllnorm(2), "does not exist")
})

test_that("P2 held subsets agree with multistart reference likelihoods", {
  skip_unless_tier("full")
  set.seed(81029)
  id <- "lnorm"
  pn <- c("meanlog","sdlog")
  held_default <- c(0.0,1.0)
  names(held_default) <- pn
  density <- stats::dlnorm
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
      if ("meanlog" %in% free) q["meanlog"] <- z[match("meanlog",free)]
      q
    }
    cost <- function(z) {
      if(any(!is.finite(z)) || any(abs(z)>40)) return(1e100)
      v <- suppressWarnings(-sum(ref(x,decode(z))))
      if(is.finite(v)) v else 1e100
    }
    if(length(free)) {
      start <- log(hv[free])
      if("meanlog" %in% free) start["meanlog"] <- mean(log(x))
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
expect_error(mllnorm(rep(1,3),meanlog=0),"unbounded")
expect_true(is.finite(logLik(mllnorm(rep(1,3),sdlog=2))))
})

test_that("P2 distant held lognormal mean retains a finite scale", {
  f <- mllnorm(c(1,2),meanlog=1e300)
  expect_identical(f[["meanlog"]],1e300)
  expect_equal(f[["sdlog"]],1e300)
  expect_true(is.finite(logLik(f)))
})

# Independent 180/360-digit raw-model references: TASK-101.7.5 evidence.
test_that("raw transform audit case 4601", {
  x <- c(1.445187510724387, 1.445187510725832, 1.4451875107272774, 1.4451875107287226, 1.445187510731613)
  fit <- mllnorm(x)
  expect_lte(abs(unname(fit["meanlog"]) / (0.3682390783550075) - 1), 1e-10)
  expect_lte(abs(unname(fit["sdlog"]) / (1.7204957773060616e-12) - 1), 1e-10)
  expect_lte(abs(as.numeric(logLik(fit)) - (126.50615506228813)) / max(1, abs(126.50615506228813)), 1e-10)
})
test_that("raw transform audit case 4604", {
  x <- c(1.445187510724387, 1.445187510725832, 1.4451875107272774, 1.4451875107287226, 1.445187510731613)
  fit <- mllnorm(x)
  expect_lte(abs(unname(fit["meanlog"]) / (0.3682390783550075) - 1), 1e-10)
  expect_lte(abs(unname(fit["sdlog"]) / (1.7204957773060616e-12) - 1), 1e-10)
  expect_lte(abs(as.numeric(logLik(fit)) - (126.50615506228813)) / max(1, abs(126.50615506228813)), 1e-10)
})
test_that("raw transform audit case 4611", {
  x <- c(100000001.46643509, 100000000.44635502, 100000001.90765181, 100000002.98407541, 100000001.44518751, 100000000.48315705, 100000001.20661016, 100000000.26392101, 100000004.95617464, 100000000.76818824, 100000001.8055238, 100000000.58251828)
  fit <- mllnorm(x)
  expect_lte(abs(unname(fit["meanlog"]) / (18.42068075921553) - 1), 1e-10)
  expect_lte(abs(unname(fit["sdlog"]) / (1.2760092241976638e-08) - 1), 1e-10)
  expect_lte(abs(as.numeric(logLik(fit)) - (-19.952111548312093)) / max(1, abs(-19.952111548312093)), 1e-10)
})
test_that("raw transform audit case 4600", {
  x <- c(1.445187481823527, 1.4451874962754023, 1.4451875107272774, 1.4451875251791524, 1.4451875540829027)
  fit <- mllnorm(x)
  expect_lte(abs(unname(fit["meanlog"]) / (0.36823908035480735) - 1), 1e-10)
  expect_lte(abs(unname(fit["sdlog"]) / (1.7204650438422402e-08) - 1), 1e-10)
  expect_lte(abs(as.numeric(logLik(fit)) - (80.45454250892595)) / max(1, abs(80.45454250892595)), 1e-10)
})
test_that("raw transform audit case 4603", {
  x <- c(1.445187481823527, 1.4451874962754023, 1.4451875107272774, 1.4451875251791524, 1.4451875540829027)
  fit <- mllnorm(x)
  expect_lte(abs(unname(fit["meanlog"]) / (0.36823908035480735) - 1), 1e-10)
  expect_lte(abs(unname(fit["sdlog"]) / (1.7204650438422402e-08) - 1), 1e-10)
  expect_lte(abs(as.numeric(logLik(fit)) - (80.45454250892595)) / max(1, abs(80.45454250892595)), 1e-10)
})

test_that("public log-mean restoration retains an uncertified finite fit", {
  for (unit in c(1e-150, 1e150)) {
    x <- unit * (1 + c(-2, -1, 0, 1, 3) * 1e-12)
    fit <- mllnorm(x)
    expect_false(attr(fit, "certified"))
    expect_true(is.finite(as.numeric(logLik(fit))))
  }
})

test_that("held rounded log mean retains the exact anchor residual", {
  fit <- mllnorm(rep(2, 3), meanlog = log(2))
  expect_lte(abs(unname(fit["sdlog"]) / 2.3190468138462995584e-17 - 1), 1e-13)
  expect_true(is.finite(as.numeric(logLik(fit))))
})
