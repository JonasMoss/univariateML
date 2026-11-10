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
x_start <- ml_simulate("mlinvweibull", 100)
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
  skip_unless_tier("full")
  set.seed(81029)
  id <- "invweibull"
  pn <- c("shape","scale")
  held_default <- c(2.0,3.0)
  names(held_default) <- pn
  density <- dinvweibull
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

# Independent 180/360-digit raw-model references: TASK-101.7.5 evidence.
test_that("raw transform audit case 4219", {
  x <- c(0.5071697345584799, 0.5071697345589871, 0.5071697345594942, 0.5071697345600014, 0.5071697345610158)
  fit <- mlinvweibull(x, reltol = 1e-10, iterlim = 200)
  expect_lte(abs(unname(fit["shape"]) / (707675622377.0812) - 1), 1e-10)
  expect_lte(abs(unname(fit["scale"]) / (0.5071697345591792) - 1), 1e-10)
  expect_lte(abs(as.numeric(logLik(fit)) - (131.91568101929354)) / max(1, abs(131.91568101929354)), 1e-10)
})
test_that("raw transform audit case 4283", {
  x <- c(1e-200, 2e-200, 3e-200, 5e-200, 9e-200)
  expect_error(mlinvweibull(x, shape = 1.928819454840455, scale = 0.3434414370956311, reltol = 1e-10, iterlim = 200))
})
test_that("raw transform audit case 4217", {
  x <- c(0.5070683006125823, 0.5071190175860383, 0.5071697345594942, 0.5072204515329501, 0.507321885479862)
  fit <- mlinvweibull(x, reltol = 1e-10, iterlim = 200)
  expect_lte(abs(unname(fit["shape"]) / (7076.606527224669) - 1), 1e-10)
  expect_lte(abs(unname(fit["scale"]) / (0.5071382319443192) - 1), 1e-10)
  expect_lte(abs(as.numeric(logLik(fit)) - (39.81219762261764)) / max(1, abs(39.81219762261764)), 1e-10)
})
test_that("raw transform audit case 4220", {
  x <- c(0.9998, 0.9999, 1.0, 1.0001, 1.0003)
  fit <- mlinvweibull(x, reltol = 1e-10, iterlim = 200)
  expect_lte(abs(unname(fit["shape"]) / (7076.606527223182) - 1), 1e-10)
  expect_lte(abs(unname(fit["scale"]) / (0.9999378854591896) - 1), 1e-10)
  expect_lte(abs(as.numeric(logLik(fit)) - (36.41764987636973)) / max(1, abs(36.41764987636973)), 1e-10)
})
test_that("raw transform audit case 4221", {
  x <- c(0.99999998, 0.99999999, 1.0, 1.00000001, 1.00000003)
  fit <- mlinvweibull(x, reltol = 1e-10, iterlim = 200)
  expect_lte(abs(unname(fit["shape"]) / (70766413.46901025) - 1), 1e-10)
  expect_lte(abs(unname(fit["scale"]) / (0.9999999937896172) - 1), 1e-10)
  expect_lte(abs(as.numeric(logLik(fit)) - (82.46937861550163)) / max(1, abs(82.46937861550163)), 1e-10)
})
test_that("raw transform audit case 4222", {
  x <- c(0.999999999998, 0.999999999999, 1.0, 1.000000000001, 1.000000000003)
  fit <- mlinvweibull(x, reltol = 1e-10, iterlim = 200)
  expect_lte(abs(unname(fit["shape"]) / (707663145134.7852) - 1), 1e-10)
  expect_lte(abs(unname(fit["scale"]) / (0.999999999999379) - 1), 1e-10)
  expect_lte(abs(as.numeric(logLik(fit)) - (128.52106495920626)) / max(1, abs(128.52106495920626)), 1e-10)
})
test_that("raw transform audit case 4218", {
  x <- c(0.5071697244160995, 0.5071697294877968, 0.5071697345594942, 0.5071697396311916, 0.5071697497745863)
  fit <- mlinvweibull(x, reltol = 1e-10, iterlim = 200)
  expect_lte(abs(unname(fit["shape"]) / (70766413.34268498) - 1), 1e-10)
  expect_lte(abs(unname(fit["scale"]) / (0.5071697314097761) - 1), 1e-10)
  expect_lte(abs(as.numeric(logLik(fit)) - (85.86392635209116)) / max(1, abs(85.86392635209116)), 1e-10)
})
test_that("raw transform audit case 9338", {
  x <- c(0.988503900932562, 0.9927057395157377, 0.9955081820301651, 0.9979146225997783, 1.0001935876214705, 1.0024856651664025, 1.004904185632642, 1.0075787228651987, 1.0107036114154169, 1.0146470464667317, 1.0203382467452637, 1.0320720648857378)
  fit <- mlinvweibull(x, reltol = 1e-10, iterlim = 200)
  expect_lte(abs(unname(fit["shape"]) / (105.80607046342593) - 1), 1e-10)
  expect_lte(abs(unname(fit["scale"]) / (1.00006845636793) - 1), 1e-10)
  expect_lte(abs(as.numeric(logLik(fit)) - (36.91980806615555)) / max(1, abs(36.91980806615555)), 1e-10)
})
test_that("raw transform audit case 9798", {
  x <- c(0.999998843731662, 0.9999992679008999, 0.9999995498064518, 0.9999997912445391, 1.0000000193568888, 1.0000002482581318, 1.0000004892200485, 1.0000007550151475, 1.0000010646738942, 1.000001454082512, 1.000002013420705, 1.0000031568544765)
  fit <- mlinvweibull(x, reltol = 1e-10, iterlim = 200)
  expect_lte(abs(unname(fit["shape"]) / (1058060.7046228813) - 1), 1e-10)
  expect_lte(abs(unname(fit["scale"]) / (1.0000000068454025) - 1), 1e-10)
  expect_lte(abs(as.numeric(logLik(fit)) - (147.51042142852074)) / max(1, abs(147.51042142852074)), 1e-10)
})
test_that("raw transform audit case 4228", {
  x <- c(2.6516987632880034e+149, 2.180658596102709e+149, 9.134933419025255e+149, 4.864424540397452e+149, 5.071697345594942e+149, 6.058163188737759e+149, 6.212659185566571e+149, 1.7898872530714507e+149, 2.8094570154848314e+149, 9.308703872748639e+149, 4.375259238937079e+149, 5.23122126168884e+149)
  fit <- mlinvweibull(x, reltol = 1e-10, iterlim = 200)
  expect_lte(abs(unname(fit["shape"]) / (2.0285581651990117) - 1), 1e-10)
  expect_lte(abs(unname(fit["scale"]) / (3.389075478414657e+149) - 1), 1e-10)
  expect_lte(abs(as.numeric(logLik(fit)) - (-4144.6620460087115)) / max(1, abs(-4144.6620460087115)), 1e-10)
})
test_that("raw transform audit case 4212", {
  x <- c(0.26516987632880035, 0.21806585961027092, 0.9134933419025255, 0.4864424540397452, 0.5071697345594942, 0.6058163188737758, 0.6212659185566571, 0.17898872530714507, 0.28094570154848314, 0.9308703872748638, 0.4375259238937079, 0.523122126168884)
  fit <- mlinvweibull(x, reltol = 1e-10, iterlim = 200)
  expect_lte(abs(unname(fit["shape"]) / (2.0285581651990117) - 1), 1e-10)
  expect_lte(abs(unname(fit["scale"]) / (0.3389075478414657) - 1), 1e-10)
  expect_lte(abs(as.numeric(logLik(fit)) - (-0.008878619429328452)) / max(1, abs(-0.008878619429328452)), 1e-10)
})
test_that("raw transform audit case 4227", {
  x <- c(2.6516987632880036e-151, 2.180658596102709e-151, 9.134933419025255e-151, 4.864424540397452e-151, 5.071697345594942e-151, 6.058163188737759e-151, 6.212659185566572e-151, 1.7898872530714506e-151, 2.8094570154848313e-151, 9.308703872748639e-151, 4.375259238937079e-151, 5.23122126168884e-151)
  fit <- mlinvweibull(x, reltol = 1e-10, iterlim = 200)
  expect_lte(abs(unname(fit["shape"]) / (2.0285581651990117) - 1), 1e-10)
  expect_lte(abs(unname(fit["scale"]) / (3.389075478414657e-151) - 1), 1e-10)
  expect_lte(abs(as.numeric(logLik(fit)) - (4144.644288769853)) / max(1, abs(4144.644288769853)), 1e-10)
})
test_that("raw transform audit case 4214", {
  x <- c(0.26516987632880035, 0.21806585961027092, 0.9134933419025255)
  fit <- mlinvweibull(x, reltol = 1e-10, iterlim = 200)
  expect_lte(abs(unname(fit["shape"]) / (2.292803066777193) - 1), 1e-10)
  expect_lte(abs(unname(fit["scale"]) / (0.2810958635274569) - 1), 1e-10)
  expect_lte(abs(as.numeric(logLik(fit)) - (0.44376226468664065)) / max(1, abs(0.44376226468664065)), 1e-10)
})
test_that("raw transform audit case 4226", {
  x <- c(1e-200, 2e-200, 3e-200, 5e-200, 9e-200)
  fit <- mlinvweibull(x, reltol = 1e-10, iterlim = 200)
  expect_lte(abs(unname(fit["shape"]) / (1.4424748841607793) - 1), 1e-10)
  expect_lte(abs(unname(fit["scale"]) / (2.1013325956265178e-200) - 1), 1e-10)
  expect_lte(abs(as.numeric(logLik(fit)) - (2291.098594735031)) / max(1, abs(2291.098594735031)), 1e-10)
})
test_that("raw transform audit case 4225", {
  x <- c(1e-17, 2e-17, 3e-17, 5.0000000000000005e-17, 9.000000000000001e-17)
  fit <- mlinvweibull(x, reltol = 1e-10, iterlim = 200)
  expect_lte(abs(unname(fit["shape"]) / (1.4424748841607793) - 1), 1e-10)
  expect_lte(abs(unname(fit["scale"]) / (2.101332595626518e-17) - 1), 1e-10)
  expect_lte(abs(as.numeric(logLik(fit)) - (184.233234645479)) / max(1, abs(184.233234645479)), 1e-10)
})
test_that("raw transform audit case 4223", {
  x <- c(1e-06, 2e-06, 3e-06, 4.9999999999999996e-06, 9e-06)
  fit <- mlinvweibull(x, reltol = 1e-10, iterlim = 200)
  expect_lte(abs(unname(fit["shape"]) / (1.4424748841607793) - 1), 1e-10)
  expect_lte(abs(unname(fit["scale"]) / (2.101332595626518e-06) - 1), 1e-10)
  expect_lte(abs(as.numeric(logLik(fit)) - (57.59105453080648)) / max(1, abs(57.59105453080648)), 1e-10)
})
test_that("raw transform audit case 4224", {
  x <- c(1e-12, 2e-12, 3e-12, 5e-12, 9e-12)
  fit <- mlinvweibull(x, reltol = 1e-10, iterlim = 200)
  expect_lte(abs(unname(fit["shape"]) / (1.4424748841607793) - 1), 1e-10)
  expect_lte(abs(unname(fit["scale"]) / (2.1013325956265178e-12) - 1), 1e-10)
  expect_lte(abs(as.numeric(logLik(fit)) - (126.66860732062784)) / max(1, abs(126.66860732062784)), 1e-10)
})

test_that("tiny held power retains the geometric-mean scale limit", {
  fit <- mlinvweibull(c(1, 4), shape = 1e-20)
  expect_equal(unname(fit["scale"]), 2, tolerance = 1e-14)
})

test_that("a finite scale survives an overflowing relative multiplier", {
  fit <- mlinvweibull(c(1e-300, 1e300, 1e300, 1e300), shape = 1e-20)
  expect_lte(abs(unname(fit["scale"]) / 1e150 - 1), 1e-12)
})

test_that("ulp-wide scale restoration is explicitly uncertified", {
  x <- 1 + (0:4) * .Machine$double.eps
  fit <- mlinvweibull(x)
  expect_false(attr(fit, "certified"))
  expect_true(is.finite(as.numeric(logLik(fit))))
})
