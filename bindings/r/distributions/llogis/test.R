context("mlllogis")

## Data generation.
set.seed(313)
small_data <- rllogis(n = 100, shape = 2, scale = 1/3)
tiny_data <- rllogis(n = 10, shape = 1, scale = 1)

## Finds errors with na and data out of bounds.
expect_error(mlllogis(c(tiny_data, NA)))
expect_error(mlllogis(c(tiny_data, -1)))

# Check correctness
obj_1 <- mllogis(log(tiny_data))
obj_2 <- mlllogis(tiny_data)

expect_equal(
  unname(obj_1[1]),
  unname(log(obj_2)[2])
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
  sum(dllogis(small_data, est[1], scale=est[2], log = TRUE)),
  attr(est, "logLik")
)

## Check class.
expect_equal(attr(est, "model"), "Log-logistic")
expect_equal(class(est), "univariateML")

# Check names.
expect_equal(names(est), c("shape", "scale"))

test_that("llogis handles a seeded sample with a large outlier", {
  set.seed(7)
  x <- c(rllogis(199, 2, 1), 1e3)
  objective <- function(p) -sum(dllogis(x, exp(p[1]), scale=exp(p[2]), log = TRUE))
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
  skip_unless_tier("full")
  set.seed(81029)
  id <- "llogis"
  pn <- c("shape","scale")
  held_default <- c(1.0,2.0)
  names(held_default) <- pn
  density <- dllogis
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
expect_error(mlllogis(rep(.5,3),scale=.5),"unbounded")
expect_true(is.finite(logLik(mlllogis(rep(.5,3),shape=2))))
})

test_that("P2 held solver respects iterlim", {
  expect_error(suppressWarnings(mlllogis(c(2,3),scale=1,iterlim=1)))
})

test_that("P2 failed held solver is distinguished from constant-data collapse", {
  expect_error(suppressWarnings(mlllogis(rep(2,3),scale=1,iterlim=1)),"failed to converge")
})

test_that("llogis: P3 unfinished optimizer stops", {
  g <- jsonlite::fromJSON(file.path(Sys.getenv("UMVL_DISTRIBUTIONS_DIR"), "llogis", "golden.json"))
  x <- g$data
  expect_error(mlllogis(x, iterlim=1, abstol=1e-10),
               "[Ff]ail|converge|unresolved|exhausted|score check")
})

test_that("llogis R6 collapse diagnostics and held exceptions", {
  for (v in c(.5,2)) {
    expect_error(mlllogis(rep(v,3)),"does not exist")
    expect_error(mlllogis(rep(v,3),scale=v),"unbounded")
    expect_true(is.finite(logLik(mlllogis(rep(v,3),shape=2))))
  }
})

# Independent 180/360-digit raw-model references: TASK-101.7.5 evidence.
test_that("raw transform audit case 4525", {
  x <- c(1.307038121314793, 1.3070381213161002, 1.3070381213174072, 1.307038121318714, 1.3070381213213282)
  fit <- mlllogis(x, abstol = 1e-10, iterlim = 200)
  expect_lte(abs(unname(fit["shape"]) / (977338636898.3333) - 1), 1e-10)
  expect_lte(abs(unname(fit["scale"]) / (1.3070381213175297) - 1), 1e-10)
  expect_lte(abs(as.numeric(logLik(fit)) - (126.83443367637379)) / max(1, abs(126.83443367637379)), 1e-10)
})
test_that("raw transform audit case 4528", {
  x <- c(1.307038121314793, 1.3070381213161002, 1.3070381213174072, 1.307038121318714, 1.3070381213213282)
  fit <- mlllogis(x, abstol = 1e-10, iterlim = 200)
  expect_lte(abs(unname(fit["shape"]) / (977338636898.3333) - 1), 1e-10)
  expect_lte(abs(unname(fit["scale"]) / (1.3070381213175297) - 1), 1e-10)
  expect_lte(abs(as.numeric(logLik(fit)) - (126.83443367637379)) / max(1, abs(126.83443367637379)), 1e-10)
})
test_that("raw transform audit case 4535", {
  x <- c(1.4612222342357195, 0.7038909590134532, 0.013073080872494258, 1.3070381213174072, 1307058.4295823444)
  fit <- mlllogis(x, abstol = 1e-10, iterlim = 200)
  expect_lte(abs(unname(fit["shape"]) / (0.3040901444416098) - 1), 1e-10)
  expect_lte(abs(unname(fit["scale"]) / (2.201982518014345) - 1), 1e-10)
  expect_lte(abs(as.numeric(logLik(fit)) - (-26.21124672480389)) / max(1, abs(-26.21124672480389)), 1e-10)
})
test_that("raw transform audit case 4536", {
  x <- c(1.3070355072411646, 1.3070368142792859, 1.3070381213174072, 1.3070394283555284, 1.30704073539365)
  fit <- mlllogis(x, abstol = 1e-10, iterlim = 200)
  expect_lte(abs(unname(fit["shape"]) / (1156676.740617789) - 1), 1e-10)
  expect_lte(abs(unname(fit["scale"]) / (1.3070381213165507) - 1), 1e-10)
  expect_lte(abs(as.numeric(logLik(fit)) - (58.66887513411426)) / max(1, abs(58.66887513411426)), 1e-10)
})
test_that("raw transform audit case 4533", {
  x <- c(1.4612222342357196e-150, 7.038909590134533e-151, 1.3073080872494259e-152, 1.3070381213174072e-150, 7.555950558444047e-153, 1.6454641076172512e-150, 3.634118342449831e-150, 5.611994994187286e-152, 3.3079750606038745e-150, 2.030826493716806e-149, 6.404111626327396e-151, 2.1198704112740274e-152)
  fit <- mlllogis(x, abstol = 1e-10, iterlim = 200)
  expect_lte(abs(unname(fit["shape"]) / (0.6905841610868956) - 1), 1e-10)
  expect_lte(abs(unname(fit["scale"]) / (5.212327592697621e-151) - 1), 1e-10)
  expect_lte(abs(as.numeric(logLik(fit)) - (4126.668189703903)) / max(1, abs(4126.668189703903)), 1e-10)
})
test_that("raw transform audit case 4518", {
  x <- c(1.4612222342357195, 0.7038909590134532, 0.013073080872494258, 1.3070381213174072, 0.007555950558444047, 1.6454641076172511, 3.634118342449831, 0.05611994994187286, 3.3079750606038747, 20.308264937168058, 0.6404111626327396, 0.021198704112740274)
  fit <- mlllogis(x, abstol = 1e-10, iterlim = 200)
  expect_lte(abs(unname(fit["shape"]) / (0.6905841610868956) - 1), 1e-10)
  expect_lte(abs(unname(fit["scale"]) / (0.5212327592697621) - 1), 1e-10)
  expect_lte(abs(as.numeric(logLik(fit)) - (-17.98497768537857)) / max(1, abs(-17.98497768537857)), 1e-10)
})
test_that("raw transform audit case 4534", {
  x <- c(1.4612222342357194e+150, 7.038909590134532e+149, 1.307308087249426e+148, 1.307038121317407e+150, 7.555950558444047e+147, 1.6454641076172512e+150, 3.634118342449831e+150, 5.611994994187286e+148, 3.307975060603874e+150, 2.0308264937168056e+151, 6.404111626327396e+149, 2.1198704112740275e+148)
  fit <- mlllogis(x, abstol = 1e-10, iterlim = 200)
  expect_lte(abs(unname(fit["shape"]) / (0.6905841610868956) - 1), 1e-10)
  expect_lte(abs(unname(fit["scale"]) / (5.212327592697621e+149) - 1), 1e-10)
  expect_lte(abs(as.numeric(logLik(fit)) - (-4162.638145074661)) / max(1, abs(-4162.638145074661)), 1e-10)
})
test_that("raw transform audit case 4524", {
  x <- c(1.3070380951766447, 1.307038108247026, 1.3070381213174072, 1.3070381343877884, 1.3070381605285508)
  fit <- mlllogis(x, abstol = 1e-10, iterlim = 200)
  expect_lte(abs(unname(fit["shape"]) / (97732768.82081644) - 1), 1e-10)
  expect_lte(abs(unname(fit["scale"]) / (1.3070381225421193) - 1), 1e-10)
  expect_lte(abs(as.numeric(logLik(fit)) - (80.78268785196853)) / max(1, abs(80.78268785196853)), 1e-10)
})
test_that("raw transform audit case 4527", {
  x <- c(1.3070380951766447, 1.307038108247026, 1.3070381213174072, 1.3070381343877884, 1.3070381605285508)
  fit <- mlllogis(x, abstol = 1e-10, iterlim = 200)
  expect_lte(abs(unname(fit["shape"]) / (97732768.82081644) - 1), 1e-10)
  expect_lte(abs(unname(fit["scale"]) / (1.3070381225421193) - 1), 1e-10)
  expect_lte(abs(as.numeric(logLik(fit)) - (80.78268785196853)) / max(1, abs(80.78268785196853)), 1e-10)
})
test_that("raw transform audit case 4522", {
  x <- c(1.4612222342357195, 1.4612222342357195, 1.4612222342357195, 1.4612222342357195, 0.7038909590134532, 0.013073080872494258)
  fit <- mlllogis(x, abstol = 1e-10, iterlim = 200)
  expect_lte(abs(unname(fit["shape"]) / (1.2532425554232094) - 1), 1e-10)
  expect_lte(abs(unname(fit["scale"]) / (0.9111157554269419) - 1), 1e-10)
  expect_lte(abs(as.numeric(logLik(fit)) - (-8.10597742931569)) / max(1, abs(-8.10597742931569)), 1e-10)
})
