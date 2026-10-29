context("mlpower")

## Data generation.
set.seed(313)
small_data <- extraDistr::rpower(100, 1, 1)
tiny_data <- extraDistr::rpower(10, 3, 7)
medium_data <- extraDistr::rpower(1000, 9, 11)

epsilon <- .Machine$double.eps^0.5

## Checks if the ML is correct.
mle1 <- suppressWarnings(nlm(function(p) {
  -sum(extraDistr::dpower(small_data, max(small_data) * (1 + epsilon), p, log = TRUE))
}, p = 1))

mle2 <- suppressWarnings(nlm(function(p) {
  -sum(extraDistr::dpower(tiny_data, max(tiny_data) * (1 + epsilon), p,
    log = TRUE
  ))
}, p = 7))

mle3 <- suppressWarnings(nlm(function(p) {
  -sum(extraDistr::dpower(medium_data, max(medium_data) * (1 + epsilon), p,
    log = TRUE
  ))
}, p = 11))

tolerance <- 1e-4

expect_equal(mle1$estimate, as.numeric(mlpower(small_data))[2],
  tolerance = tolerance
)
expect_equal(mle2$estimate, as.numeric(mlpower(tiny_data))[2],
  tolerance = tolerance
)
expect_equal(mle3$estimate, as.numeric(mlpower(medium_data))[2],
  tolerance = tolerance
)

expect_equal(-mle1$minimum, attr(mlpower(small_data), "logLik"),
  tolerance = tolerance
)
expect_equal(-mle2$minimum, attr(mlpower(tiny_data), "logLik"),
  tolerance = tolerance
)
expect_equal(-mle3$minimum, attr(mlpower(medium_data), "logLik"),
  tolerance = tolerance
)

## Finds errors with na and data out of bounds.
expect_error(mlpower(c(tiny_data, NA)))

## Checks that na.rm works as intended.
expect_equal(
  coef(mlpower(small_data)),
  coef(mlpower(c(small_data, NA), na.rm = TRUE))
)

est <- mlpower(tiny_data)

## Check class.
expect_equal(attr(est, "model"), "PowerDist")
expect_equal(class(est), "univariateML")

## Check support.
expect_equal(class(attr(est, "support"))[[1]], "numeric")

test_that("power endpoint offset is positive and survives rounding", {
  for(e in c(-1,Inf,NaN)) expect_error(mlpower(c(1,2),epsilon=e),"nonnegative and finite")
  f<-mlpower(c(1e100,2e100))
  expect_gt(f[["alpha"]],2e100)
})

test_that("P2 held subsets agree with multistart reference likelihoods", {
  set.seed(81029)
  id <- "power"
  pn <- c("alpha","beta")
  held_default <- c(1.0,2.0)
  names(held_default) <- pn
  density <- extraDistr::dpower
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
    if (id=="power") hv["alpha"] <- max(x)*1.2
    args <- c(list(x=x),as.list(hv[held]))
    if (id=="power" && !"alpha" %in% held) {
      expect_error(do.call(mlpower,args),"excludes the maximizing upper endpoint")
      next
    }
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
expect_error(mlpower(c(1,2),alpha=2),"zero likelihood")
expect_error(mlpower(c(1,2),alpha=1),"zero likelihood")
expect_error(mlpower(c(1,2),beta=2),"excludes")
expect_true(is.finite(logLik(mlpower(rep(2,3),alpha=3))))
})
