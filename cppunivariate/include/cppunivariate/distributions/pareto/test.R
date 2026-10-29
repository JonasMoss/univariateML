context("mlpareto")

## Data generation.
set.seed(313)
tiny_data <- extraDistr::rpareto(10, 1, 7)
small_data <- extraDistr::rpareto(100, 3, 9)
medium_data <- extraDistr::rpareto(1000, 1 / 2, 2)
large_data <- extraDistr::rpareto(10000, 13, 20)

## Checks logLiks.
expect_equal(
  sum(extraDistr::dpareto(tiny_data,
    mlpareto(tiny_data)[1],
    mlpareto(tiny_data)[2],
    log = TRUE
  )),
  attr(mlpareto(tiny_data), "logLik"),
  tolerance = 1e-5
)
expect_equal(
  sum(extraDistr::dpareto(small_data,
    mlpareto(small_data)[1],
    mlpareto(small_data)[2],
    log = TRUE
  )),
  attr(mlpareto(small_data), "logLik"),
  tolerance = 1e-5
)
expect_equal(
  sum(extraDistr::dpareto(medium_data,
    mlpareto(medium_data)[1],
    mlpareto(medium_data)[2],
    log = TRUE
  )),
  attr(mlpareto(medium_data), "logLik"),
  tolerance = 1e-5
)
expect_equal(
  sum(extraDistr::dpareto(large_data,
    mlpareto(large_data)[1],
    mlpareto(large_data)[2],
    log = TRUE
  )),
  attr(mlpareto(large_data), "logLik"),
  tolerance = 1e-5
)

## Finds errors with na and data out of bounds.
expect_error(mlpareto(c(tiny_data, -1)))
expect_error(mlpareto(c(tiny_data, NA)))

## Checks that na.rm works as intended.
expect_equal(
  coef(mlpareto(small_data)),
  coef(mlpareto(c(small_data, NA), na.rm = TRUE))
)

## Check class.
est <- mlpareto(small_data, na.rm = TRUE)
expect_equal(attr(est, "model"), "Pareto")
expect_equal(class(est), "univariateML")

## Check support.
expect_equal(class(attr(est, "support"))[[1]], "numeric")

test_that("Pareto handles concentration and a huge support ratio", {
  expect_error(mlpareto(rep(1,5)),"does not exist")
  for(x in list(1+(0:4)*.Machine$double.eps,c(1e-300,1,1e300))) {
    f<-mlpareto(x)
    expect_true(all(is.finite(f)))
    expect_true(is.finite(attr(f,"logLik")))
    expect_gt(f[["a"]],0)
  }
})

test_that("P2 held subsets agree with multistart reference likelihoods", {
  set.seed(81029)
  id <- "pareto"
  pn <- c("a","b")
  held_default <- c(1.0,2.0)
  names(held_default) <- pn
  density <- extraDistr::dpareto
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
    if (id=="pareto") hv["b"] <- min(x)*.8
    args <- c(list(x=x),as.list(hv[held]))
    f <- do.call(get(paste0("ml",id)),args)
    expect_identical(unname(as.numeric(f[held])),unname(hv[held]))
    expect_setequal(attr(f,"fixed"),held)
    p <- setNames(as.numeric(f),pn)
    expect_equal(as.numeric(logLik(f)),sum(ref(x,p)),tolerance=1e-8)
    free <- setdiff(pn,held)
    # The Pareto endpoint is attained at min(x); no unconstrained optimizer
    # can traverse its support boundary. Include that exact boundary.
    if (id=="pareto" && "b" %in% free) {hv["b"]<-min(x);free<-setdiff(free,"b")}
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
expect_error(mlpareto(c(1,2),b=2),"zero likelihood")
expect_error(mlpareto(rep(2,3),b=2),"constant data")
expect_equal(mlpareto(rep(2,3),a=2)[["b"]],2)
})
