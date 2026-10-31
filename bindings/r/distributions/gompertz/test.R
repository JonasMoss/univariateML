context("mlgompertz errors")

test_that("mlgompertz errors on overdispersed data", {
  expect_error(
    mlgompertz(c(0.1, 0.2, 5)),
    "Use `mlexp`"
  )
})

test_that("gompertz fitting respects extreme scale changes", {
  set.seed(1)
  x <- ml_simulate("mlgompertz", 50)
  f <- mlgompertz(x)
  for (s in c(1e-300,1e300)) {
    g <- mlgompertz(x*s)
    expect_equal(attr(g,"logLik")+length(x)*log(s),attr(f,"logLik"),tolerance=1e-6)
  }
})

test_that("Gompertz moment equality selects the exponential face", {
  expect_error(mlgompertz(c(1,1,4,12)), "Use `mlexp`")
})

test_that("constant positive Gompertz data have an unbounded likelihood", {
  expect_error(mlgompertz(rep(1,5)),"does not exist")
})

test_that("P2 held subsets agree with multistart reference likelihoods", {
  skip_unless_tier("full")
  set.seed(81029)
  id <- "gompertz"
  pn <- c("a","b")
  held_default <- c(1.0,1.0)
  names(held_default) <- pn
  density <- dgompertz
  ref <- function(x,p) {
    a <- p[["a"]]; b <- p[["b"]]; log(a)+b*x-a*expm1(b*x)/b
  }
  samples <- list(n20=ml_simulate(paste0("ml",id),20),
                  n100=ml_simulate(paste0("ml",id),100))
  samples$ties <- rep(samples$n20[1:5],each=4)
  samples$small <- samples$n20[1:2]
  samples$singleton <- samples$n20[1]
  samples$constant <- rep(samples$n20[1],5)
  samples$limit <- rexp(100)
  subsets <- lapply(1:(2^length(pn)-1),function(mask)
    pn[as.logical(intToBits(mask)[seq_along(pn)])])
  for (x in samples) for (held in subsets) {
    hv <- held_default
    args <- c(list(x=x),as.list(hv[held]))
    if (id=="gompertz" && identical(held,"a") &&
        sum(x)<=hv["a"]*sum(x*x)/2) {
      expect_error(do.call(mlgompertz,args),"mlexp")
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
expect_error(mlgompertz(c(1,2,4),a=1),"mlexp")
expect_error(mlgompertz(rep(2,3),a=1),"mlexp")
expect_error(mlgompertz(rep(0,3),b=1),"unbounded")
expect_equal(as.numeric(mlgompertz(rep(0,3),a=2)),c(2,1))
expect_true(is.finite(logLik(mlgompertz(rep(2,3),b=1))))
})

test_that("P2 held solver respects iterlim", {
  expect_error(suppressWarnings(mlgompertz(c(2,3),a=.1,iterlim=1)))
})

test_that("gompertz: P3 unfinished optimizer stops", {
  g <- jsonlite::fromJSON(file.path(Sys.getenv("UMVL_DISTRIBUTIONS_DIR"), "gompertz", "golden.json"))
  x <- g$data
  expect_error(mlgompertz(x, iterlim=1, reltol=1e-10),
               "[Ff]ail|converge|unresolved|exhausted|score check")
})
