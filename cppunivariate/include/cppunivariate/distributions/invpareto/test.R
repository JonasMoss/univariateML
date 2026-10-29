test_that("invpareto matches a multistart actuar oracle", {
  set.seed(871)
  x <- actuar::rinvpareto(300, shape=1.3, scale=2.7)
  fit <- mlinvpareto(x, reltol=1e-10, iterlim=1000)
  objective <- function(p) -sum(actuar::dinvpareto(x, shape=exp(p[1]), scale=exp(p[2]), log=TRUE))
  oracles <- lapply(c(.4,1,3), function(s) optim(rep(log(s), 2), objective, method="BFGS", control=list(reltol=1e-12,maxit=2000)))
  oracle <- oracles[[which.min(vapply(oracles, function(o) o$value, 0.0))]]
  expect_equal(as.numeric(fit),exp(oracle$par),tolerance=1e-4)
  expect_equal(attr(fit,"logLik"),-oracle$value,tolerance=1e-8)
})
test_that("invpareto mapping and limit", {
 x<-c(.13,.7,2,11)
 expect_equal(actuar::dinvpareto(x,shape=2.3,scale=3,log=TRUE),extraDistr::dlomax(1/x,lambda=3,kappa=2.3,log=TRUE)-2*log(x))
 expect_error(mlinvpareto(rep(1,10)),"Use `mlinvexp`")
})

test_that("inverse Pareto inherits the Lomax equality comparison", {
  y <- c(1,1,40,60); x <- 1/y
  f <- mlinvpareto(x); g <- mllomax(y)
  expect_equal(attr(f,"logLik"),attr(g,"logLik")-2*sum(log(x)))
  expect_error(mlinvpareto(1/c(1,1,4,12)),"Use `mlinvexp`")
})

test_that("inverse Pareto inherits the global Lomax criterion", {
  for (z in list(c(1,100),c(.001,4,6),c(.001,1,2),c(1,29),c(1,1,1,12,12))) {
    x <- 1/z; f <- mlinvpareto(x,reltol=1e-12); l <- mllomax(z,reltol=1e-12)
    expect_equal(as.numeric(f),rev(as.numeric(l)),tolerance=1e-7)
    expect_equal(attr(f,"logLik"),attr(l,"logLik")-2*sum(log(x)),tolerance=1e-10)
    for(s in c(1e-200,1e200)) {
      g <- mlinvpareto(x*s,reltol=1e-12)
      expect_equal(as.numeric(g)/c(1,s),as.numeric(f),tolerance=1e-6)
    }
    g <- mlinvpareto(rep(x,50),reltol=1e-12)
    expect_equal(as.numeric(g),as.numeric(f),tolerance=1e-6)
  }
  for(z in list(c(1,10),c(1,28),rep(1,5))) expect_error(mlinvpareto(1/z),"mlinvexp")
})

test_that("P2 held subsets agree with multistart reference likelihoods", {
  set.seed(81029)
  id <- "invpareto"
  pn <- c("shape","scale")
  held_default <- c(2,1)
  names(held_default) <- pn
  density <- actuar::dinvpareto
  ref <- function(x,p) {
    suppressWarnings(do.call(density,c(list(x=x),as.list(p),list(log=TRUE))))
  }
  samples <- list(n20=simulate_default(paste0("ml",id),20),
                  n100=simulate_default(paste0("ml",id),100))
  samples$ties <- rep(samples$n20[1:5],each=4)
  samples$small <- samples$n20[1:2]
  samples$singleton <- samples$n20[1]
  samples$constant <- rep(samples$n20[1],5)
  samples$limit <- 1/rexp(100)
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
expect_error(mlinvpareto(rep(2,3)),"mlinvexp")
expect_true(is.finite(logLik(mlinvpareto(rep(2,3),shape=2))))
})

test_that("P2 held solver respects iterlim", {
  expect_error(suppressWarnings(mlinvpareto(c(2,3),shape=2,iterlim=1)))
})
