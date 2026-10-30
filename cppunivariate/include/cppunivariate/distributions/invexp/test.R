test_that("invexp matches a multistart density oracle", {
  set.seed(871)
  x <- rinvexp(n = 300, scale = 1/2.7)
  fit <- mlinvexp(x)
  objective <- function(p) {
    scale <- exp(p[1])
    if (!is.finite(scale) || scale <= 0) return(1e100)
    -sum(dinvexp(x, scale=scale, log=TRUE))
  }
  oracles <- lapply(c(.4,1,3), function(s) optim(rep(log(s), 1), objective, method="BFGS", control=list(reltol=1e-12,maxit=2000)))
  oracle <- oracles[[which.min(vapply(oracles, function(o) o$value, 0.0))]]
  expect_equal(as.numeric(fit),exp(oracle$par),tolerance=1e-4)
  expect_equal(attr(fit,"logLik"),-oracle$value,tolerance=1e-8)
})
test_that("invexp scale is the reciprocal-exponential rate", {
 x<-c(.13,.7,2,11)
 expect_equal(dinvexp(x,scale=1/3,log=TRUE),dexp(1/x,rate=1/3,log=TRUE)-2*log(x))
 expect_equal(unname(coef(mlinvexp(x))),1/mean(1/x))
})

test_that("inverse exponential rejects unrepresentable reciprocal moments", {
  expect_error(mlinvexp(c(1,5e-324)),"precision")
})

test_that("P2 held subsets agree with multistart reference likelihoods", {
  set.seed(81029)
  id <- "invexp"
  pn <- c("scale")
  held_default <- c(1)
  names(held_default) <- pn
  density <- dinvexp
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
expect_true(is.finite(logLik(mlinvexp(2,scale=2))))
})
