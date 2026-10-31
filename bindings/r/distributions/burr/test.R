burr_scale_oracle <- function(x) {
  nll <- function(t) {
    p <- exp(t)
    v <- -sum(dburr(x = x, shape1 = p[1], shape2 = p[2], scale = p[3], log = TRUE))
    if (is.finite(v)) v else 1e100
  }
  starts <- expand.grid(k=c(.01,.1,.5,1,5,100), c=c(.5,2,10))
  starts <- rbind(starts,expand.grid(k=c(1e-6,1e6),c=c(.001,2,1000)))
  candidates <- lapply(seq_len(nrow(starts)), function(i) {
    k <- starts$k[i]; c <- starts$c[i]
    initial <- c(log(k),log(c),mean(log(x))+(digamma(k)-digamma(1))/c)
    # Outside floating-point support this start has zero density everywhere.
    if (nll(initial) >= 1e90) return(list(value=1e100))
    z <- optim(initial, nll, method="Nelder-Mead",
               control=list(maxit=3000,reltol=1e-12))
    optim(z$par,nll,method="BFGS",control=list(maxit=2000,reltol=1e-13))
  })
  best <- candidates[[which.min(vapply(candidates, `[[`, 0., "value"))]]
  ll <- -best$value
  w <- mlweibull(x); p <- mlpareto(x)
  faces <- c(weibull=attr(w,"logLik"),pareto=attr(p,"logLik"))
  face <- if (max(faces) >= ll - 1e-8*length(x)) names(which.max(faces)) else "interior"
  list(ll=max(ll,faces), face=face, estimates=exp(best$par))
}

test_that("Burr scale fits agree with a full density oracle", {
  skip_unless_tier("full")
  for (n in c(20,200)) for (seed in 1:6) {
    set.seed(seed)
    x <- rburr(n = n, shape1 = 1, shape2 = 2, scale = 3)
    oracle <- suppressWarnings(burr_scale_oracle(x))
    fit <- tryCatch(mlburr(x),error=function(e)e)
    if (oracle$face == "interior") {
      expect_false(inherits(fit,"error"),info=paste(n,seed))
      if (!inherits(fit,"error")) {
        expect_named(fit,c("shape1","shape2","scale"))
        expect_equal(as.numeric(fit),oracle$estimates,tolerance=1e-3)
        expect_equal(attr(fit,"logLik"),oracle$ll,tolerance=1e-6)
        expect_equal(sum(dburr(x = x, shape1 = fit[1], shape2 = fit[2], scale = fit[3], log = TRUE)),
                     attr(fit,"logLik"),tolerance=1e-10)
      }
    } else expect_error(mlburr(x),paste0("Use `ml",oracle$face,"`"))
  }
})

test_that("Burr limits name the fitted model regardless of data units", {
  u <- ((1:200)-.5)/200
  for (scale in c(.1,10)) {
    expect_error(mlburr(scale*sqrt(-log(1-u))),"Use `mlweibull`")
    expect_error(mlburr(scale/sqrt(1-u)),"Use `mlpareto`")
  }
  expect_error(mlburr(c(1,2,3),scale0=-1),"positive")
})

test_that("burr accepts shape/scale starts and rejects invalid starts", {
  set.seed(1)
  x <- rburr(n = 200, shape1 = 1, shape2 = 2, scale = 3)
  a <- mlburr(x)
  b <- mlburr(x,shape10=.5,shape20=3,scale0=2,gradtol=1e-8,reltol=1e-12,iterlim=1000)
  expect_equal(attr(a,"logLik"),attr(b,"logLik"),tolerance=1e-8)
  expect_error(mlburr(x,shape10=-1),"positive")
  expect_error(mlburr(x,shape20=0),"positive")
})

test_that("Burr limits and small-shape corners match the derived bounds", {
  x <- c(.2,1,2,3,8)
  k <- 1e5
  expect_equal(pburr(q = x, shape1 = k, shape2 = 2, scale = 3 * sqrt(k)),
               pweibull(x,shape=2,scale=3),tolerance=1e-5)
  # Keep (x/scale)^shape2 representable in the CDF.
  c <- 400
  expect_equal(pburr(q = x, shape1 = 2/c, shape2 = c, scale = 2),
               ifelse(x<2,0,1-(2/x)^2),tolerance=.005)
  # f_logX <= shape2/e uniformly over shape1 and scale. In particular,
  # shape2 -> 0 cannot conceal another continuous boundary distribution.
  for (c in c(.001,.01,.1)) for (k in c(.0001,1,10000)) for (s in c(1e-10,1,1e10)) {
    ll <- sum(dburr(x = x, shape1 = k, shape2 = c, scale = s, log = TRUE))
    expect_lte(ll,length(x)*(log(c)-1)-sum(log(x))+1e-10)
  }
  # For fixed shape2 the maximum log-X density vanishes as shape1 -> 0.
  k <- 1e-8; c <- 2
  bound <- length(x)*(log(c)+(k+1)*log(k/(k+1)))-sum(log(x))
  expect_lte(sum(dburr(x = x, shape1 = k, shape2 = c, scale = 3, log = TRUE)),bound+1e-10)
})

test_that("burr fixed scale reproduces 6c89a8c and a reference oracle", {
  skip_unless_tier("full")
  root <- Sys.getenv("UMVL_DISTRIBUTIONS_DIR")
  skip_if(!nzchar(root), "Historical fixture needs the distribution source tree")
  golden <- jsonlite::fromJSON(file.path(root,"burr","fixed-scale-golden.json"))
  x <- golden$data
  fit <- mlburr(x,scale=1)
  expect_equal(unname(fit[1:2]),c(1.077036666067376,1.9857562895421235),tolerance=1e-10)
  expect_equal(attr(fit,"logLik"),-614.507368787803,tolerance=1e-10)
  for (s in c(.3,3)) {
    y <- s*x
    z <- mlburr(y,scale=s)
    expect_identical(unname(z["scale"]),s)
    expect_identical(attr(z,"fixed"),"scale")
    expect_equal(attr(logLik(z),"df"),2)
    expect_equal(unname(z[1:2]),unname(fit[1:2]),tolerance=1e-10)
    expect_equal(attr(z,"logLik"),attr(fit,"logLik")-length(x)*log(s),tolerance=1e-10)
    nll <- function(t) -sum(dburr(x = y, shape1 = exp(t[1]), shape2 = exp(t[2]), scale = s, log = TRUE))
    oracle <- optim(log(c(.7,1.5)),nll,method="BFGS",control=list(reltol=1e-13,maxit=2000))
    expect_equal(unname(z[1:2]),exp(oracle$par),tolerance=1e-5)
    expect_equal(attr(z,"logLik"),-oracle$value,tolerance=1e-9)
  }
  expect_identical(attr(mlburr(x),"fixed"),character())
  for (bad in c(0,-1,Inf,NA_real_,NaN)) expect_error(mlburr(x,scale=bad),"positive finite fixed scale")
  expect_error(mlburr(c(3,4,5),scale=3),"Use `mlpareto`")
})

test_that("burr fixed scale uses a profile fallback when moments have no root", {
  x <- c(.5,2,3,4,5)
  z <- mlburr(x,scale=1,abstol=1e-10)
  nll <- function(t) -sum(dburr(x = x, shape1 = exp(t[1]), shape2 = exp(t[2]), scale = 1, log = TRUE))
  oracle <- optim(log(c(.7,1.5)),nll,method="BFGS",control=list(reltol=1e-13,maxit=2000))
  expect_equal(as.numeric(z[1:2]),exp(oracle$par),tolerance=1e-5)
  expect_equal(attr(z,"logLik"),-oracle$value,tolerance=1e-9)
})

test_that("held-scale Burr endpoint penalty is not a Pareto likelihood equality", {
  x <- c(1,1.5,2,3)
  expect_error(mlburr(x,scale=1),"mlpareto")
  alpha <- length(x)/sum(log(x))
  pareto <- length(x)*log(alpha)-(alpha+1)*sum(log(x))
  # Profile shape1 exactly and approach shape2=infinity stably in log space.
  c <- 1e8; a <- log1p(exp(-c*log(x)))+c*log(x); k <- length(x)/sum(a)
  burr <- length(x)*(log(k)+log(c))-sum(log(x))-k*sum(a)-sum(a-c*log(x))
  expect_equal(burr,pareto-log(2),tolerance=1e-7)
  expect_equal(pareto,-3.800827,tolerance=1e-6)
  expect_error(mlburr(rep(1,4),scale=1),"unbounded")
})


test_that("P2 burr holds agree with independent multistart reference likelihoods", {
  skip_unless_tier("full")
  pars <- c(shape1=1, shape2=2, scale=1)
  thresholds <- c()
  regular <- setdiff(names(pars), thresholds)
  density <- dburr
  samples <- lapply(c(3, 20, 100), function(n)
    do.call(qburr, c(list(p = (seq_len(n)-.5)/n), as.list(pars))))
  samples[[4]] <- rep(samples[[2]][seq(1, 20, 2)], each=2)
  # These samples deliberately include proper limit-model data.
  samples[[5]] <- qexp(((1:20)-.5)/20)
  samples[[6]] <- c(.3,.8,1,1,1)*pars["scale"]
  samples[[7]] <- c(1.1,1.4,2,3,5)*pars["scale"]
  for (x in samples) for (mask in seq_len(2^length(regular)-1)) {
    held <- c(thresholds, regular[as.logical(intToBits(mask)[seq_along(regular)])])
    free <- setdiff(names(pars), held)
    args <- as.list(pars[held])
    fit <- tryCatch(do.call(mlburr, c(list(x=x),args)), error=identity)
    always <- "shape1" %in% held || all(c("shape2","scale") %in% held)
    if(always) expect_false(inherits(fit,"error"),info=paste(length(x),paste(held,collapse=",")))
    if (inherits(fit,"error")) {
      expect_match(conditionMessage(fit), "[Ll]imit|[Uu]se `ml|[Uu]nbounded|[Nn]o MLE|[Nn]o verified|unresolved|failed|does not exist")
      next
    }
    expect_identical(unname(as.numeric(fit[held])),unname(pars[held]))
    expect_true(all(held %in% attr(fit,"fixed")))
    objective <- function(z) {
      p <- pars; p[free] <- exp(z)
      value <- suppressWarnings(-sum(do.call(density,c(list(x=x,log=TRUE),as.list(p)))))
      if(is.finite(value)) value else 1e100
    }
    actual <- attr(fit,"logLik")
    expect_equal(actual, -objective(log(fit[free])), tolerance=1e-7)
    if(length(free)) {
      # Starts depend only on reference parameters and observations.
      starts <- lapply(c(-1,0,1), function(s) log(pars[free])+s)
      oracle <- vapply(starts,function(z) {
        o <- suppressWarnings(nlminb(z,objective,control=list(iter.max=1000,eval.max=3000,rel.tol=1e-12)))
        -o$objective
      },0.)
      expect_gte(actual, max(oracle)-1e-6*length(x))
      expect_lte(actual, max(oracle)+1e-5*length(x))
    }
  }
})

test_that("P2 burr validates new held values", {
  for (v in c(0,-1,Inf,NA_real_)) expect_error(mlburr(c(.2,.3,.5), shape1=v),"positive|finite")
  for (v in c(0,-1,Inf,NA_real_)) expect_error(mlburr(c(.2,.3,.5), shape2=v),"positive|finite")
})

 test_that("P2 burr constant samples respect the held tangent", {
   x <- rep(2,5)
   for(args in list(list(shape1=2,shape2=3),list(shape2=3,scale=1),list(shape1=2,scale=1),list(shape1=2,shape2=3,scale=1))) {
     f <- do.call(mlburr,c(list(x=x),args))
     expect_true(is.finite(attr(f,"logLik")))
     expect_identical(unname(as.numeric(f[names(args)])),unname(unlist(args)))
   }
   expect_error(mlburr(rep(1,5),shape1=2,scale=1),"unbounded")
   expect_error(mlburr(x,shape1=2),"unbounded")
 })

test_that("burr: P3 unfinished optimizer stops", {
  g <- jsonlite::fromJSON(file.path(Sys.getenv("UMVL_DISTRIBUTIONS_DIR"), "burr", "golden.json"))
  x <- g$data
  expect_error(mlburr(x, iterlim=1, gradtol=1e-10),
               "[Ff]ail|converge|unresolved|exhausted|score check")
})

test_that("burr: P3 held profile exhaustion stops", {
  expect_error(mlburr(c(.3,.8,1.7,4.2),shape1=2,scale=1,iterlim=1), "[Ff]ail|converge|unresolved|exhausted")
})
