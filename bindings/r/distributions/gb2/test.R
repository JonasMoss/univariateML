gb2_ll <- function(x, v) {
  t <- v[2] * (log(x) - log(v[4]))
  s <- function(t) pmax(t, 0) + log1p(exp(-abs(t)))
  sum(log(v[2])-log(x)-lbeta(v[3],v[1])-v[3]*s(-t)-v[1]*s(t))
}
gb2_sample <- function(n=400) qgb2(p = ((1:n) - 0.5)/n, shape1 = 0.3, shape2 = 2, shape3 = 0.4, scale = 1/1)

test_that("GB2 agrees with independent multistart likelihood optimization", {
  skip_unless_tier("full")
  x <- gb2_sample()
  fit <- mlgb2(x)
  nll <- function(z) {
    v <- exp(z)
    ans <- -gb2_ll(x,v)
    if(is.finite(ans)) ans else 1e100
  }
  for(p in c(.1,1,10)) for(q in c(.1,1,10)) {
    start <- c(q, sqrt(trigamma(p)+trigamma(q))/sd(log(x)), p, 1)
    o <- optim(log(start),nll,method="Nelder-Mead",control=list(maxit=3000,reltol=1e-12))
    expect_gte(attr(fit,"logLik"), -o$value-1e-6)
  }
  expect_equal(attr(fit,"logLik"),gb2_ll(x,fit),tolerance=1e-10)
  expect_equal(sum(dgb2(x = x, shape1 = fit[1], shape2 = fit[2], shape3 = fit[3], scale = fit[4], log = TRUE)),
               attr(fit,"logLik"),tolerance=1e-10)
})

test_that("GB2 reciprocal symmetry and scale equivariance", {
  x <- gb2_sample()
  f <- mlgb2(x); r <- mlgb2(1/x); s <- mlgb2(7*x)
  expect_equal(as.numeric(r),as.numeric(c(f[3],f[2],f[1],1/f[4])),tolerance=2e-4)
  expect_equal(as.numeric(s),as.numeric(c(f[1:3],7*f[4])),tolerance=2e-4)
  expect_equal(attr(r,"logLik"),attr(f,"logLik")+2*sum(log(x)),tolerance=1e-9)
  expect_equal(attr(s,"logLik"),attr(f,"logLik")-length(x)*log(7),tolerance=1e-9)
})

test_that("GB2 fixed subsets preserve metadata and constrained likelihood", {
  x <- gb2_sample(200)
  truth <- c(shape1=.3,shape2=2,shape3=.4,scale=1)
  subsets <- lapply(0:15, function(mask) names(truth)[as.logical(intToBits(mask)[1:4])])
  for(held in subsets) {
    f <- do.call(mlgb2,c(list(x),as.list(truth[held])))
    expect_equal(attr(f,"fixed"),names(truth)[names(truth)%in%held])
    expect_equal(attr(logLik(f),"df"),4L-length(held))
    expect_equal(unname(as.numeric(f[held])),unname(truth[held]),tolerance=1e-10)
    expect_equal(attr(f,"logLik"),gb2_ll(x,f),tolerance=1e-10)
    expect_gte(attr(f,"logLik"),gb2_ll(x,truth)-1e-7)
    free <- setdiff(names(truth),held)
    if(length(free)) {
      nll <- function(z) {
        v <- truth; v[free] <- exp(z)
        ans <- -gb2_ll(x,v)
        if(is.finite(ans)) ans else 1e100
      }
      o <- optim(log(truth[free]),nll,method=if(length(free)==1) "BFGS" else "Nelder-Mead",
        control=list(maxit=4000,reltol=1e-12))
      expect_equal(attr(f,"logLik"),-o$value,tolerance=1e-7)
    }
  }
  expect_error(mlgb2(x,iterlim=1),"certified delegate budget stop")
  expect_error(mlgb2(x,shape1=-1),"positive finite")
  expect_error(mlgb2(rep(1,5)),"unbounded")
  f <- mlgb2(1,shape1=.3,shape2=2,shape3=.4,scale=1)
  expect_equal(attr(f,"logLik"),gb2_ll(1,truth),tolerance=1e-12)
})

test_that("GB2 constrained members agree with their existing fitters", {
  u <- ((1:400)-.5)/400
  for(member in c("burr","invburr","betapr")) {
    x <- switch(member,burr=qburr(p = u, shape1 = 0.3, shape2 = 2, scale = 1/1),
      invburr=qinvburr(p = u, shape1 = 0.3, shape2 = 2, scale = 1/1),betapr=qbetapr(p = u, shape1 = 0.3, shape2 = 0.4, scale = 1))
    f <- switch(member,burr=mlgb2(x,shape3=1),invburr=mlgb2(x,shape1=1),
      betapr=mlgb2(x,shape2=1))
    old <- do.call(paste0("ml",member),list(x))
    expect_equal(attr(f,"logLik"),attr(old,"logLik"),tolerance=1e-7)
    expect_gte(attr(mlgb2(x),"logLik"),attr(old,"logLik")-1e-6)
  }
})

test_that("GB2 explicitly selects faces on deterministic face data", {
  skip_unless_tier("dev")
  u <- ((1:200)-.5)/200
  cases <- list(lnorm=qlnorm(u),trgamma=qtrgamma(p = u, shape1 = 0.7, shape2 = 2, scale = 1/1),
    invtrgamma=qinvtrgamma(p = u, shape1 = 0.7, shape2 = 2, scale = 1/1),pareto=(1-u)^(-1/2),power=u^(1/2),
    double=ifelse(u<.5,(2*u)^(1/2),(2*(1-u))^(-1/2)))
  for(id in names(cases)) {
    expect_error(mlgb2(cases[[id]]),if(id=="double") "double Pareto" else
      paste0("Use `ml",id,"`"),info=id)
  }
})


test_that("P2 gb2 holds agree with independent multistart reference likelihoods", {
  skip_unless_tier("full")
  pars <- c(shape1=0.3, shape2=2, shape3=0.4, scale=1)
  thresholds <- c()
  regular <- setdiff(names(pars), thresholds)
  density <- dgb2
  samples <- lapply(c(3, 20, 100), function(n)
    do.call(qgb2, c(list(p = (seq_len(n)-.5)/n), as.list(pars))))
  samples[[4]] <- rep(samples[[2]][seq(1, 20, 2)], each=2)
  # These samples deliberately include proper limit-model data.
  samples[[5]] <- qexp(((1:20)-.5)/20)
  samples[[6]] <- c(.3,.8,1,1,1)*pars["scale"]
  samples[[7]] <- c(1.1,1.4,2,3,5)*pars["scale"]
  for (x in samples) for (mask in seq_len(2^length(regular)-1)) {
    held <- c(thresholds, regular[as.logical(intToBits(mask)[seq_along(regular)])])
    free <- setdiff(names(pars), held)
    args <- as.list(pars[held])
    fit <- tryCatch(do.call(mlgb2, c(list(x=x),args)), error=identity)
    always <- all(c("shape2","scale") %in% held) || all(c("shape1","shape3") %in% held)
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

test_that("P2 gb2 constant samples with coercive holds fit", {
  expect_true(is.finite(attr(mlgb2(rep(2,5),shape1=2,shape2=3,scale=1),"logLik")))
  expect_true(is.finite(attr(mlgb2(rep(2,5),shape3=2,shape2=3,scale=1),"logLik")))
  expect_true(is.finite(attr(mlgb2(rep(2,5),shape1=2,shape3=3,scale=1),"logLik")))
})

test_that("gb2: P3 unfinished optimizer stops", {
  g <- jsonlite::fromJSON(file.path(Sys.getenv("UMVL_DISTRIBUTIONS_DIR"), "gb2", "golden.json"))
  x <- g$data
  expect_error(mlgb2(x, iterlim=1, gradtol=1e-10),
               "certified delegate budget stop")
})

test_that("gb2: P3 held profile exhaustion stops", {
  expect_error(mlgb2(rep(2,4),shape1=2,shape2=1,scale=1,iterlim=1), "[Ff]ail|converge|unresolved|exhausted")
})

test_that("GB2 held unit shape and scale inherit Burr scalar certificate", {
  x <- c(.1,.2,.4,.8)
  a <- mlgb2(x,shape3=1,scale=1,gradtol=1e-10)
  b <- mlburr(x,scale=1,abstol=1e-10)
  expect_equal(as.numeric(a[c(1,2,4)]),as.numeric(b),tolerance=1e-10)
  e <- mlgb2(x,shape3=1,scale=1,iterlim=1)
  expect_s3_class(e,"univariateML")
  expect_equal(as.numeric(e),as.numeric(a),tolerance=1e-6)
})
