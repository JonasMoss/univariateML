gb2_ll <- function(x, v) {
  t <- v[2] * (log(x) - log(v[4]))
  s <- function(t) pmax(t, 0) + log1p(exp(-abs(t)))
  sum(log(v[2])-log(x)-lbeta(v[3],v[1])-v[3]*s(-t)-v[1]*s(t))
}
gb2_sample <- function(n=400) actuar::qtrbeta(((1:n)-.5)/n,.3,2,.4)

test_that("GB2 agrees with independent multistart likelihood optimization", {
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
  expect_equal(sum(actuar::dtrbeta(x,fit[1],fit[2],fit[3],scale=fit[4],log=TRUE)),
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
  expect_error(mlgb2(x,iterlim=1),"search unresolved")
  expect_error(mlgb2(x,shape1=-1),"positive finite")
  expect_error(mlgb2(rep(1,5)),"unbounded")
  f <- mlgb2(1,shape1=.3,shape2=2,shape3=.4,scale=1)
  expect_equal(attr(f,"logLik"),gb2_ll(1,truth),tolerance=1e-12)
})

test_that("GB2 constrained members agree with their existing fitters", {
  u <- ((1:400)-.5)/400
  for(member in c("burr","invburr","betapr")) {
    x <- switch(member,burr=actuar::qburr(u,.3,2),
      invburr=actuar::qinvburr(u,.3,2),betapr=extraDistr::qbetapr(u,.3,.4))
    f <- switch(member,burr=mlgb2(x,shape3=1),invburr=mlgb2(x,shape1=1),
      betapr=mlgb2(x,shape2=1))
    old <- do.call(paste0("ml",member),list(x))
    expect_equal(attr(f,"logLik"),attr(old,"logLik"),tolerance=1e-7)
    expect_gte(attr(mlgb2(x),"logLik"),attr(old,"logLik")-1e-6)
  }
})

test_that("GB2 explicitly selects faces on deterministic face data", {
  u <- ((1:200)-.5)/200
  cases <- list(lnorm=qlnorm(u),trgamma=actuar::qtrgamma(u,.7,2),
    invtrgamma=actuar::qinvtrgamma(u,.7,2),pareto=(1-u)^(-1/2),power=u^(1/2),
    double=ifelse(u<.5,(2*u)^(1/2),(2*(1-u))^(-1/2)))
  for(id in names(cases)) {
    expect_error(mlgb2(cases[[id]]),if(id=="double") "double Pareto" else
      paste0("Use `ml",id,"`"),info=id)
  }
})
