invburr_scale_oracle <- function(x) {
  nll <- function(t) {
    p <- exp(t)
    v <- -sum(actuar::dburr(x, shape1=p[1], shape2=p[2], scale=p[3], log=TRUE))
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

test_that("inverse Burr agrees with reciprocal actuar oracle", {
  for (n in c(20,200)) for (seed in 1:6) {
    set.seed(seed)
    y <- actuar::rburr(n,shape1=1,shape2=2,scale=3)
    x <- 1/y
    oracle <- suppressWarnings(invburr_scale_oracle(y))
    if (oracle$face == "interior") {
      fit <- mlinvburr(x)
      expect_named(fit,c("shape1","shape2","scale"))
      expect_equal(as.numeric(fit),c(oracle$estimates[1:2],1/oracle$estimates[3]),tolerance=1e-3)
      expect_equal(attr(fit,"logLik"),oracle$ll-2*sum(log(x)),tolerance=1e-6)
      expect_equal(sum(actuar::dinvburr(x,shape1=fit[1],shape2=fit[2],scale=fit[3],log=TRUE)),
                   attr(fit,"logLik"),tolerance=1e-10)
      forward <- mlburr(y)
      expect_equal(as.numeric(fit),unname(c(forward[1:2],1/forward[3])),tolerance=1e-5)
    } else {
      model <- if (oracle$face == "weibull") "invweibull" else "power"
      expect_error(mlinvburr(x),paste0("Use `ml",model,"`"))
    }
  }
})

test_that("inverse Burr limits name inverse Weibull and power", {
  u <- ((1:200)-.5)/200
  for (scale in c(.1,10)) {
    xw <- scale/sqrt(-log(1-u)); xp <- scale*sqrt(1-u)
    expect_error(mlinvburr(xw),"Use `mlinvweibull`")
    expect_error(mlinvburr(xp),"Use `mlpower`")
    # actuar inverse Weibull uses rate=1/scale; reciprocal Weibull scale=rate.
    expect_equal(attr(mlinvweibull(xw),"logLik"),
                 attr(mlweibull(1/xw),"logLik")-2*sum(log(xw)),tolerance=1e-8)
    # mlpower uses upper endpoint (max), reciprocal Pareto minimum.
    expect_equal(attr(mlpower(xp,epsilon=0),"logLik"),
                 attr(mlpareto(1/xp),"logLik")-2*sum(log(xp)),tolerance=1e-8)
  }
})

test_that("invburr accepts actuar-named starts and rejects invalid starts", {
  set.seed(1)
  x <- actuar::rinvburr(200,shape1=1,shape2=2,scale=3)
  a <- mlinvburr(x)
  b <- mlinvburr(x,shape10=.5,shape20=3,scale0=2,gradtol=1e-8,reltol=1e-12,iterlim=1000)
  expect_equal(attr(a,"logLik"),attr(b,"logLik"),tolerance=1e-8)
  expect_error(mlinvburr(x,shape10=-1),"positive")
  expect_error(mlinvburr(x,shape20=0),"positive")
})
