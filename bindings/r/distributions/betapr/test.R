betapr_oracle <- function(x) {
  n <- length(x); lx <- log(x)
  # Keep dbetapr away from catastrophic large-shape cancellation. Exact
  # package limit fits below cover the excluded infinite-shape faces.
  f <- function(p) {
    if(any(!is.finite(p)) || any(p[1:2] > log(1e5)) || any(p < -700) || p[3] > 700) return(1e100)
    ll <- sum(dbetapr(x = x, shape1 = exp(p[1]), shape2 = exp(p[2]), scale = exp(p[3]), log = TRUE))
    if(is.finite(ll)) -ll else 1e100
  }
  starts <- expand.grid(a=c(.1,.5,2,10,100),b=c(.1,.5,2,10,100))
  fits <- lapply(seq_len(nrow(starts)), function(i) {
    a <- starts$a[i]; b <- starts$b[i]
    optim(c(log(a),log(b),mean(lx)-digamma(a)+digamma(b)), f,
      method='BFGS', control=list(maxit=3000,reltol=1e-13))
  })
  fit <- fits[[which.min(vapply(fits, function(z) z$value, 0.))]]
  faces <- c(gamma=attr(mlgamma(x),'logLik'), invgamma=attr(mlinvgamma(x),'logLik'))
  face <- names(which.max(faces))
  list(ll=-fit$value, par=exp(fit$par), faces=faces,
       model=if(max(faces) >= -fit$value-1e-8*n) face else 'betapr')
}
test_that("three-parameter fits and D1 comparisons agree with a slow oracle", {
  skip_unless_tier("full")
  samples <- list()
  for (seed in 1:12) {
    set.seed(seed)
    samples[[paste0("small", seed)]] <- rbetapr(n = 20, shape1 = 2, shape2 = 3, scale = 4)
  }
  for (seed in 1:3) {
    set.seed(seed)
    samples[[paste0("ordinary", seed)]] <- rbetapr(n = 400, shape1 = 0.7, shape2 = 1.2, scale = 3)
  }
  set.seed(1)
  samples$gamma <- rgamma(300, 2, rate=.5)
  set.seed(1)
  samples$invgamma <- rinvgamma(n = 300, alpha = 2, beta = 4)
  set.seed(1)
  samples$lnorm <- rlnorm(300, .7, .8)
  for (label in names(samples)) {
    x <- samples[[label]]
    o <- betapr_oracle(x)
    if (o$model == "betapr") {
      fit <- mlbetapr(x)
      expect_identical(names(fit), c("shape1", "shape2", "scale"))
      expect_equal(attr(fit, "logLik"), o$ll, tolerance=2e-7, info=label)
      expect_equal(as.numeric(fit), o$par, tolerance=2e-3, info=label)
      expect_equal(sum(dbetapr(x = x, shape1 = fit[1], shape2 = fit[2], scale = fit[3], log = TRUE)),
                   attr(fit, "logLik"), tolerance=1e-10)
    } else {
      expect_error(mlbetapr(x), paste0("Use `ml", o$model, "`"), info=label)
    }
    if (label == "gamma") expect_identical(o$model, "gamma")
    if (label == "invgamma") expect_identical(o$model, "invgamma")
  }
})

test_that("scale, controls and validation work", {
  set.seed(313)
  x <- rbetapr(n = 1000, shape1 = 1, shape2 = 2, scale = 3)
  fit <- mlbetapr(x)
  expect_s3_class(fit, "univariateML")
  expect_identical(attr(fit, "model"), "Beta prime")
  expect_equal(coef(mlbetapr(c(x, NA), na.rm=TRUE)), coef(fit))
  expect_error(mlbetapr(c(x, NA)))
  expect_error(mlbetapr(c(x, 0)))
  expect_error(mlbetapr(rep(1, 20)))
  expect_error(mlbetapr(x, scale0=-1))
  expect_equal(coef(mlbetapr(x, shape10=1, shape20=2, scale0=3,
                            gradtol=1e-6, reltol=1e-13, abstol=1e-10, iterlim=1000)),
               coef(fit), tolerance=1e-4)
  moved <- mlbetapr(x * 7)
  expect_equal(as.numeric(moved[1:2]), as.numeric(fit[1:2]), tolerance=1e-4)
  expect_equal(as.numeric(moved[3]), as.numeric(fit[3])*7, tolerance=1e-4)

})


test_that("fixed scale agrees with reference-density oracle and records df", {
  skip_unless_tier("full")
  for (s in c(.2, 1, 7)) {
    x <- c(.03, .1, .4, .7, 1, 2, 4, 12) * s
    fit <- mlbetapr(x, scale=s)
    objective <- function(p) -sum(dbetapr(x = x, shape1 = exp(p[1]), shape2 = exp(p[2]), scale = s, log = TRUE))
    oracle <- optim(c(0,0), objective, method="BFGS",
                    control=list(reltol=1e-13, maxit=3000))
    expect_equal(attr(fit, "logLik"), -oracle$value, tolerance=1e-9)
    expect_equal(as.numeric(fit[seq_len(2)]), exp(oracle$par), tolerance=2e-5)
    expect_identical(attr(fit, "fixed"), "scale")
    expect_equal(attr(logLik(fit), "df"), 2)
    expect_identical(as.numeric(fit["scale"]), s)
    expect_equal(AIC(fit), -2*attr(fit,"logLik")+2*2)
    for (bad in c(0, -1, Inf, NaN))
      expect_error(mlbetapr(x, scale=bad), "scale.*positive finite")
  }
})

test_that("fixed unit scale reproduces the historical golden", {
  root <- Sys.getenv("UMVL_DISTRIBUTIONS_DIR")
  skip_if(!nzchar(root), "Historical fixture needs the distribution source tree")
  g <- jsonlite::fromJSON(file.path(root, "betapr", "fixed-scale-golden.json"))
  fit <- mlbetapr(g$data, scale=1)
  expect_equal(as.numeric(fit[seq_along(g$estimates)]),
               as.numeric(unlist(g$estimates)), tolerance=1e-10)
  expect_equal(attr(fit,"logLik"), g$logLik, tolerance=1e-10)
})

test_that("fixing beta-prime scale removes the free-scale escapes", {
  set.seed(1)
  for (x in list(rgamma(300,2,rate=.5), rinvgamma(n = 300, alpha = 2, beta = 4))) {
    expect_s3_class(mlbetapr(x, scale=1), "univariateML")
  }
  expect_error(mlbetapr(rep(1,20),scale=1), "positive log-variance")
})

test_that("beta-prime new hold subsets agree with independent reference oracles", {
  skip_unless_tier("full")
  set.seed(8104)
  samples <- list(rbetapr(n = 20, shape1 = 0.7, shape2 = 1.2, scale = 1),rbetapr(n = 100, shape1 = 0.7, shape2 = 1.2, scale = 1),
    c(.05,.05,.3,2,15),c(.2,4),1,rep(2,4),rgamma(20,2),rinvgamma(n = 100, alpha = 3, beta = 1))
  subsets <- list(list(shape1=.7),list(shape2=1.2),list(shape1=.7,shape2=1.2),
    list(shape1=.7,scale=1.3),list(shape2=1.2,scale=1.3),list(shape1=.7,shape2=1.2,scale=1.3))
  for(x in samples)for(args in subsets) {
    free <- setdiff(c("shape1","shape2","scale"),names(args))
    objective <- function(p) {
      pars <- c(args,as.list(setNames(exp(p),free)))
      ll <- sum(do.call(dbetapr,c(list(x=x,log=TRUE),pars)))
      if(is.finite(ll)) -ll else 1e100
    }
    starts <- expand.grid(rep(list(log(c(.2,2,20))),length(free)))
    best <- if(!length(free)) objective(numeric()) else min(vapply(seq_len(nrow(starts)),function(i)
      nlminb(as.numeric(starts[i,]),objective,lower=rep(-16,length(free)),upper=rep(log(1e5),length(free)),
        control=list(iter.max=2000,eval.max=4000,rel.tol=1e-12))$objective,numeric(1)))
    face <- -Inf; model <- ""
    if(length(args)==1 && !is.null(args$shape1)) {
      face <- sum(dgamma(x,args$shape1,rate=args$shape1/mean(x),log=TRUE)); model <- "mlgamma"
    }
    if(length(args)==1 && !is.null(args$shape2)) {
      b <- args$shape2; rate <- b/mean(1/x)
      face <- sum(dgamma(1/x,b,rate=rate,log=TRUE)-2*log(x)); model <- "mlinvgamma"
    }
    if(face >= -best-1e-7*length(x)) {
      expect_error(do.call(mlbetapr,c(list(x),args)),model)
    } else {
      f <- do.call(mlbetapr,c(list(x),args))
      expect_setequal(attr(f,"fixed"),names(args))
      for(nm in names(args)) expect_identical(unname(f[nm]),args[[nm]])
      expect_equal(attr(f,"logLik"),-best,tolerance=2e-7)
      expect_equal(sum(dml(x,f,log=TRUE)),attr(f,"logLik"),tolerance=1e-10)
    }
  }
  for(nm in c("shape1","shape2"))for(bad in c(0,-1,Inf,NaN))
    expect_error(do.call(mlbetapr,c(list(x=1:3),setNames(list(bad),nm))),"positive finite")
  expect_error(mlbetapr(c(.1,1,5),shape1=.7,iterlim=1),"did not converge")
})

test_that("betapr: P3 unfinished optimizer stops", {
  g <- jsonlite::fromJSON(file.path(Sys.getenv("UMVL_DISTRIBUTIONS_DIR"), "betapr", "golden.json"))
  x <- g$data
  expect_error(mlbetapr(x, iterlim=1, gradtol=1e-10, scale=1),
               "[Ff]ail|converge|unresolved|exhausted|score check")
})

test_that("betapr: P3 finite failed default BFGS stops", {
  expect_error(mlbetapr(c(.01,.2,.3,.5,2,10,50),iterlim=1), "BfgsFailure")
})

test_that("non-unit held shapes expose certified faces and numerical stops", {
  g <- tryCatch(mlbetapr(rep(2,3),shape1=2),error=identity)
  expect_s3_class(g,"univariateML_limit")
  expect_identical(g$model,"gamma")
  expect_equal(unname(g$estimates),c(2,1))
  ig <- tryCatch(mlbetapr(rep(.5,3),shape2=2),error=identity)
  expect_s3_class(ig,"univariateML_limit")
  expect_identical(ig$model,"invgamma")
  expect_equal(unname(ig$estimates),c(2,1))
  e <- tryCatch(mlbetapr(1:4,shape1=1e100),error=identity)
  expect_s3_class(e,"univariateML_numerical")
  expect_true(nrow(e$candidates)>0)
  expect_true(is.infinite(e$loglik_upper))
  x <- c(.03,.1,.4,.7,1,2,4,12)
  f <- mlbetapr(x,shape1=2)
  inv <- mlbetapr(1/x,shape2=2)
  expect_equal(unname(inv[1]),unname(f[2]),tolerance=1e-6)
  expect_equal(unname(inv[3]),1/unname(f[3]),tolerance=1e-6)
  expect_equal(attr(inv,"logLik"),attr(f,"logLik")+2*sum(log(x)),tolerance=1e-10)
})

test_that("nonunique conditional beta-prime peaks retain both witnesses", {
  x <- c(1.9901008294052028e-5,2.209857117039297e-5,.0002450722434289249,
         12.288342947780086,942.976056905759,16.239512580216314,
         2.898690421966857,1.2684036923492177,400970.63293690106*exp(5.646438395028704))
  e <- tryCatch(mlbetapr(x,shape1=.22690393135372058),error=identity)
  expect_s3_class(e,"univariateML_tie")
  expect_true(any(e$candidates$shape2<.12,na.rm=TRUE))
  expect_true(any(e$candidates$shape2>.12,na.rm=TRUE))
  expect_true(is.finite(e$loglik_upper))
})

test_that("beta-prime certificate budget stops preserve witnesses", {
  set.seed(8104)
  invisible(rbetapr(n=20,shape1=.7,shape2=1.2,scale=1))
  invisible(rbetapr(n=100,shape1=.7,shape2=1.2,scale=1))
  invisible(rgamma(20,2))
  x <- rinvgamma(n=100,alpha=3,beta=1)
  e <- tryCatch(mlbetapr(x,shape1=.66110553857791232,iterlim=64),error=identity)
  expect_s3_class(e,"univariateML_budget")
  expect_true(nrow(e$candidates)>0)
  expect_match(conditionMessage(e),"raising iterlim")
})

test_that("all-free certificate stops carry witnesses and selected faces are proper", {
  g <- tryCatch(mlbetapr(1:4), error=identity)
  expect_s3_class(g,"univariateML_limit")
  expect_identical(g$model,"gamma")
  expect_true(is.finite(g$loglik_upper))
  e <- tryCatch(mlbetapr(c(.5,1,2)), error=identity)
  expect_s3_class(e,"univariateML_tie")
  expect_true(nrow(e$candidates)>=2)
  e <- tryCatch(mlbetapr(c(.03,.1,.4,.7,1,2,4,12),iterlim=1),error=identity)
  expect_s3_class(e,"univariateML_budget")
  expect_true(nrow(e$candidates)>0)
  expect_error(mlbetapr(rep(2,3)),class="univariateML_nomle")
})

test_that("betapr: TASK-101.7.3 raw likelihood regressions", {
  fixtures <- list(
    list(index=248, x=c(0.6510020100386483,0.6510020165486686,0.6510020230586888,0.651002029568709,0.6510020425887495),
         held=list(scale=1.0594104501122898),
         theta=c(shape1=5454373785714345.0,shape2=8876194498901787.0,scale=1.0594104501122898), logLik=84.44195054705918),
    list(index=249, x=c(0.6510020230573869,0.6510020230580378,0.6510020230586888,0.6510020230593399,0.6510020230606418),
         held=list(scale=1.0594104501122898),
         theta=c(shape1=5.454453294772166e+23,shape2=8.876323905999515e+23,scale=1.0594104501122898), logLik=130.4936888613748),
    list(index=251, x=c(0.99999998,0.99999999,1.0,1.00000001,1.00000003),
         held=list(scale=1.0594104501122898),
         theta=c(shape1=6567301472501320.0,shape2=6957467795090793.0,scale=1.0594104501122898), logLik=82.29573789639241),
    list(index=252, x=c(0.999999999998,0.999999999999,1.0,1.000000000001,1.000000000003),
         held=list(scale=1.0594104501122898),
         theta=c(shape1=6.56723725758789e+23,shape2=6.957399779053996e+23,scale=1.0594104501122898), logLik=128.34741532364148),
    list(index=254, x=c(1e-12,2e-12,3e-12,5e-12,9e-12),
         held=list(scale=1.0594104501122898),
         theta=c(shape1=2.0262551677149374,shape2=536658974818.8154,scale=1.0594104501122898), logLik=126.82230092559585),
    list(index=255, x=c(1e-17,2e-17,3e-17,5.0000000000000005e-17,9.000000000000001e-17),
         held=list(scale=1.0594104501122898),
         theta=c(shape1=2.0262551677082414,shape2=5.366589748160354e+16,scale=1.0594104501122898), logLik=184.38692825044689),
    list(index=256, x=c(1e-200,2e-200,3e-200,5e-200,9e-200),
         held=list(scale=1.0594104501122898),
         theta=c(shape1=2.0262551677082414,shape2=5.366589748160354e+199,scale=1.0594104501122898), logLik=2291.252288339999),
    list(index=257, x=c(1.8666916871781815e-151,2.7261096086292664e-151,1.484646969214721e-151,3.988768449995832e-151,4.39429737837367e-151,1.901375652366606e-150,8.50681333541539e-151,1.0314927998288996e-151,1.7856179604413502e-150,1.5002084830804323e-150,1.3375151204526194e-150,6.510020230586889e-151),
         held=list(scale=1.0594104501122898),
         theta=c(shape1=1.3768062808236863,shape2=1.8278993726688574e+150,scale=1.0594104501122898), logLik=4135.705570780781),
    list(index=260, x=c(0.6510007210546427,0.6510013720566658,0.6510020230586888,0.6510026740607118,0.651003325062735),
         held=list(scale=1.0594104501122898),
         theta=c(shape1=807247310521.9342,shape2=1313676772576.6814,scale=1.0594104501122898), logLik=62.39620481822838)
  )
  for (f in fixtures) {
    fit <- do.call(mlbetapr, c(list(x=f$x), f$held))
    expect_equal(as.numeric(fit[names(f$theta)])/pmax(abs(unname(f$theta)),1e-300),
                 unname(f$theta)/pmax(abs(unname(f$theta)),1e-300), tolerance=1e-10)
    expect_equal(attr(fit,"logLik")/f$logLik, 1, tolerance=1e-10)
  }
})

test_that("betapr: TASK-101.7.3 held-shape scale increment regression", {
  fit <- mlbetapr(c(1e-06,2e-06,3e-06,4.9999999999999996e-06,9e-06),shape1=2.014271442794272,scale=1.0594104501122898)
  expect_equal(unname(as.numeric(fit))/c(2.014271442794272,533486.0575303183,1.0594104501122898),rep(1,3),tolerance=1e-10)
  expect_equal(attr(fit,"logLik")/57.74469737827288,1,tolerance=1e-10)
})

test_that("betapr centered fit tolerates overflowing scale plus data", {
  x <- c(.99999998,.99999999,1,1.00000001,1.00000003)
  scale <- 1.0594104501122898
  a <- mlbetapr(x,scale=scale)
  b <- mlbetapr(x*2^1023,scale=scale*2^1023)
  expect_equal(as.numeric(b[1:2])/as.numeric(a[1:2]),rep(1,2),tolerance=1e-10)
  expect_equal(attr(b,"logLik"),attr(a,"logLik")-length(x)*1023*log(2),tolerance=1e-10)
})

test_that("betapr rejects a separated shape restoration loss", {
  e <- .Machine$double.eps
  expect_error(mlbetapr(c(1-2*e,1-e,1,1+e,1+3*e),scale=1),"BfgsFailure")
})

test_that("betapr held-shape score rejects an underflowed statistic", {
  expect_error(mlbetapr(c(1e-100,2e-100,3e-100,5e-100,9e-100),
                       shape1=1e-320,scale=1e300),"BfgsFailure")
})
