invburr_scale_oracle <- function(x) {
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

test_that("inverse Burr agrees with reciprocal density oracle", {
  skip_unless_tier("full")
  for (n in c(20,200)) for (seed in 1:6) {
    set.seed(seed)
    y <- rburr(n = n, shape1 = 1, shape2 = 2, scale = 3)
    x <- 1/y
    oracle <- suppressWarnings(invburr_scale_oracle(y))
    if (oracle$face == "interior") {
      fit <- mlinvburr(x)
      expect_named(fit,c("shape1","shape2","scale"))
      expect_equal(as.numeric(fit),c(oracle$estimates[1:2],1/oracle$estimates[3]),tolerance=1e-3)
      expect_equal(attr(fit,"logLik"),oracle$ll-2*sum(log(x)),tolerance=1e-6)
      expect_equal(sum(dinvburr(x = x, shape1 = fit[1], shape2 = fit[2], scale = fit[3], log = TRUE)),
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

test_that("invburr accepts shape/scale starts and rejects invalid starts", {
  set.seed(1)
  x <- rinvburr(n = 200, shape1 = 1, shape2 = 2, scale = 3)
  a <- mlinvburr(x)
  b <- mlinvburr(x,shape10=.5,shape20=3,scale0=2,gradtol=1e-8,reltol=1e-12,iterlim=1000)
  expect_equal(attr(a,"logLik"),attr(b,"logLik"),tolerance=1e-8)
  expect_error(mlinvburr(x,shape10=-1),"positive")
  expect_error(mlinvburr(x,shape20=0),"positive")
})

test_that("invburr fixed scale reproduces 6c89a8c and a reference oracle", {
  skip_unless_tier("full")
  root <- Sys.getenv("UMVL_DISTRIBUTIONS_DIR")
  skip_if(!nzchar(root), "Historical fixture needs the distribution source tree")
  golden <- jsonlite::fromJSON(file.path(root,"invburr","fixed-scale-golden.json"))
  x <- golden$data
  fit <- mlinvburr(x,scale=1)
  expect_equal(unname(fit[1:2]),c(1.028600546046916,2.1411632236775726),tolerance=1e-10)
  expect_equal(attr(fit,"logLik"),-626.1759641045593,tolerance=1e-10)
  for (s in c(.3,3)) {
    y <- s*x
    z <- mlinvburr(y,scale=s)
    expect_identical(unname(z["scale"]),s)
    expect_identical(attr(z,"fixed"),"scale")
    expect_equal(attr(logLik(z),"df"),2)
    expect_equal(unname(z[1:2]),unname(fit[1:2]),tolerance=1e-10)
    expect_equal(attr(z,"logLik"),attr(fit,"logLik")-length(x)*log(s),tolerance=1e-10)
    nll <- function(t) -sum(dinvburr(x = y, shape1 = exp(t[1]), shape2 = exp(t[2]), scale = s, log = TRUE))
    oracle <- optim(log(c(.7,1.5)),nll,method="BFGS",control=list(reltol=1e-13,maxit=2000))
    expect_equal(unname(z[1:2]),exp(oracle$par),tolerance=1e-5)
    expect_equal(attr(z,"logLik"),-oracle$value,tolerance=1e-9)
  }
  expect_identical(attr(mlinvburr(x),"fixed"),character())
  for (bad in c(0,-1,Inf,NA_real_,NaN)) expect_error(mlinvburr(x,scale=bad),"positive finite fixed scale")
  expect_error(mlinvburr(c(1,2,3),scale=3),"Use `mlpower`")
})

test_that("invburr fixed scale uses a profile fallback when moments have no root", {
  x <- 1/c(.5,2,3,4,5)
  z <- mlinvburr(x,scale=1,abstol=1e-10)
  nll <- function(t) -sum(dinvburr(x = x, shape1 = exp(t[1]), shape2 = exp(t[2]), scale = 1, log = TRUE))
  oracle <- optim(log(c(.7,1.5)),nll,method="BFGS",control=list(reltol=1e-13,maxit=2000))
  expect_equal(as.numeric(z[1:2]),exp(oracle$par),tolerance=1e-5)
  expect_equal(attr(z,"logLik"),-oracle$value,tolerance=1e-9)
})

test_that("held-scale inverse Burr endpoint penalty is not a power likelihood equality", {
  x <- 1/c(1,1.5,2,3)
  expect_error(mlinvburr(x,scale=1),"mlpower")
  alpha <- -length(x)/sum(log(x))
  power <- length(x)*log(alpha)+(alpha-1)*sum(log(x))
  c <- 1e8; a <- log1p(exp(c*log(x)))-c*log(x); k <- length(x)/sum(a)
  invburr <- length(x)*(log(k)+log(c))-sum(log(x))-k*sum(a)-sum(a+c*log(x))
  expect_equal(invburr,power-log(2),tolerance=1e-7)
  expect_error(mlinvburr(rep(1,4),scale=1),"unbounded")
})


test_that("P2 invburr holds agree with independent multistart reference likelihoods", {
  skip_unless_tier("full")
  pars <- c(shape1=1, shape2=2, scale=1)
  thresholds <- c()
  regular <- setdiff(names(pars), thresholds)
  density <- dinvburr
  samples <- lapply(c(3, 20, 100), function(n)
    do.call(qinvburr, c(list(p = (seq_len(n)-.5)/n), as.list(pars))))
  samples[[4]] <- rep(samples[[2]][seq(1, 20, 2)], each=2)
  # These samples deliberately include proper limit-model data.
  samples[[5]] <- qexp(((1:20)-.5)/20)
  samples[[6]] <- c(.3,.8,1,1,1)*pars["scale"]
  samples[[7]] <- c(1.1,1.4,2,3,5)*pars["scale"]
  for (x in samples) for (mask in seq_len(2^length(regular)-1)) {
    held <- c(thresholds, regular[as.logical(intToBits(mask)[seq_along(regular)])])
    free <- setdiff(names(pars), held)
    args <- as.list(pars[held])
    fit <- tryCatch(do.call(mlinvburr, c(list(x=x),args)), error=identity)
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

test_that("P2 invburr validates new held values", {
  for (v in c(0,-1,Inf,NA_real_)) expect_error(mlinvburr(c(.2,.3,.5), shape1=v),"positive|finite")
  for (v in c(0,-1,Inf,NA_real_)) expect_error(mlinvburr(c(.2,.3,.5), shape2=v),"positive|finite")
})

 test_that("P2 invburr constant samples respect the held tangent", {
   x <- rep(2,5)
   for(args in list(list(shape1=2,shape2=3),list(shape2=3,scale=1),list(shape1=2,scale=1),list(shape1=2,shape2=3,scale=1))) {
     f <- do.call(mlinvburr,c(list(x=x),args))
     expect_true(is.finite(attr(f,"logLik")))
     expect_identical(unname(as.numeric(f[names(args)])),unname(unlist(args)))
   }
   expect_error(mlinvburr(rep(1,5),shape1=2,scale=1),"unbounded")
   expect_error(mlinvburr(x,shape1=2),"unbounded")
 })

test_that("invburr: P3 unfinished optimizer stops", {
  g <- jsonlite::fromJSON(file.path(Sys.getenv("UMVL_DISTRIBUTIONS_DIR"), "invburr", "golden.json"))
  x <- g$data
  expect_error(mlinvburr(x, iterlim=1, gradtol=1e-10),
               "[Ff]ail|converge|unresolved|exhausted|score check")
})

test_that("invburr: P3 held profile exhaustion stops", {
  expect_error(mlinvburr(c(.3,.8,1.7,4.2),shape1=2,scale=1,iterlim=1), "[Ff]ail|converge|unresolved|exhausted")
})

test_that("invburr detects constant off-scale collapse only with both shapes free", {
  for (s in c(.3,1,3)) {
    x <- rep(2.0*s,3)
    expect_error(mlinvburr(x,scale=s), "Likelihood is unbounded.*constant sample; no MLE")
    for (args in list(list(shape1=2),list(shape2=3),list(shape1=2,shape2=3))) {
      fit <- do.call(mlinvburr,c(list(x=x,scale=s),args))
      expect_true(is.finite(attr(fit,"logLik")))
      expect_identical(unname(fit["scale"]),s)
      expect_equal(unname(as.numeric(fit[names(args)])),unname(unlist(args)))
    }
    # A nonconstant sample on the same side retains its finite fit.
    x <- s*c(1/.3,1/.5,1/.8)
    fit <- mlinvburr(x,scale=s)
    expect_true(is.finite(attr(fit,"logLik")))
  }
})

 test_that("invburr preserves certificate refusal diagnostics", {
  x <- exp(seq(-1e-6,1e-6,length.out=20))
  e <- tryCatch(mlinvburr(1/x),univariateML_numerical=identity)
  expect_s3_class(e,"univariateML_numerical")
  expect_true(is.data.frame(e$candidates))
  nom <- e$candidates[is.finite(e$candidates$shape1),,drop=FALSE]
  expect_gt(nrow(nom),0L)
  nom <- nom[which.max(nom$shape1),,drop=FALSE]
  expect_gt(nom$shape1,1e100)
  expect_true(all(is.finite(unlist(nom[c("shape1","shape2","scale")]))))
  expect_true(all(unlist(nom[c("shape1","shape2","scale")])>0))
  expect_true(is.finite(nom$loglik_lower)&&is.finite(nom$loglik_upper))
  expect_lte(nom$loglik_lower,nom$loglik_upper)
  face <- e$candidates[e$candidates$model=="invweibull",,drop=FALSE]
  if(nrow(face)) {
    expect_true(all(is.finite(unlist(face[c("shape","scale","loglik_lower","loglik_upper")]))))
    expect_true(all(face$shape>0&face$scale>0))
    expect_true(all(face$loglik_lower<=face$loglik_upper))
    expect_true(all(face$loglik_upper>=nom$loglik_lower))
    expect_true(all(e$loglik_lower>=face$loglik_lower))
  }
  x <- c(.2,.5,1,2,4)
  expect_error(mlinvburr(1/x,iterlim=2), "certificate numerical failure.*no verified MLE")
  x <- c(0.0049057434842350416,0.042501201178458653,0.19481544379842536,0.29869422522389177,0.40386521559557592,0.49096570363940406,0.5326256684997116,0.72206471601197653,0.74104603671600355,0.82688168528714312,0.98659907210500519,1.1056048590023038,1.140076235502067,1.1606952121643113,1.4300263464615877,1.4793399841671804,1.5655241685783525,1.6390163836698108,2.0838622791695847,5.1042846241626947)
  expect_error(mlinvburr(1/x), "certificate unresolved.*sign-uncertain seam; no verified MLE")
  expect_error(mlinvburr(rep(2,3)), "unbounded.*constant sample; no MLE")
})

test_that("inverse Burr refuses reciprocal overflow and artificial ties", {
  expect_error(mlinvburr(c(1,5e-324)), "certificate numerical failure; no verified MLE")
  x <- 1.5+(0:1024)*.Machine$double.eps
  i <- which(diff(1/x)==0)[1]
  expect_true(is.finite(i))
  expect_error(mlinvburr(c(x[i],x[i+1],3)), "certificate numerical failure; no verified MLE")
})

test_that("structured invburr endpoint limit uses public model names", {
  e <- tryCatch(mlinvburr(c(1,.5,.25),scale=1),univariateML_limit=identity)
  expect_identical(e$model,"power")
  expect_equal(e$estimates,c(alpha=1,beta=1/log(2)),tolerance=1e-12)
  expect_true(is.finite(e$limit_loglik))
})

test_that("invburr sign-uncertain-seam exposes structured tie diagnostics", {
  e <- tryCatch(mlinvburr(1/c(0.0049057434842350416,0.042501201178458653,0.19481544379842536,0.29869422522389177,0.40386521559557592,0.49096570363940406,0.5326256684997116,0.72206471601197653,0.74104603671600355,0.82688168528714312,0.98659907210500519,1.1056048590023038,1.140076235502067,1.1606952121643113,1.4300263464615877,1.4793399841671804,1.5655241685783525,1.6390163836698108,2.0838622791695847,5.1042846241626947)),univariateML_tie=identity)
  expect_s3_class(e,"univariateML_stop")
  expect_true(nrow(e$candidates)>=2L)
  expect_true(all(e$candidates$loglik_lower<=e$candidates$loglik_upper))
  expect_true(is.finite(e$loglik_lower))
})

test_that("invburr nearly-constant retains an uncertified finite search", {
  e <- tryCatch(mlinvburr(1/c(0.99999900000050002,0.99999910526355817,0.99999921052662744,0.9999993157897078,0.99999942105279915,0.99999952631590161,0.99999963157901528,0.99999973684213994,0.99999984210527559,0.99999994736842246,1.0000000526315804,1.0000001578947493,1.0000002631579294,1.0000003684211205,1.0000004736843227,1.0000005789475359,1.0000006842107605,1.000000789473996,1.0000008947372423,1.0000010000005)),univariateML_numerical=identity)
  expect_s3_class(e,"univariateML_numerical")
  expect_true(is.data.frame(e$candidates))
  nom <- e$candidates[is.finite(e$candidates$shape1),,drop=FALSE]
  expect_gt(nrow(nom),0L)
  nom <- nom[which.max(nom$shape1),,drop=FALSE]
  expect_gt(nom$shape1,1e100)
  expect_true(all(is.finite(unlist(nom[c("shape1","shape2","scale")]))))
  expect_true(all(unlist(nom[c("shape1","shape2","scale")])>0))
  expect_true(is.finite(nom$loglik_lower)&&is.finite(nom$loglik_upper))
  expect_lte(nom$loglik_lower,nom$loglik_upper)
  face <- e$candidates[e$candidates$model=="invweibull",,drop=FALSE]
  if(nrow(face)) {
    expect_true(all(is.finite(unlist(face[c("shape","scale","loglik_lower","loglik_upper")]))))
    expect_true(all(face$shape>0&face$scale>0))
    expect_true(all(face$loglik_lower<=face$loglik_upper))
    expect_true(all(face$loglik_upper>=nom$loglik_lower))
    expect_true(all(e$loglik_lower>=face$loglik_lower))
  }
})

test_that("invburr burr-k1000-r0 exposes structured limit diagnostics", {
  e <- tryCatch(mlinvburr(1/c(1.9964892694079062,1.4200005950190642,1.9743019980704946,1.2197287005958721,1.4225995479881626,1.3157966553088341,0.73993727475227145,0.54722770691864697,0.45191337211974208,1.2873097597929237,0.82121203961716593,0.71742472173341532,0.49662546185097139,0.50362133862028513,1.9039605277382454,0.44113984974587089,0.70433800798072665,1.0510023394724524,0.86043036041400101,1.6667673885263476,0.77702711842775052,1.9593669373814471,0.26458369013083416,0.45464268934386737,0.21491182426553512,1.7015469399366296,1.1819465625871135,0.8586733698248058,0.59167998109529263,0.44017579526839962,0.62624828047368308,0.98655801970217505,0.57139631035053795,0.51337745151115244,0.75748588358755875,0.43771943358567916,0.98605461903315861,1.3018311813985106,1.1866216948888411,0.42924863858380002,0.44140052283539877,1.196264737299584,1.07766835322611,1.4443112646323752,1.2516989419471247,1.0680479247006633,1.051768161411192,0.5465700675904327,1.6020198955375378,0.90951835946016879)),univariateML_limit=identity)
  expect_s3_class(e,"univariateML_stop")
  expect_true(nrow(e$candidates)>=2L)
  expect_true(all(e$candidates$loglik_lower<=e$candidates$loglik_upper))
  expect_identical(e$model,"invweibull")
  expect_named(e$estimates,c("shape","scale"))
  expect_true(all(is.finite(e$estimates)))
})

test_that("held-power inverse Burr limit carries the reciprocal face", {
  e <- tryCatch(mlinvburr(c(1/3,1/3,1/3),shape2=2),univariateML_limit=identity)
  expect_identical(e$model,"invweibull")
  expect_equal(e$estimates,c(shape=2,scale=1/3),tolerance=1e-12)
  expect_equal(e$limit_loglik,3*(log(2)+log(3)-1),tolerance=1e-12)
})

test_that("held-scale inverse Burr transports scalar certificates and budgets", {
  x <- c(.1,.2,.4,.8)
  a <- mlinvburr(1/x,scale=1,abstol=1e-10)
  b <- mlburr(x,scale=1,abstol=1e-10)
  expect_equal(as.numeric(a),as.numeric(b),tolerance=1e-10)
  e <- mlinvburr(1/x,scale=1,iterlim=1)
  expect_s3_class(e,"univariateML")
  expect_equal(as.numeric(e),as.numeric(a),tolerance=1e-6)
})

# Independent 180/360-digit raw-model references: TASK-101.7.5 evidence.
test_that("raw transform audit case 10483", {
  x <- c(0.999999999998, 0.999999999999, 1.0, 1.000000000001, 1.000000000003)
  fit <- mlinvburr(x, shape1 = 0.855256299891619, abstol = 1e-10, iterlim = 200)
  expect_lte(abs(unname(fit["shape1"]) / (0.855256299891619) - 1), 1e-10)
  expect_lte(abs(unname(fit["shape2"]) / (1019651840251.4617) - 1), 1e-10)
  expect_lte(abs(unname(fit["scale"]) / (1.0000000000003357) - 1), 1e-10)
  expect_lte(abs(as.numeric(logLik(fit)) - (128.11879065235854)) / max(1, abs(128.11879065235854)), 1e-10)
})
test_that("raw transform audit case 3588", {
  x <- c(1.121092244780946, 1.121092244782067, 1.1210922447831881, 1.1210922447843092, 1.1210922447865515)
  fit <- mlinvburr(x, shape1 = 0.855256299891619, abstol = 1e-10, iterlim = 200)
  expect_lte(abs(unname(fit["shape1"]) / (0.855256299891619) - 1), 1e-10)
  expect_lte(abs(unname(fit["shape2"]) / (1019650101014.3536) - 1), 1e-10)
  expect_lte(abs(unname(fit["scale"]) / (1.1210922447835645) - 1), 1e-10)
  expect_lte(abs(as.numeric(logLik(fit)) - (127.54725664194598)) / max(1, abs(127.54725664194598)), 1e-10)
})
test_that("raw transform audit case 3591", {
  x <- c(1.121092244780946, 1.121092244782067, 1.1210922447831881, 1.1210922447843092, 1.1210922447865515)
  fit <- mlinvburr(x, shape1 = 0.855256299891619, abstol = 1e-10, iterlim = 200)
  expect_lte(abs(unname(fit["shape1"]) / (0.855256299891619) - 1), 1e-10)
  expect_lte(abs(unname(fit["shape2"]) / (1019650101014.3536) - 1), 1e-10)
  expect_lte(abs(unname(fit["scale"]) / (1.1210922447835645) - 1), 1e-10)
  expect_lte(abs(as.numeric(logLik(fit)) - (127.54725664194598)) / max(1, abs(127.54725664194598)), 1e-10)
})
test_that("raw transform audit case 9432", {
  x <- c(4.846798306840522, 2.735667524057006, 2.067740730246208, 1.7014389169215507, 1.4569306990828055, 1.2751660686582076, 1.1299545222466922, 1.0072505256326596, 0.8981113115208634, 0.7953098508697711, 0.6898664186040794, 0.5564930042509462)
  fit <- mlinvburr(x, scale = 1.1483262751193506, abstol = 1e-10, iterlim = 200)
  expect_lte(abs(unname(fit["shape1"]) / (1.2800549077110803) - 1), 1e-10)
  expect_lte(abs(unname(fit["shape2"]) / (2.804335379571106) - 1), 1e-10)
  expect_lte(abs(unname(fit["scale"]) / (1.1483262751193506) - 1), 1e-10)
  expect_lte(abs(as.numeric(logLik(fit)) - (-13.928650640271442)) / max(1, abs(-13.928650640271442)), 1e-10)
})
test_that("raw transform audit case 9892", {
  x <- c(4.847313995660762, 2.7365809305442856, 2.0689488347295812, 1.7029066370018815, 1.4586441119184907, 1.2771228967709176, 1.1321617468502887, 1.0097251843067736, 0.9008846541282619, 0.7984385932211183, 0.6934679855358582, 0.5609433843485362)
  fit <- mlinvburr(x, scale = 1.1483262751193506, abstol = 1e-10, iterlim = 200)
  expect_lte(abs(unname(fit["shape1"]) / (1.2865485116424884) - 1), 1e-10)
  expect_lte(abs(unname(fit["shape2"]) / (2.812057492603485) - 1), 1e-10)
  expect_lte(abs(unname(fit["scale"]) / (1.1483262751193506) - 1), 1e-10)
  expect_lte(abs(as.numeric(logLik(fit)) - (-13.911508890528076)) / max(1, abs(-13.911508890528076)), 1e-10)
})
test_that("raw transform audit case 10482", {
  x <- c(0.99999998, 0.99999999, 1.0, 1.00000001, 1.00000003)
  fit <- mlinvburr(x, shape1 = 0.855256299891619, abstol = 1e-10, iterlim = 200)
  expect_lte(abs(unname(fit["shape1"]) / (0.855256299891619) - 1), 1e-10)
  expect_lte(abs(unname(fit["shape2"]) / (101966007.86014903) - 1), 1e-10)
  expect_lte(abs(unname(fit["scale"]) / (1.0000000033564398) - 1), 1e-10)
  expect_lte(abs(as.numeric(logLik(fit)) - (82.06712084330835)) / max(1, abs(82.06712084330835)), 1e-10)
})
test_that("raw transform audit case 3587", {
  x <- c(1.1210922223613433, 1.1210922335722657, 1.1210922447831881, 1.1210922559941106, 1.1210922784159554)
  fit <- mlinvburr(x, shape1 = 0.855256299891619, abstol = 1e-10, iterlim = 200)
  expect_lte(abs(unname(fit["shape1"]) / (0.855256299891619) - 1), 1e-10)
  expect_lte(abs(unname(fit["shape2"]) / (101966008.22632757) - 1), 1e-10)
  expect_lte(abs(unname(fit["scale"]) / (1.1210922485460666) - 1), 1e-10)
  expect_lte(abs(as.numeric(logLik(fit)) - (81.49560371872833)) / max(1, abs(81.49560371872833)), 1e-10)
})
test_that("raw transform audit case 3590", {
  x <- c(1.1210922223613433, 1.1210922335722657, 1.1210922447831881, 1.1210922559941106, 1.1210922784159554)
  fit <- mlinvburr(x, shape1 = 0.855256299891619, abstol = 1e-10, iterlim = 200)
  expect_lte(abs(unname(fit["shape1"]) / (0.855256299891619) - 1), 1e-10)
  expect_lte(abs(unname(fit["shape2"]) / (101966008.22632757) - 1), 1e-10)
  expect_lte(abs(unname(fit["scale"]) / (1.1210922485460666) - 1), 1e-10)
  expect_lte(abs(as.numeric(logLik(fit)) - (81.49560371872833)) / max(1, abs(81.49560371872833)), 1e-10)
})
test_that("raw transform audit case 3641", {
  x <- c(1.6311139990647936, 3.722402769857163, 0.49959572843217215, 0.6088765095016044, 2.419754129573806)
  fit <- mlinvburr(x, scale = 1.1483262751193506, abstol = 1e-10, iterlim = 200)
  expect_lte(abs(unname(fit["shape1"]) / (1.2575779921548758) - 1), 1e-10)
  expect_lte(abs(unname(fit["shape2"]) / (1.9084982695889698) - 1), 1e-10)
  expect_lte(abs(unname(fit["scale"]) / (1.1483262751193506) - 1), 1e-10)
  expect_lte(abs(as.numeric(logLik(fit)) - (-7.633207503410568)) / max(1, abs(-7.633207503410568)), 1e-10)
})
test_that("raw transform audit case 3640", {
  x <- c(1.6311139990647936, 3.722402769857163, 0.49959572843217215)
  fit <- mlinvburr(x, scale = 1.1483262751193506, abstol = 1e-10, iterlim = 200)
  expect_lte(abs(unname(fit["shape1"]) / (1.3462035118632834) - 1), 1e-10)
  expect_lte(abs(unname(fit["shape2"]) / (1.7734008129755383) - 1), 1e-10)
  expect_lte(abs(unname(fit["scale"]) / (1.1483262751193506) - 1), 1e-10)
  expect_lte(abs(as.numeric(logLik(fit)) - (-4.959691457402898)) / max(1, abs(-4.959691457402898)), 1e-10)
})

test_that("invburr transports the held-scale fallback flag", {
  f <- mlinvburr(exp(c(1.1,1)),scale=1)
  expect_s3_class(f,"univariateML")
  expect_identical(attr(f,"certified"),FALSE)
  expect_true(is.finite(as.numeric(logLik(f))))
})

test_that("invburr short certificate work preserves the joint search", {
  skip_if_not_installed("jsonlite")
  root <- Sys.getenv("UMVL_DISTRIBUTIONS_DIR")
  if(!nzchar(root)) root <- testthat::test_path("fixtures")
  g <- jsonlite::fromJSON(file.path(root,"invburr","golden.json"))
  f <- mlinvburr(g$data,gradtol=1e-8,iterlim=16)
  expect_identical(attr(f,"certified"),FALSE)
  expect_true(is.finite(as.numeric(logLik(f))))
})

test_that("invburr retains an unclosed held-power face as uncertified", {
  f <- mlinvburr(c(1,1,1,1,1/6.0001),shape2=1,gradtol=1e-14)
  expect_identical(attr(f,"certified"),FALSE)
  expect_true(is.finite(as.numeric(logLik(f))))
})
