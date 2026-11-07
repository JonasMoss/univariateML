trgamma_oracle <- function(x) {
  nll <- function(p) {
    v <- -sum(dtrgamma(x = x, shape1 = exp(p[1]), shape2 = exp(p[2]), scale = exp(p[3]), log = TRUE))
    if (is.finite(v)) v else 1e100
  }
  y <- log(x)
  s <- sqrt(mean((y - mean(y))^2))
  best <- list(value = Inf)
  for (a in c(.05, .3, 1, 3, 10, 50, 300)) for (m in c(.5, 1, 2)) {
    tau <- m * sqrt(trigamma(a)) / s
    start <- c(log(a), log(tau), mean(y) - digamma(a) / tau)
    if (nll(start) >= 1e90) next
    o <- optim(start, nll, method = "Nelder-Mead", control = list(maxit = 4000, reltol = 1e-12))
    o <- optim(o$par, nll, method = "BFGS", control = list(maxit = 2000, reltol = 1e-14))
    if (o$value < best$value) best <- o
  }
  ll <- -best$value
  faces <- c(lnorm = attr(mllnorm(x), "logLik"), power = attr(mlpower(x, epsilon = 0), "logLik"))
  face <- if (max(faces) >= ll - 1e-8 * length(x)) names(which.max(faces)) else "interior"
  list(ll = max(ll, faces), face = face, estimates = exp(best$par))
}

test_that("mltrgamma agrees with a multistart density oracle and its faces", {
  skip_unless_tier("full")
  cases <- list(
    list(n = 30, a = 2, t = 1.5, s = 2), list(n = 200, a = 2, t = 1.5, s = 2),
    list(n = 100, a = .4, t = 3, s = 1), list(n = 100, a = 1, t = .8, s = 5),
    # Near the lognormal limit: the fit is interior or escapes to mllnorm.
    list(n = 200, a = 60, t = .25, s = 1e-5), list(n = 400, a = 200, t = .15, s = 1e-12)
  )
  for (case in cases) for (seed in 1:3) {
    set.seed(seed)
    x <- rtrgamma(n = case$n, shape1 = case$a, shape2 = case$t, scale = case$s)
    oracle <- suppressWarnings(trgamma_oracle(x))
    fit <- tryCatch(mltrgamma(x,iterlim=1000), error = function(e) e)
    info <- paste(case$n, case$a, seed)
    if (oracle$face == "interior") {
      expect_false(inherits(fit, "error"), info = info)
      if (inherits(fit, "error")) next
      expect_gte(attr(fit, "logLik"), oracle$ll - 1e-7 * case$n)
      expect_equal(attr(fit, "logLik"), oracle$ll, tolerance = 1e-6, info = info)
      if (case$a < 50) expect_equal(as.numeric(fit), oracle$estimates, tolerance = 1e-3, info = info)
      expect_equal(sum(dtrgamma(x = x, shape1 = fit[1], shape2 = fit[2], scale = fit[3], log = TRUE)),
                   attr(fit, "logLik"), tolerance = 1e-10)
    } else {
      expect_error(mltrgamma(x,iterlim=1000), paste0("Use `ml", oracle$face, "`"), info = info)
    }
  }
})

test_that("mltrgamma faces are triggered by data from the limit models", {
  z <- qnorm(((1:400) - .5) / 400)
  expect_error(mltrgamma(exp(z + .05 * (z^2 - 1))), "Use `mllnorm`")
  expect_error(mltrgamma(3 * (((1:400) - .5) / 400)^(1 / 2.5)), "Use `mlpower`")
  expect_error(mltrgamma(c(1, 2, 3)), "Use `mlpower`")
  # Lognormal samples escape exactly when log(x) is right-skewed (here).
  for (seed in 1:6) {
    set.seed(seed)
    x <- rlnorm(300)
    k3 <- mean((log(x) - mean(log(x)))^3)
    if (k3 > 0) expect_error(mltrgamma(x), "Use `mllnorm`")
    else expect_gt(attr(mltrgamma(x), "logLik"), attr(mllnorm(x), "logLik"))
  }
  # Left-skewed log data very close to lognormal: interior, scale underflows.
  z <- qnorm(((1:4000) - .5) / 4000)
  x <- exp(z - .001 * (z^2 - 1))
  expect_error(mltrgamma(x), "not representable.*Use `mllnorm`")
})

test_that("mltrgamma fixed shapes reduce to gamma, Weibull and exact profiles", {
  set.seed(3)
  x <- rtrgamma(n = 150, shape1 = 2, shape2 = 1.5, scale = 2)
  n <- length(x)
  for (tau in c(.4, 1, 2.5)) {
    fit <- mltrgamma(x, shape2 = tau)
    g <- mlgamma(x^tau, abstol = 1e-12)
    expect_equal(unname(fit["shape2"]), tau)
    expect_equal(unname(fit["shape1"]), unname(g["shape"]), tolerance = 1e-8)
    expect_equal(unname(fit["scale"]), unname(g["scale"]^(1 / tau)), tolerance = 1e-8)
    expect_equal(attr(fit, "logLik"),
                 attr(g, "logLik") + n * log(tau) + (tau - 1) * sum(log(x)), tolerance = 1e-10)
    expect_identical(attr(fit, "fixed"), "shape2")
    expect_equal(attr(logLik(fit), "df"), 2L)
  }
  w <- mltrgamma(x, shape1 = 1)
  ref <- mlweibull(x, reltol = 1e-12)
  expect_equal(unname(w[2:3]), unname(ref[1:2]), tolerance = 1e-8)
  expect_equal(attr(w, "logLik"), attr(ref, "logLik"), tolerance = 1e-10)
  for (a in c(.2, 3, 30)) {
    fit <- mltrgamma(x, shape1 = a)
    nll <- function(p) {
      value <- suppressWarnings(-sum(dtrgamma(x = x, shape1 = a, shape2 = exp(p[1]), scale = exp(p[2]), log = TRUE)))
      if (is.finite(value)) value else 1e100
    }
    o <- optim(unname(c(log(fit[2]) + .3, log(fit[3]) - .3)), nll,
               method = "Nelder-Mead", control = list(reltol = 1e-14, maxit = 5000))
    o <- optim(o$par, nll, method = "BFGS", control = list(reltol = 1e-14, maxit = 2000))
    expect_equal(attr(fit, "logLik"), -o$value, tolerance = 1e-9)
    expect_equal(unname(fit[2:3]), unname(exp(o$par)), tolerance = 1e-5)
    both <- mltrgamma(x, shape1 = a, shape2 = 1.2)
    o <- optimize(function(s) -sum(dtrgamma(x = x, shape1 = a, shape2 = 1.2, scale = exp(s), log = TRUE)),
                  c(-20, 20), tol = 1e-12)
    expect_equal(attr(both, "logLik"), -o$objective, tolerance = 1e-10)
    expect_identical(attr(both, "fixed"), c("shape1", "shape2"))
    expect_equal(attr(logLik(both), "df"), 1L)
  }
  expect_identical(attr(mltrgamma(x), "fixed"), character())
  for (bad in c(0, -1, Inf, NaN)) {
    expect_error(mltrgamma(x, shape1 = bad), "positive finite")
    expect_error(mltrgamma(x, shape2 = bad), "positive finite")
  }
  expect_error(mltrgamma(c(2, 2, 2)), "two distinct")
})

test_that("mltrgamma and mlinvtrgamma are reciprocal fits", {
  set.seed(7)
  x <- rtrgamma(n = 250, shape1 = 0.8, shape2 = 2, scale = 3)
  a <- mltrgamma(x)
  b <- mlinvtrgamma(1 / x)
  expect_equal(unname(b[1:2]), unname(a[1:2]), tolerance = 1e-8)
  expect_equal(unname(b[3]), unname(1 / a[3]), tolerance = 1e-8)
  expect_equal(attr(b, "logLik"), attr(a, "logLik") + 2 * sum(log(x)), tolerance = 1e-12)
  expect_equal(dinvtrgamma(x = 1/x[1:5], shape1 = 2, shape2 = 1.5, scale = 1/3, log = TRUE),
               dtrgamma(x = x[1:5], shape1 = 2, shape2 = 1.5, scale = 3, log = TRUE) + 2 * log(x[1:5]))
})


test_that("trgamma log quantiles and large power transforms remain representable", {
  f <- qtrgamma
  # At shape2=1, compare directly to R's stable gamma quantile.
  expect_equal(f(p = -1000, shape1 = 2, shape2 = 1, scale = 1/1, log.p = TRUE),
               qgamma(-1000, 2, log.p = TRUE), tolerance = 1e-12)
  a <- 100; tau <- .0067; scale <- exp(-1 * log(a)/tau)
  u <- c(.01,.5,.99)
  expected <- exp(log(scale) + log(qgamma(u, a, lower.tail = TRUE))/tau)
  expect_equal(f(p = u, shape1 = a, shape2 = tau, scale = scale), expected, tolerance = 1e-12)

})


test_that("P2 trgamma holds agree with independent multistart reference likelihoods", {
  skip_unless_tier("full")
  pars <- c(shape1=3, shape2=1.5, scale=2)
  thresholds <- c()
  regular <- setdiff(names(pars), thresholds)
  density <- dtrgamma
  samples <- lapply(c(3, 20, 100), function(n)
    do.call(qtrgamma, c(list(p = (seq_len(n)-.5)/n), as.list(pars))))
  samples[[4]] <- rep(samples[[2]][seq(1, 20, 2)], each=2)
  # These samples deliberately include proper limit-model data.
  samples[[5]] <- qexp(((1:20)-.5)/20)
  samples[[6]] <- c(.3,.8,1,1,1)*pars["scale"]
  samples[[7]] <- c(1.1,1.4,2,3,5)*pars["scale"]
  for (x in samples) for (mask in seq_len(2^length(regular)-1)) {
    held <- c(thresholds, regular[as.logical(intToBits(mask)[seq_along(regular)])])
    free <- setdiff(names(pars), held)
    args <- as.list(pars[held])
    fit <- tryCatch(do.call(mltrgamma, c(list(x=x),args)), error=identity)
    always <- any(c("shape1","shape2") %in% held)
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

test_that("P2 trgamma validates new held values", {
  for (v in c(0,-1,Inf,NA_real_)) expect_error(mltrgamma(c(.2,.3,.5), scale=v),"positive|finite")
})

 test_that("P2 trgamma held scale conditional shapes on constant data", {
   for(args in list(list(scale=1,shape1=2),list(scale=1,shape2=3),list(scale=1,shape1=2,shape2=3))) {
     f <- do.call(mltrgamma,c(list(x=rep(2,5)),args))
     expect_identical(unname(as.numeric(f[names(args)])),unname(unlist(args)))
     expect_true(is.finite(attr(f,"logLik")))
   }
   expect_error(mltrgamma(rep(1,5),scale=1,shape1=2),"unbounded")
 })

test_that("trgamma: P3 unfinished optimizer stops", {
  g <- jsonlite::fromJSON(file.path(Sys.getenv("UMVL_DISTRIBUTIONS_DIR"), "trgamma", "golden.json"))
  x <- g$data
  expect_error(mltrgamma(x, iterlim=1, reltol=1e-10),
               "[Ff]ail|converge|unresolved|exhausted|score check")
})

test_that("trgamma: P3 held profile exhaustion stops", {
  expect_error(mltrgamma(c(.3,.8,1.7,4.2),scale=2,shape2=1,iterlim=1), "[Ff]ail|converge|unresolved|exhausted")
})

test_that("transformed gamma certificates preserve held endpoints and reciprocals", {
  x <- c(.1,.3,1)
  a <- mltrgamma(x, scale=1)
  b <- mlinvtrgamma(1/x, scale=1)
  expect_equal(as.numeric(a)[1:2],as.numeric(b)[1:2],tolerance=1e-9)
  expect_identical(as.numeric(a["scale"]),1)
  expect_identical(as.numeric(b["scale"]),1)
  expect_equal(as.numeric(logLik(b)),as.numeric(logLik(a))+2*sum(log(x)),tolerance=1e-10)
  expect_equal(as.numeric(logLik(a)),sum(dml(x,a,log=TRUE)),tolerance=1e-10)
  expect_equal(as.numeric(logLik(b)),sum(dml(1/x,b,log=TRUE)),tolerance=1e-10)
})

test_that("held-scale concentration and numerical controls retain different stops", {
  caught <- function(expr) tryCatch(expr,error=identity)
  expect_s3_class(caught(mltrgamma(c(2,2,2),scale=1)),"univariateML_nomle")
  expect_s3_class(caught(mlinvtrgamma(c(.5,.5,.5),scale=1)),"univariateML_nomle")
  expect_true(is.finite(as.numeric(logLik(mltrgamma(c(2,2,2),shape1=2,scale=1)))))
  expect_true(is.finite(as.numeric(logLik(mlinvtrgamma(c(.5,.5,.5),shape1=2,scale=1)))))
  e <- caught(mltrgamma(c(.1,.3,1),scale=1,iterlim=1))
  expect_s3_class(e,"univariateML_stop")
  expect_true(e$kind %in% c("numerical","budget"))
  expect_false(inherits(caught(mltrgamma(c(.1,.3,1),scale=1,reltol=0)),"univariateML_stop"))
})

test_that("held endpoint comparisons do not assert nonattainment", {
  x <- c(exp(-1),1-.Machine$double.eps/2,1)
  for (fit in list(function() mltrgamma(x,scale=1),function() mlinvtrgamma(1/x,scale=1))) {
    e <- tryCatch(fit(),error=identity)
    expect_s3_class(e,"univariateML_tie")
    expect_equal(nrow(e$candidates),2L)
    expect_true(e$loglik_upper>=e$loglik_lower)
    expect_true(e$loglik_upper-e$loglik_lower<1e-7)
  }
})

test_that("coefficient reconstruction loss is a numerical stop with a real witness", {
  z <- qnorm(((1:201)-.5)/201)
  x <- exp(1e-14*(z-.1*(z*z-1)))
  e <- tryCatch(mltrgamma(x),error=identity)
  expect_s3_class(e,"univariateML_numerical")
  expect_match(conditionMessage(e),"reconstructing")
  expect_gt(nrow(e$candidates),0)
  c <- e$candidates[1,]
  ll <- sum(dtrgamma(x,c$shape1,c$shape2,c$scale,log=TRUE))
  expect_gte(ll,c$loglik_lower-1e-8)
  expect_lte(ll,c$loglik_upper+1e-8)
})

test_that("partial logarithmic ties stop as numerical failures", {
  x <- 7-(0:15)*4*.Machine$double.eps
  for (fit in list(function() mltrgamma(x),function() mlinvtrgamma(x))) {
    e <- tryCatch(fit(),error=identity)
    expect_s3_class(e,"univariateML_numerical")
    expect_match(conditionMessage(e),"taking logarithms")
  }
  i <- which(diff(log(x))==0)[1]
  e <- tryCatch(mltrgamma(x[i+1],scale=x[i]),error=identity)
  expect_s3_class(e,"univariateML_numerical")
  expect_match(conditionMessage(e),"taking logarithms")
})

test_that("limit witnesses retain raw endpoints", {
  x <- 7*((1:400)/400)^(1/2.5)
  a <- tryCatch(mltrgamma(x),error=identity)
  b <- tryCatch(mlinvtrgamma(1/x),error=identity)
  expect_s3_class(a,"univariateML_limit")
  expect_s3_class(b,"univariateML_limit")
  expect_identical(unname(a$estimates["alpha"]),max(x))
  expect_identical(unname(b$estimates["b"]),min(1/x))
})

test_that("positive near-face improvements stop as unresolved comparisons", {
  z <- qnorm(((1:201)-.5)/201)
  x <- exp(.001*(z-1e-5*(z*z-1)))
  for (fit in list(function() mltrgamma(x),function() mlinvtrgamma(1/x))) {
    e <- tryCatch(fit(),error=identity)
    expect_s3_class(e,"univariateML_tie")
    expect_match(conditionMessage(e),"mllnorm")
    expect_gte(nrow(e$candidates),2)
    expect_gte(e$loglik_upper,e$loglik_lower)
    expect_lt(e$loglik_upper-e$loglik_lower,3e-6)
  }
})

test_that("unrepresentable scales carry numerical stop details", {
  z <- qnorm(((1:201)-.5)/201)
  x <- exp(z-.001*(z*z-1))
  for (fit in list(function() mltrgamma(x),function() mlinvtrgamma(1/x))) {
    e <- tryCatch(fit(),error=identity)
    expect_s3_class(e,"univariateML_numerical")
    expect_match(conditionMessage(e),"log\\(scale\\)")
    expect_identical(e$loglik_upper,Inf)
  }
})
