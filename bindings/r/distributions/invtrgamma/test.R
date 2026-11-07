test_that("mlinvtrgamma agrees with a multistart density oracle", {
  skip_unless_tier("full")
  nll_for <- function(x) function(p) {
    v <- suppressWarnings(-sum(dinvtrgamma(x = x, shape1 = exp(p[1]), shape2 = exp(p[2]), scale = exp(p[3]), log = TRUE)))
    if (is.finite(v)) v else 1e100
  }
  for (generating_shape in c(2, 60)) for (seed in 1:4) {
    set.seed(seed)
    x <- rinvtrgamma(n = 200, shape1 = generating_shape, shape2 = if (generating_shape == 2) 1.5 else 0.25, scale = 0.5)
    nll <- nll_for(x)
    y <- -log(x)
    s <- sqrt(mean((y - mean(y))^2))
    best <- list(value = Inf)
    for (a in c(.3, 1, 3, 10, 60, 300)) for (m in c(.5, 1, 2)) {
      tau <- m * sqrt(trigamma(a)) / s
      start <- c(log(a), log(tau), -(mean(y) - digamma(a) / tau))
      o <- optim(start, nll, method = "Nelder-Mead", control = list(maxit = 4000, reltol = 1e-12))
      o <- optim(o$par, nll, method = "BFGS", control = list(maxit = 2000, reltol = 1e-14))
      if (o$value < best$value) best <- o
    }
    faces <- c(attr(mllnorm(x), "logLik"), attr(mlpareto(x), "logLik"))
    if (max(faces) >= -best$value - 1e-8 * length(x)) {
      expect_error(mlinvtrgamma(x,iterlim=1000), if (which.max(faces) == 1) "Use `mllnorm`" else "Use `mlpareto`")
      next
    }
    fit <- tryCatch(mlinvtrgamma(x,iterlim=1000), error = function(e) e)
    if (inherits(fit, "error")) {
      expect_match(conditionMessage(fit), "not representable")
      next
    }
    expect_equal(attr(fit, "logLik"), -best$value, tolerance = 1e-8)
    if (generating_shape == 2) expect_equal(as.numeric(fit), exp(best$par), tolerance = 1e-3)
    expect_equal(sum(dinvtrgamma(x = x, shape1 = fit[1], shape2 = fit[2], scale = fit[3], log = TRUE)),
                 attr(fit, "logLik"), tolerance = 1e-10)
    forward <- mltrgamma(1 / x,iterlim=1000)
    expect_equal(unname(fit[1:2]), unname(forward[1:2]), tolerance = 1e-8)
    expect_equal(unname(fit[3]), unname(1 / forward[3]), tolerance = 1e-8)
    expect_equal(attr(fit, "logLik"), attr(forward, "logLik") - 2 * sum(log(x)), tolerance = 1e-12)
  }
})

test_that("mlinvtrgamma mirrors the lognormal and Pareto faces", {
  p <- ((1:400) - .5) / 400
  z <- qnorm(p)
  expect_error(mlinvtrgamma(exp(-(z + .05 * (z^2 - 1)))), "Use `mllnorm`")
  expect_error(mlinvtrgamma(2 * (1 - p)^(-1 / 1.7)), "Use `mlpareto`")
  z <- qnorm(((1:4000) - .5) / 4000)
  expect_error(mlinvtrgamma(exp(-(z - .001 * (z^2 - 1)))), "not representable.*Use `mllnorm`")
})

test_that("mlinvtrgamma fixed shapes reduce to inverse gamma and inverse Weibull", {
  set.seed(11)
  x <- rinvtrgamma(n = 150, shape1 = 2, shape2 = 1.5, scale = 0.5)
  g <- mlinvtrgamma(x, shape2 = 1)
  ref <- mlinvgamma(x, abstol = 1e-12)
  expect_equal(unname(g[c(1, 3)]), unname(ref[1:2]), tolerance = 1e-8)
  expect_equal(attr(g, "logLik"), attr(ref, "logLik"), tolerance = 1e-10)
  expect_identical(attr(g, "fixed"), "shape2")
  w <- mlinvtrgamma(x, shape1 = 1)
  ref <- mlinvweibull(x, reltol = 1e-12)
  expect_equal(unname(w[2:3]), unname(ref[1:2]), tolerance = 1e-8)
  expect_equal(attr(w, "logLik"), attr(ref, "logLik"), tolerance = 1e-10)
  both <- mlinvtrgamma(x, shape1 = 2, shape2 = 1.5)
  o <- optimize(function(s) -sum(dinvtrgamma(x = x, shape1 = 2, shape2 = 1.5, scale = exp(s), log = TRUE)),
                c(-20, 20), tol = 1e-12)
  expect_equal(attr(both, "logLik"), -o$objective, tolerance = 1e-10)
  expect_equal(attr(logLik(both), "df"), 1L)
  expect_error(mlinvtrgamma(x, shape2 = -1), "positive finite")
})


test_that("invtrgamma log quantiles and large power transforms remain representable", {
  f <- qinvtrgamma
  # At shape2=1, compare directly to R's stable gamma quantile.
  expect_equal(f(p = -1000, shape1 = 2, shape2 = 1, scale = 1/1, log.p = TRUE),
               1/qgamma(-1000, 2, lower.tail = FALSE, log.p = TRUE), tolerance = 1e-12)
  a <- 100; tau <- .0065; scale <- exp(1 * log(a)/tau)
  u <- c(.01,.5,.99)
  expected <- exp(log(scale) - log(qgamma(u, a, lower.tail = FALSE))/tau)
  expect_equal(f(p = u, shape1 = a, shape2 = tau, scale = scale), expected, tolerance = 1e-12)

})


test_that("P2 invtrgamma holds agree with independent multistart reference likelihoods", {
  skip_unless_tier("full")
  pars <- c(shape1=3, shape2=1.5, scale=0.5)
  thresholds <- c()
  regular <- setdiff(names(pars), thresholds)
  density <- dinvtrgamma
  samples <- lapply(c(3, 20, 100), function(n)
    do.call(qinvtrgamma, c(list(p = (seq_len(n)-.5)/n), as.list(pars))))
  samples[[4]] <- rep(samples[[2]][seq(1, 20, 2)], each=2)
  # These samples deliberately include proper limit-model data.
  samples[[5]] <- qexp(((1:20)-.5)/20)
  samples[[6]] <- c(.3,.8,1,1,1)*pars["scale"]
  samples[[7]] <- c(1.1,1.4,2,3,5)*pars["scale"]
  for (x in samples) for (mask in seq_len(2^length(regular)-1)) {
    held <- c(thresholds, regular[as.logical(intToBits(mask)[seq_along(regular)])])
    free <- setdiff(names(pars), held)
    args <- as.list(pars[held])
    fit <- tryCatch(do.call(mlinvtrgamma, c(list(x=x),args)), error=identity)
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

test_that("P2 invtrgamma validates new held values", {
  for (v in c(0,-1,Inf,NA_real_)) expect_error(mlinvtrgamma(c(.2,.3,.5), scale=v),"positive|finite")
})

 test_that("P2 invtrgamma held scale conditional shapes on constant data", {
   for(args in list(list(scale=1,shape1=2),list(scale=1,shape2=3),list(scale=1,shape1=2,shape2=3))) {
     f <- do.call(mlinvtrgamma,c(list(x=rep(2,5)),args))
     expect_identical(unname(as.numeric(f[names(args)])),unname(unlist(args)))
     expect_true(is.finite(attr(f,"logLik")))
   }
   expect_error(mlinvtrgamma(rep(1,5),scale=1,shape1=2),"unbounded")
 })

test_that("invtrgamma: P3 unfinished optimizer stops", {
  g <- jsonlite::fromJSON(file.path(Sys.getenv("UMVL_DISTRIBUTIONS_DIR"), "invtrgamma", "golden.json"))
  x <- g$data
  expect_error(mlinvtrgamma(x, iterlim=1, reltol=1e-10),
               "[Ff]ail|converge|unresolved|exhausted|score check")
})

test_that("invtrgamma: P3 held profile exhaustion stops", {
  expect_error(mlinvtrgamma(c(.3,.8,1.7,4.2),scale=2,shape2=1,iterlim=1), "[Ff]ail|converge|unresolved|exhausted")
})
