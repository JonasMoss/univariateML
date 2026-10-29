test_that("mlgev matches constrained reference density oracles", {
  set.seed(20261029)
  for (shape in c(-.9, -.3, 0, .3, 2)) {
    x <- extraDistr::rgev(100, xi = shape)
    fit <- mlgev(x)
    objective <- function(p, fixed = NULL) {
      k <- if (is.null(fixed)) p[3] else fixed
      if (k < -.5 || k > 1) return(1e100)
      ans <- -sum(extraDistr::dgev(x, mu = p[1], sigma = exp(p[2]), xi = k, log = TRUE))
      if (is.finite(ans)) ans else 1e100
    }
    starts <- c(-.5, -.2, 0, .3, 1)
    values <- vapply(starts, function(k) {
      scale <- max(sd(x), abs(k) * diff(range(x)))
      optim(c(median(x), log(scale), k), objective, method = "Nelder-Mead",
            control = list(reltol = 1e-12, maxit = 3000))$value
    }, numeric(1))
    expect_lte(-as.numeric(logLik(fit)), min(values) + 1e-6)
    expect_equal(as.numeric(logLik(fit)), sum(dml(x, fit, log = TRUE)), tolerance = 1e-9)
    expect_equal(attr(logLik(fit), "df"), 3L)
    for (k in c(-.5, 0, .3, 1)) {
      held <- mlgev(x, xi = k)
      oracle <- optim(c(held[["mu"]] + .1, log(held[["sigma"]]) + .1),
                      function(p) objective(p, k), method = "Nelder-Mead",
                      control = list(reltol = 1e-12, maxit = 2000))
      expect_equal(-as.numeric(logLik(held)), oracle$value, tolerance = 1e-7)
      expect_equal(unname(held[["xi"]]), k)
      expect_equal(attr(held, "fixed"), "xi")
      expect_equal(attr(logLik(held), "df"), 2L)
    }
    gumbel <- mlgumbel(x)
    zero <- mlgev(x, xi = 0)
    expect_equal(as.numeric(zero[c("mu", "sigma")]), as.numeric(gumbel), tolerance = 1e-7)
  }
})
test_that("mlgev reports invalid inputs and returns attained bounds", {
  for (x in list(c(1,2), c(0,0,1), rep(1,5)))
    expect_error(mlgev(x), "at least three|multiplicity")
  for (k in c(-.51,1.01,Inf,NaN)) expect_error(mlgev(1:5, xi = k), "in \\[-0.5, 1\\]")
  expect_error(mlgev(c(1,2,NA)))
  expect_error(mlgev(1:5, iterlim = 1), "converge")
  for (k in c(-.9, 2)) {
    x <- extraDistr::qgev(((1:400)-.5)/400, xi = k)
    fit <- mlgev(x)
    expect_equal(unname(fit[["xi"]]), if (k < 0) -.5 else 1)
  }
})

test_that("runtime GEV repairs use stable Gumbel transforms and preserve RNG checks", {
  fixes <- univariateML:::univariateML_reference_fixes
  for (type in c("q", "p", "r")) {
    fix <- fixes[[paste0("extraDistr::", type, "gev")]]
    expect_true(fix$broken())
    ref <- getExportedValue("extraDistr", paste0(type, "gev"))
    if (type == "r") {
      set.seed(3); actual <- fix$replacement(20, mu = 2, sigma = 3, xi = .3)
      set.seed(3); expected <- ref(20, mu = 2, sigma = 3, xi = .3)
      expect_equal(actual, expected, tolerance = 1e-12)
      set.seed(3); seed <- .Random.seed
      expect_true(fix$broken())
      expect_identical(.Random.seed, seed)
      rm(".Random.seed", envir = .GlobalEnv)
      expect_true(fix$broken())
      expect_false(exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
      set.seed(3); actual <- fix$replacement(20, mu = c(0,1), sigma = c(1,2), xi = c(-.2,.3))
      set.seed(3); expected <- ref(20, mu = c(0,1), sigma = c(1,2), xi = c(-.2,.3))
      expect_equal(actual, expected, tolerance = 1e-12)
      set.seed(3); e <- rexp(20)
      expected <- expm1(-1e-16*log(e))/1e-16
      set.seed(3); expect_equal(fix$replacement(20, xi = 1e-16), expected, tolerance = 1e-12)
    } else for (lower in c(FALSE, TRUE)) for (logs in c(FALSE, TRUE)) {
      x <- if (type == "q") c(.1,.5,.9) else c(-.5,.2,1)
      if (type == "q" && logs) x <- log(x)
      expect_equal(fix$replacement(x, mu = c(0,.1,.2), sigma = 2, xi = .3,
                                   lower.tail = lower, log.p = logs),
                   ref(x, mu = c(0,.1,.2), sigma = 2, xi = .3,
                       lower.tail = lower, log.p = logs), tolerance = 1e-12)
      if (type == "q") {
        u <- c(.1,.5,.9)
        z <- -log(if (lower) -log(u) else -log1p(-u))
        expected <- expm1(1e-16*z)/1e-16
        expect_equal(fix$replacement(if (logs) log(u) else u, xi = 1e-16,
                                     lower.tail = lower, log.p = logs),
                     expected, tolerance = 1e-12)
        expect_equal(fix$replacement(if (logs) c(-Inf,0) else c(0,1),
                                     xi = -.5, lower.tail = lower, log.p = logs),
                     if (lower) c(-Inf,2) else c(2,-Inf))
      }
    }
  }
  p <- fixes[["extraDistr::pgev"]]$replacement
  expect_equal(p(c(1000,2000), lower.tail = FALSE, log.p = TRUE), c(-1000,-2000))
  z <- c(.1,1,10); xi <- c(-.5,0,.5)
  t <- ifelse(xi == 0, -z, -log1p(xi*z)/xi)
  expected <- log(-expm1(-exp(t))); expected[1] <- log(-expm1(-exp(t[1])))
  expect_equal(p(z, xi = xi, lower.tail = FALSE, log.p = TRUE), expected, tolerance = 1e-12)
})

test_that("GEV fixed shape accepts harmless ties", {
  for (x in list(c(0,1,1,2),c(0,1,2,2,2),c(0,0,0,1))) for(k in c(-.5,0,.3)) {
    f <- mlgev(x,xi=k)
    objective <- function(p) {
      v <- -sum(extraDistr::dgev(x,mu=p[1],sigma=exp(p[2]),xi=k,log=TRUE))
      if(is.finite(v)) v else 1e100
    }
    o <- optim(c(f["mu"],log(f["sigma"])),objective,method="Nelder-Mead",control=list(reltol=1e-12,maxit=2000))
    expect_equal(as.numeric(logLik(f)), -o$value, tolerance=1e-7)
  }
  expect_error(mlgev(c(0,0,0,1),xi=.5), "multiplicity")
})

test_that("gev: new holds agree with independent multistart reference fits", {
  set.seed(81029)
  id <- "gev"
  pars <- c("mu","sigma","xi")
  masks <- c(1,2,3,5,6,7)
  samples <- list(
    extraDistr::rgev(20, xi=.3),
    extraDistr::rgev(100, xi=.3),
    c(.2,.5,.5,1,2,3,4), c(.2,.8),
    if(id=="frechet") extraDistr::qgumbel((1:20-.5)/20) else rexp(20),
    if(id=="gev") extraDistr::qgev((1:20-.5)/20,xi=-.9) else runif(20,.1,2))
  density <- getExportedValue("extraDistr",paste0("d",id))
  for(x in samples) for(mask in masks) {
    held <- pars[as.logical(intToBits(mask)[1:3])]
    unit <- max(sd(x),diff(range(x)),.1)
    base <- if(id=="frechet") list(lambda=2,mu=min(x)-unit,sigma=unit) else
      list(mu=if(id=="gpd") min(x)-unit else median(x),sigma=unit,xi=.3)
    values <- base[held]
    args <- c(list(x=x),values)
    if(id=="frechet" && !"mu" %in% held) args["mu"] <- list(NULL)
    f <- do.call(get(paste0("ml",id)),args)
    expect_identical(unname(as.numeric(f[held])),unname(unlist(values)))
    expect_setequal(attr(f,"fixed"),held)
    expect_equal(as.numeric(logLik(f)),sum(do.call(density,c(list(x=x,log=TRUE),as.list(f)))),tolerance=1e-8)
    free <- setdiff(pars,held)
    if(!length(free)) next
    encode <- function(p,a) switch(p,lambda=log(a),sigma=log(a/unit),
      mu=if(id=="gev") (a-min(x))/unit else log((min(x)-a)/unit),xi=a)
    decode <- function(p,a) switch(p,lambda=exp(a),sigma=unit*exp(a),
      mu=if(id=="gev") min(x)+unit*a else min(x)-unit*exp(a),xi=a)
    objective <- function(z) {
      a <- base
      for(j in seq_along(free)) a[[free[j]]] <- decode(free[j],z[j])
      if(any(!is.finite(unlist(a))) ||
         (id=="gev" && (a$xi < -.5 || a$xi>1)) ||
         (id=="gpd" && a$xi < -.5) ||
         (id=="frechet" && !"lambda" %in% held && !"mu" %in% held && a$lambda<1)) return(1e100)
      ll <- suppressWarnings(sum(do.call(density,c(list(x=x,log=TRUE),a))))
      if(is.finite(ll)) -ll else 1e100
    }
    z0 <- vapply(free,function(p) encode(p,base[[p]]),0.)
    vals <- vapply(c(-1,-.3,0,.5,1),function(delta) {
      z <- z0+delta
      suppressWarnings(optim(z,objective,method="Nelder-Mead",
        control=list(maxit=4000,reltol=1e-12))$value)
    },0.)
    # Start an additional reference solve at the result to check its score;
    # the other five starts are independent of the fitter.
    zf <- vapply(free,function(p) encode(p,f[[p]]),0.)
    vals <- c(vals,suppressWarnings(optim(zf,objective,method="Nelder-Mead",
      control=list(maxit=4000,reltol=1e-12))$value))
    expect_equal(-as.numeric(logLik(f)),min(vals),tolerance=2e-6)
  }
})

test_that("GEV holds validate support, controls and finite coefficients", {
  for(v in c(0,-1,Inf,NaN)) expect_error(mlgev(1:5,sigma=v),"positive.*finite")
  expect_error(mlgev(1:5,mu=Inf),"finite")
  expect_error(mlgev(1:5,mu=0,sigma=1,xi=-.5),"support|holds")
  expect_error(mlgev(1:5,mu=0,sigma=1,iterlim=1),"converge")
  expect_error(mlgev(c(0,0,1,2),mu=0,xi=1),"multiplicity")
  f <- mlgev(2,sigma=3,xi=0)
  expect_equal(unname(f["mu"]),2)
})

test_that("GEV held location compares half-minimum ties with the collapse face", {
  for(x in list(c(0,0,1,2),c(0,0,.01,100),c(0,0,0,1))) {
    f <- tryCatch(mlgev(x,mu=0),error=identity)
    if(inherits(f,"error")) expect_match(conditionMessage(f),"scale collapse")
    else {
      face <- -sum(x==0)-2*sum(log(x[x>0]))
      expect_gt(as.numeric(logLik(f)),face)
    }
  }
  expect_error(mlgev(rep(0,4),mu=0,xi=0),"scale collapse")
})

test_that("GEV new holds permit held positive shape above the free-shape bound", {
  x <- c(0,1,2,4,10)
  for(h in list(list(mu=-1),list(sigma=2),list(mu=0,sigma=2))) {
    f <- do.call(mlgev,c(list(x=x,xi=2),h))
    expect_identical(unname(f["xi"]),2)
    expect_equal(as.numeric(logLik(f)),sum(extraDistr::dgev(x,f[1],f[2],f[3],log=TRUE)),tolerance=1e-8)
  }
  expect_error(mlgev(c(0,0,1,2),mu=0,xi=2),"scale collapse")
})

test_that("GEV new holds allow constant samples when scale cannot collapse", {
  for(h in list(list(mu=0),list(sigma=2),list(mu=0,sigma=2),
                list(mu=0,xi=.3),list(sigma=2,xi=.3),list(mu=0,sigma=2,xi=.3))) {
    f <- do.call(mlgev,c(list(x=2),h))
    expect_equal(as.numeric(logLik(f)),sum(extraDistr::dgev(2,f[1],f[2],f[3],log=TRUE)),tolerance=1e-8)
    expect_identical(unname(as.numeric(f[names(h)])),unname(unlist(h)))
  }
})
