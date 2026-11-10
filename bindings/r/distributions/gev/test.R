# Independent R7a likelihood: former references assign zero at xi=-1 endpoints.
extreme_closed_density <- function(id, x, ..., log = FALSE) {
  a <- list(...)
  if (id %in% c("gev", "gpd") && !is.null(a$xi) && a$xi == -1) {
    mu <- a$mu; sigma <- a$sigma
    slack <- 8*.Machine$double.eps*pmax(1, abs(x), abs(mu), sigma)
    ll <- if (id == "gev") -base::log(sigma)+(x-mu)/sigma-1 else rep(-base::log(sigma),length(x))
    ll[x > mu+sigma+slack | (id == "gpd" & x < mu)] <- -Inf
    return(if(log) ll else exp(ll))
  }
  do.call(get(paste0("d",id), asNamespace("univariateML")), c(list(x=x,log=log),a))
}

test_that("mlgev matches constrained reference density oracles", {
  skip_unless_tier("full")
  set.seed(20261029)
  for (shape in c(-.9, -.3, 0, .3, 2)) {
    x <- rgev(n = 100, mu = 0, sigma = 1, xi = shape)
    fit <- suppressWarnings(mlgev(x), classes = "univariateML_restricted")
    objective <- function(p, fixed = NULL) {
      k <- if (is.null(fixed)) p[3] else fixed
      if (k < -1 || k > 1) return(1e100)
      ans <- -sum(dgev(x = x, mu = p[1], sigma = exp(p[2]), xi = k, log = TRUE))
      if (is.finite(ans)) ans else 1e100
    }
    starts <- c(-.5, -.2, 0, .3, 1)
    values <- vapply(starts, function(k) {
      scale <- max(sd(x), abs(k) * diff(range(x)))
      optim(c(median(x), log(scale), k), objective, method = "Nelder-Mead",
            control = list(reltol = 1e-12, maxit = 3000))$value
    }, numeric(1))
    expect_lte(-as.numeric(logLik(fit)), min(values) + 1e-6)
    expect_equal(as.numeric(logLik(fit)), sum(extreme_closed_density("gev",x,mu=fit[[1]],sigma=fit[[2]],xi=fit[[3]],log=TRUE)), tolerance = 1e-9)
    expect_equal(attr(logLik(fit), "df"), 3L)
    for (k in c(-.5, 0, .3, 1)) {
      held <- suppressWarnings(mlgev(x, xi = k), classes = "univariateML_restricted")
      oracle <- optim(c(held[["mu"]] + .1, log(held[["sigma"]]) + .1),
                      function(p) objective(p, k), method = "Nelder-Mead",
                      control = list(reltol = 1e-12, maxit = 2000))
      expect_equal(-as.numeric(logLik(held)), oracle$value, tolerance = 1e-7)
      expect_equal(unname(held[["xi"]]), k)
      expect_equal(attr(held, "fixed"), "xi")
      expect_equal(attr(logLik(held), "df"), 2L)
    }
    gumbel <- mlgumbel(x)
    zero <- suppressWarnings(mlgev(x, xi = 0), classes = "univariateML_restricted")
    expect_equal(as.numeric(zero[c("mu", "sigma")]), as.numeric(gumbel), tolerance = 1e-7)
  }
})
test_that("mlgev reports invalid inputs and returns attained bounds", {
  for (x in list(c(1,2), c(0,0,1), rep(1,5)))
    expect_error(suppressWarnings(mlgev(x), classes = "univariateML_restricted"), "at least three|multiplicity")
  for (k in c(-1.01,Inf,NaN)) expect_error(suppressWarnings(mlgev(1:5, xi = k), classes = "univariateML_restricted"), "in \\[-1, 1\\]")
  expect_error(suppressWarnings(mlgev(c(1,2,NA)), classes = "univariateML_restricted"))
  expect_error(suppressWarnings(mlgev(1:5, iterlim = 1), classes = "univariateML_restricted"), "converge")
  for (k in c(-.9, 2)) {
    x <- qgev(p = ((1:400) - 0.5)/400, mu = 0, sigma = 1, xi = k)
    fit <- suppressWarnings(mlgev(x), classes = "univariateML_restricted")
    if(k<0) expect_lt(unname(fit[["xi"]]), -.5) else expect_equal(unname(fit[["xi"]]), 1)
  }
})


test_that("GEV fixed shape accepts harmless ties", {
  for (x in list(c(0,1,1,2),c(0,1,2,2,2),c(0,0,0,1))) for(k in c(-.5,0,.3)) {
    f <- suppressWarnings(mlgev(x,xi=k), classes = "univariateML_restricted")
    objective <- function(p) {
      v <- -sum(dgev(x = x, mu = p[1], sigma = exp(p[2]), xi = k, log = TRUE))
      if(is.finite(v)) v else 1e100
    }
    o <- optim(c(f["mu"],log(f["sigma"])),objective,method="Nelder-Mead",control=list(reltol=1e-12,maxit=2000))
    expect_equal(as.numeric(logLik(f)), -o$value, tolerance=1e-7)
  }
  expect_error(suppressWarnings(mlgev(c(0,0,0,1),xi=.5), classes = "univariateML_restricted"), "multiplicity")
})

test_that("gev: new holds agree with independent multistart reference fits", {
  skip_unless_tier("full")
  set.seed(81029)
  id <- "gev"
  pars <- c("mu","sigma","xi")
  masks <- c(1,2,3,5,6,7)
  samples <- list(
    rgev(n = 20, mu = 0, sigma = 1, xi = 0.3),
    rgev(n = 100, mu = 0, sigma = 1, xi = 0.3),
    c(.2,.5,.5,1,2,3,4), c(.2,.8),
    if(id=="frechet") qgumbel(p = (1:20 - 0.5)/20, mu = 0, sigma = 1) else rexp(20),
    if(id=="gev") qgev(p = (1:20 - 0.5)/20, mu = 0, sigma = 1, xi = -0.9) else runif(20,.1,2))
  density <- function(x,...,log=FALSE) extreme_closed_density(id,x,...,log=log)
  for(x in samples) for(mask in masks) {
    held <- pars[as.logical(intToBits(mask)[1:3])]
    unit <- max(sd(x),diff(range(x)),.1)
    base <- if(id=="frechet") list(lambda=2,mu=min(x)-unit,sigma=unit) else
      list(mu=if(id=="gpd") min(x)-unit else median(x),sigma=unit,xi=.3)
    values <- base[held]
    args <- c(list(x=x),values)
    if(id=="frechet" && !"mu" %in% held) args["mu"] <- list(NULL)
    f <- suppressWarnings(do.call(get(paste0("ml",id)),args), classes="univariateML_restricted")
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
         (id=="gev" && (a$xi < -1 || a$xi>1)) ||
         (id=="gpd" && a$xi < -1) ||
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
  for(v in c(0,-1,Inf,NaN)) expect_error(suppressWarnings(mlgev(1:5,sigma=v), classes = "univariateML_restricted"),"positive.*finite")
  expect_error(suppressWarnings(mlgev(1:5,mu=Inf), classes = "univariateML_restricted"),"finite")
  expect_error(suppressWarnings(mlgev(1:5,mu=0,sigma=1,xi=-.5), classes = "univariateML_restricted"),"support|holds")
  expect_error(suppressWarnings(mlgev(1:5,mu=0,sigma=1,iterlim=1), classes = "univariateML_restricted"),"converge")
  expect_error(suppressWarnings(mlgev(c(0,0,1,2),mu=0,xi=1), classes = "univariateML_restricted"),"multiplicity")
  f <- suppressWarnings(mlgev(2,sigma=3,xi=0), classes = "univariateML_restricted")
  expect_equal(unname(f["mu"]),2)
})

test_that("GEV held location compares half-minimum ties with the collapse face", {
  for(x in list(c(0,0,1,2),c(0,0,.01,100),c(0,0,0,1))) {
    f <- tryCatch(suppressWarnings(mlgev(x,mu=0), classes = "univariateML_restricted"),error=identity)
    if(inherits(f,"error")) expect_match(conditionMessage(f),"scale collapse")
    else {
      face <- -sum(x==0)-2*sum(log(x[x>0]))
      expect_gt(as.numeric(logLik(f)),face)
    }
  }
  expect_error(suppressWarnings(mlgev(rep(0,4),mu=0,xi=0), classes = "univariateML_restricted"),"scale collapse")
})

test_that("GEV free fit compares half-minimum ties with the free collapse face", {
  # TASK-85/87: with mu, sigma and xi all free, this used to be
  # rejected outright (InvalidData), the same as a strict majority tie.
  # Mirrors the held-mu test above; see
  # dev/research/evaluations/2026-10-29-tail85-report.md for why every sample
  # tried gives the collapse-face branch, not the interior-fit one.
  for(x in list(c(0,0,1,2),c(0,0,.01,100),c(0,0,0,1,2,5))) {
    f <- tryCatch(suppressWarnings(mlgev(x), classes = "univariateML_restricted"),error=identity)
    if(inherits(f,"error")) expect_match(conditionMessage(f),"scale collapse")
    else {
      m <- min(x)
      face <- length(x)*log(2)-length(x)-2*sum(log(x[x>m]-m))
      expect_gt(as.numeric(logLik(f)),face)
    }
  }
  # Strict majority (k=3 of n=4) still diverges outright, unaffected.
  expect_error(suppressWarnings(mlgev(c(0,0,0,1)), classes = "univariateML_restricted"),"multiplicity")
})

test_that("GEV new holds permit held positive shape above the free-shape bound", {
  x <- c(0,1,2,4,10)
  for(h in list(list(mu=-1),list(sigma=2),list(mu=0,sigma=2))) {
    f <- do.call(mlgev,c(list(x=x,xi=2),h))
    expect_identical(unname(f["xi"]),2)
    expect_equal(as.numeric(logLik(f)),sum(dgev(x = x, mu = f[1], sigma = f[2], xi = f[3], log = TRUE)),tolerance=1e-8)
  }
  expect_error(suppressWarnings(mlgev(c(0,0,1,2),mu=0,xi=2), classes = "univariateML_restricted"),"scale collapse")
})

test_that("GEV new holds allow constant samples when scale cannot collapse", {
  for(h in list(list(mu=0),list(sigma=2),list(mu=0,sigma=2),
                list(mu=0,xi=.3),list(sigma=2,xi=.3),list(mu=0,sigma=2,xi=.3))) {
    f <- suppressWarnings(do.call(mlgev,c(list(x=2),h)), classes="univariateML_restricted")
    expect_equal(as.numeric(logLik(f)),sum(extreme_closed_density("gev",2,mu=f[[1]],sigma=f[[2]],xi=f[[3]],log=TRUE)),tolerance=1e-8)
    expect_identical(unname(as.numeric(f[names(h)])),unname(unlist(h)))
  }
})

test_that("GEV tiny shapes approach Gumbel and logarithmic tails invert", {
  fit <- mlgev(c(-1, 0, 1), mu = 0, sigma = 1, xi = 1e-16)
  expect_equal(qml(c(.1, .5, .9), fit), -log(-log(c(.1, .5, .9))),
               tolerance = 1e-12)
  for (lower in c(TRUE, FALSE)) {
    lp <- c(-1, -40, -700)
    expect_equal(pml(qml(lp, fit, lower.tail = lower, log.p = TRUE), fit,
                     lower.tail = lower, log.p = TRUE), lp, tolerance = 1e-10)
  }
})

test_that("gev evaluates its attained xi=-1 endpoint", {
  fit <- suppressWarnings(mlgev(c(2,3,4), mu=2, sigma=3, xi=-1))
  expect_equal(dml(5, fit), 1/3)
  expect_equal(dml(5, fit, log=TRUE), -log(3))
  expect_equal(pml(5, fit), 1)
  expect_equal(pml(5, fit, lower.tail=FALSE, log.p=TRUE), -Inf)
  expect_equal(qml(1, fit), 5)
  expect_equal(qml(-Inf, fit, lower.tail=FALSE, log.p=TRUE), 5)
})

test_that("GEV concave location certificate has a typed non-convergence stop", {
  e <- tryCatch(mlgev(c(-2,0,.3,1,5),sigma=2,xi=-.3,iterlim=1),error=identity)
  expect_s3_class(e,"univariateML_numerical")
  f <- mlgev(c(-2,0,.3,1,5),sigma=2,xi=-.3)
  oracle <- optimize(function(m) -sum(dgev(c(-2,0,.3,1,5),m,2,-.3,log=TRUE)),c(5+2/(-.3)+1e-10,10),tol=1e-10)
  expect_equal(unname(f["mu"]),oracle$minimum,tolerance=1e-7)
})

test_that("GEV location certificate rejects a rounded location", {
  e <- tryCatch(mlgev(1e300,sigma=1,xi=-.3),error=identity)
  expect_s3_class(e,"univariateML_numerical")
})

test_that("GEV half-minimum pair certificate records the supremum and attained exception", {
  x <- c(0,0,1,2)
  e <- tryCatch(mlgev(x),error=identity)
  expect_s3_class(e,"univariateML_nomle")
  f <- suppressWarnings(mlgev(c(0,0,2,2)))
  expect_equal(unname(f[c("mu","sigma","xi")]),c(1,1,-1))
  expect_equal(as.numeric(logLik(f)),-4)
})

test_that("GEV shape-only certificate preserves holds and handles flat shapes", {
  f <- suppressWarnings(mlgev(c(-.7,-.2,0,.4,.8,1.5,3),mu=.1,sigma=1.3,iterlim=1000))
  expect_equal(unname(f[c("mu","sigma")]),c(.1,1.3))
  f <- mlgev(rep(4,3),mu=4,sigma=2)
  expect_equal(unname(f["xi"]),0)
  e <- tryCatch(mlgev(c(-.5,-.3,0,.2,.5),mu=0,sigma=1,iterlim=1),error=identity)
  expect_s3_class(e,"univariateML_numerical")
})

test_that("GEV held-minimum half ties distinguish the attained reverse exponential", {
  e <- tryCatch(mlgev(c(0,0,1,2),mu=0),error=identity)
  expect_s3_class(e,"univariateML_nomle")
  expect_equal(e$loglik_upper,-2-2*log(2))
  for(s in list(NULL,2)) {
    f <- suppressWarnings(mlgev(c(0,0,2,2),mu=0,sigma=s))
    expect_equal(unname(f[c("mu","sigma","xi")]),c(0,2,-1))
    expect_equal(as.numeric(logLik(f)),-2-4*log(2))
  }
})

test_that("GEV two-point minority fits use the certified count formula", {
  f <- suppressWarnings(mlgev(c(0,rep(1,100))))
  expect_equal(unname(f[c("mu","sigma","xi")]),c(100/101,1/101,-1),tolerance=1e-10)
  e <- tryCatch(mlgev(c(-9,rep(1,9))),error=identity)
  expect_s3_class(e,"univariateML_numerical")
})

test_that("GEV two-point compatible holds inherit the global certificate", {
  for(h in list(list(mu=1.5),list(sigma=.5),list(mu=1.5,sigma=.5))) {
    f <- suppressWarnings(do.call(mlgev,c(list(x=c(0,2,2,2)),h)),classes="univariateML_restricted")
    expect_equal(as.numeric(f),c(1.5,.5,-1))
  }
})

test_that("GEV compatible holds preserve certificate precision stops", {
  e <- tryCatch(mlgev(c(0,2,2,2),mu=1.5,sigma=.5,reltol=1e-15),error=identity)
  expect_s3_class(e,"univariateML_numerical")
  expect_true(is.finite(e$loglik_upper))
})

test_that("GEV joint certificate covers the seam and preserves numerical witnesses", {
  x <- c(0,1,1,1,2,3,4)
  f <- mlgev(x)
  expect_equal(unname(f["xi"]),0.022797535412548,tolerance=1e-10)
  for(k in c(-1,0,1)) {
    fixed <- suppressWarnings(mlgev(x,xi=k))
    expect_gte(as.numeric(logLik(f)),as.numeric(logLik(fixed))-1e-8)
  }
  e <- tryCatch(mlgev(x,reltol=1e-16),error=identity)
  expect_s3_class(e,"univariateML_numerical")
  expect_true(length(e$candidates)>0)
  expect_false(inherits(e,"univariateML_tie"))
  face <- suppressWarnings(mlgev(c(0,1,2,3,3)))
  expect_equal(unname(face["xi"]),-1)
})

test_that("GEV negative-shape coefficient corners preserve the gap dependency", {
  x <- c(0,.22725282343514291,.56276665892011701,.58275143915118077,1)
  f <- mlgev(x)
  expect_equal(as.numeric(f),c(.35972611509033436,.33733043138434093,-.31554486140157456),tolerance=1e-10)
})

test_that("gev audit-support case 2682 checks the public fit", {
  x <- c(-0.6514892575075315,0.7190118994986324,1.1956145927670203)
  fit <- suppressWarnings(tryCatch(mlgev(x,reltol=1e-10,iterlim=100,mu=0.006623074223695902),error=identity))
  expect_false(inherits(fit,"error"))
  if(inherits(fit,"error")) {
    expect_match(conditionMessage(fit),"numerical|converge|accuracy|certificate",ignore.case=TRUE)
  } else {
    raw <- sum(dml(x,fit,log=TRUE))
    expect_true(is.finite(raw))
    expect_equal(as.numeric(logLik(fit)),raw,tolerance=1e-11)
  }
})

test_that("gev audit-support case 2695 checks the public fit", {
  x <- c(0.7950005938035326,0.7950013888057164,0.7950021838079002,0.795002978810084,0.7950037738122678)
  fit <- suppressWarnings(tryCatch(mlgev(x,reltol=1e-10,iterlim=100,mu=0.006623074223695902),error=identity))
  expect_false(inherits(fit,"error"))
  if(inherits(fit,"error")) {
    expect_match(conditionMessage(fit),"numerical|converge|accuracy|certificate",ignore.case=TRUE)
  } else {
    raw <- sum(dml(x,fit,log=TRUE))
    expect_true(is.finite(raw))
    expect_equal(as.numeric(logLik(fit)),raw,tolerance=1e-11)
  }
})

test_that("gev audit-support case 2707 checks the public fit", {
  x <- c(-6.514892575075315e-151,7.190118994986324e-151,1.1956145927670203e-150,2.1354437992679257e-152,1.33255002155901e-150,9.99055037892868e-151,4.443082381781675e-151,1.784490546684275e-150,7.950021838079003e-151,1.1415363726467913e-150,-6.653630472986568e-151,-8.686654093177045e-151)
  fit <- suppressWarnings(tryCatch(mlgev(x,reltol=1e-10,iterlim=100,sigma=0.9955800131732704),error=identity))
  expect_false(inherits(fit,"error"))
  if(inherits(fit,"error")) {
    expect_match(conditionMessage(fit),"numerical|converge|accuracy|certificate",ignore.case=TRUE)
  } else {
    raw <- sum(dml(x,fit,log=TRUE))
    expect_true(is.finite(raw))
    expect_equal(as.numeric(logLik(fit)),raw,tolerance=1e-11)
  }
})

test_that("gev audit-support case 10949 checks the public fit", {
  x <- c(-3.2212091334838218,-3.221208133483822,-3.221207133483822,-3.221205133483822,-3.221201133483822)
  fit <- suppressWarnings(tryCatch(mlgev(x,reltol=1e-10,iterlim=100,sigma=0.9955800131732704),error=identity))
  expect_false(inherits(fit,"error"))
  if(inherits(fit,"error")) {
    expect_match(conditionMessage(fit),"numerical|converge|accuracy|certificate",ignore.case=TRUE)
  } else {
    raw <- sum(dml(x,fit,log=TRUE))
    expect_true(is.finite(raw))
    expect_equal(as.numeric(logLik(fit)),raw,tolerance=1e-11)
  }
})

test_that("gev audit-support case 10957 checks the public fit", {
  x <- c(-3.221210133483815,-3.2212101334838077,-3.2212101334838006,-3.2212101334837864,-3.221210133483758)
  fit <- suppressWarnings(tryCatch(mlgev(x,reltol=1e-10,iterlim=100,xi=0.30843601546572924),error=identity))
  expect_true(inherits(fit,"error"))
  if(inherits(fit,"error")) {
    expect_match(conditionMessage(fit),"numerical|converge|accuracy|certificate",ignore.case=TRUE)
  } else {
    raw <- sum(dml(x,fit,log=TRUE))
    expect_true(is.finite(raw))
    expect_equal(as.numeric(logLik(fit)),raw,tolerance=1e-11)
  }
})

test_that("gev audit-support case 10965 checks the public fit", {
  x <- c(-3.221210133482822,-3.2212101334818217,-3.221210133480822,-3.221210133478822,-3.221210133474822)
  fit <- suppressWarnings(tryCatch(mlgev(x,reltol=1e-10,iterlim=100,mu=0.006623074223695902,sigma=0.9955800131732704,xi=0.30843601546572924),error=identity))
  expect_false(inherits(fit,"error"))
  if(inherits(fit,"error")) {
    expect_match(conditionMessage(fit),"numerical|converge|accuracy|certificate",ignore.case=TRUE)
  } else {
    raw <- sum(dml(x,fit,log=TRUE))
    expect_true(is.finite(raw))
    expect_equal(as.numeric(logLik(fit)),raw,tolerance=1e-11)
  }
})

test_that("gev audit-support case 10966 checks the public fit", {
  x <- c(-3.221210133483815,-3.2212101334838077,-3.2212101334838006,-3.2212101334837864,-3.221210133483758)
  fit <- suppressWarnings(tryCatch(mlgev(x,reltol=1e-10,iterlim=100,mu=0.006623074223695902,sigma=0.9955800131732704,xi=0.30843601546572924),error=identity))
  expect_false(inherits(fit,"error"))
  if(inherits(fit,"error")) {
    expect_match(conditionMessage(fit),"numerical|converge|accuracy|certificate",ignore.case=TRUE)
  } else {
    raw <- sum(dml(x,fit,log=TRUE))
    expect_true(is.finite(raw))
    expect_equal(as.numeric(logLik(fit)),raw,tolerance=1e-11)
  }
})

test_that("gev audit-support case 2708 checks the public fit", {
  x <- c(-6.514892575075315e+149,7.190118994986324e+149,1.1956145927670203e+150,2.1354437992679258e+148,1.33255002155901e+150,9.99055037892868e+149,4.443082381781675e+149,1.784490546684275e+150,7.950021838079002e+149,1.1415363726467912e+150,-6.653630472986568e+149,-8.686654093177046e+149)
  fit <- suppressWarnings(tryCatch(mlgev(x,reltol=1e-10,iterlim=100,sigma=0.9955800131732704),error=identity))
  expect_true(inherits(fit,"error"))
  if(inherits(fit,"error")) {
    expect_match(conditionMessage(fit),"numerical|converge|accuracy|certificate",ignore.case=TRUE)
  } else {
    raw <- sum(dml(x,fit,log=TRUE))
    expect_true(is.finite(raw))
    expect_equal(as.numeric(logLik(fit)),raw,tolerance=1e-11)
  }
})

test_that("gev audit-support case 2772 checks the public fit", {
  x <- c(-6.514892575075315e+149,7.190118994986324e+149,1.1956145927670203e+150,2.1354437992679258e+148,1.33255002155901e+150,9.99055037892868e+149,4.443082381781675e+149,1.784490546684275e+150,7.950021838079002e+149,1.1415363726467912e+150,-6.653630472986568e+149,-8.686654093177046e+149)
  fit <- suppressWarnings(tryCatch(mlgev(x,reltol=1e-10,iterlim=100,sigma=0.9955800131732704,xi=0.30843601546572924),error=identity))
  expect_true(inherits(fit,"error"))
  if(inherits(fit,"error")) {
    expect_match(conditionMessage(fit),"numerical|converge|accuracy|certificate",ignore.case=TRUE)
  } else {
    raw <- sum(dml(x,fit,log=TRUE))
    expect_true(is.finite(raw))
    expect_equal(as.numeric(logLik(fit)),raw,tolerance=1e-11)
  }
})

test_that("gev audit-support case 2788 checks the public fit", {
  x <- c(-6.514892575075315e+149,7.190118994986324e+149,1.1956145927670203e+150,2.1354437992679258e+148,1.33255002155901e+150,9.99055037892868e+149,4.443082381781675e+149,1.784490546684275e+150,7.950021838079002e+149,1.1415363726467912e+150,-6.653630472986568e+149,-8.686654093177046e+149)
  fit <- suppressWarnings(tryCatch(mlgev(x,reltol=1e-10,iterlim=100,mu=0.006623074223695902,sigma=0.9955800131732704,xi=0.30843601546572924),error=identity))
  expect_true(inherits(fit,"error"))
  if(inherits(fit,"error")) {
    expect_match(conditionMessage(fit),"compatible support",ignore.case=TRUE)
  } else {
    raw <- sum(dml(x,fit,log=TRUE))
    expect_true(is.finite(raw))
    expect_equal(as.numeric(logLik(fit)),raw,tolerance=1e-11)
  }
})

test_that("gev audit-support case 10964 checks the public fit", {
  x <- c(-3.2212091334838218,-3.221208133483822,-3.221207133483822,-3.221205133483822,-3.221201133483822)
  fit <- suppressWarnings(tryCatch(mlgev(x,reltol=1e-10,iterlim=100,mu=0.006623074223695902,sigma=0.9955800131732704,xi=0.30843601546572924),error=identity))
  expect_false(inherits(fit,"error"))
  if(inherits(fit,"error")) {
    expect_match(conditionMessage(fit),"numerical|converge|accuracy|certificate",ignore.case=TRUE)
  } else {
    raw <- sum(dml(x,fit,log=TRUE))
    expect_true(is.finite(raw))
    expect_equal(as.numeric(logLik(fit)),raw,tolerance=1e-11)
  }
})

test_that("GEV support restoration preserves a small scale after translation", {
  x <- 1e8+c(0,.1,.2,.3,.4)
  x[2] <- x[2]+2^-25
  fit <- mlgev(x,xi=-1)
  expect_true(all(is.finite(dml(x,fit,log=TRUE))))
  expect_equal(sum(dml(x,fit,log=TRUE)),as.numeric(logLik(fit)),tolerance=1e-11)
  expect_equal(unname(fit["sigma"]),mean(max(x)-x),tolerance=1e-14)
})

test_that("GEV public support rejects negative zero residuals", {
  x <- c(-1, 0, .Machine$double.xmin * .Machine$double.eps)
  fit <- mlgev(x, sigma = 2, xi = -1)
  expect_equal(unname(fit["sigma"]), 2)
  expect_gt(unname(fit["mu"] + fit["sigma"]), 0)
  expect_error(univariateML:::mlgev_(x, xi = -1, mu = -2, sigma = 2),
               "support")
})
