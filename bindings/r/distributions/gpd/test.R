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

test_that("mlgpd agrees with multistart constrained density oracles", {
  skip_unless_tier("full")
  for (shape in c(.3, 0, -.3, -.8)) {
    x <- qgpd(p = ((1:200) - 0.5)/200, mu = 0, sigma = 2, xi = shape)
    fit <- suppressWarnings(mlgpd(x, mu = 0), classes = "univariateML_restricted")
    objective <- function(p) {
      if (p[2] < -1) return(1e100)
      ans <- -sum(dgpd(x = x, mu = 0, sigma = exp(p[1]), xi = p[2], log = TRUE))
      if (is.finite(ans)) ans else 1e100
    }
    fits <- lapply(c(-.5, -.2, .01, .3, 1), function(k)
      optim(c(log(2), k), objective, method = "Nelder-Mead",
            control = list(reltol = 1e-12, maxit = 2000)))
    best <- min(vapply(fits, function(z) z$value, numeric(1)))
    expect_equal(-as.numeric(logLik(fit)), best, tolerance = 1e-7)
    expect_equal(attr(fit, "fixed"), "mu")
    expect_equal(attr(logLik(fit), "df"), 2L)
    if (shape == -.8) expect_lt(unname(fit[["xi"]]), -.5)
    shifted <- suppressWarnings(mlgpd(x + 10, mu = 10), classes = "univariateML_restricted")
    expect_equal(unname(shifted[["mu"]]), 10)
    expect_equal(unname(shifted[c("sigma", "xi")]), unname(fit[c("sigma", "xi")]), tolerance = 1e-8)
    for (k in c(-.5, -.3, 0, .3, 2)) {
      held <- suppressWarnings(mlgpd(x + 10, mu = 10, xi = k), classes = "univariateML_restricted")
      lower <- if (k < 0) log(-k*max(x)) else -30
      oracle <- optimize(function(s) -sum(dgpd(x = x, mu = 0, sigma = exp(s), xi = k, log = TRUE)), c(lower + 1e-10, 20), tol = 1e-11)
      expect_equal(unname(held[["xi"]]), k)
      expect_equal(attr(held, "fixed"), c("mu", "xi"))
      expect_equal(attr(logLik(held), "df"), 1L)
      expect_equal(-as.numeric(logLik(held)), oracle$objective, tolerance = 1e-8)
      if (k == 0) expect_equal(unname(held[["sigma"]]), 1/unname(mlexp(x)[["rate"]]))
    }
  }
})
test_that("mlgpd rejects invalid thresholds, shapes and observations", {
  expect_error(suppressWarnings(mlgpd(c(1, 2, 3), mu = 1), classes = "univariateML_restricted"), "at or above|scale collapse")
  expect_error(suppressWarnings(mlgpd(c(-1, 2, 3), mu = 0), classes = "univariateML_restricted"), "at or above|scale collapse")
  expect_error(suppressWarnings(mlgpd(c(1, 2, 3), mu = Inf), classes = "univariateML_restricted"), "finite")
  for (k in c(-1.01, Inf, NaN)) expect_error(suppressWarnings(mlgpd(c(1,2,3), xi = k, mu = 0), classes = "univariateML_restricted"), "at least -1")
  expect_error(suppressWarnings(mlgpd(c(.2,.7,1,2,4), iterlim = 1, mu = 0), classes = "univariateML_restricted"), "converge")
  expect_error(suppressWarnings(mlgpd(c(1, 2, NA), mu = 0), classes = "univariateML_restricted"))
})

test_that("gpd: new holds agree with independent multistart reference fits", {
  skip_unless_tier("full")
  set.seed(81029)
  id <- "gpd"
  pars <- c("mu","sigma","xi")
  masks <- c(3,7)
  samples <- list(
    rgpd(n = 20, mu = 0, sigma = 1, xi = 0.3),
    rgpd(n = 100, mu = 0, sigma = 1, xi = 0.3),
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

test_that("GPD scale holds validate support and retain free-threshold rejection", {
  for(v in c(0,-1,Inf,NaN)) expect_error(suppressWarnings(mlgpd(1:5,mu=0,sigma=v), classes = "univariateML_restricted"),"positive.*finite")
  expect_error(suppressWarnings(mlgpd(1:5,mu=0,sigma=1,xi=-.5), classes = "univariateML_restricted"),"support|holds")
  expect_error(suppressWarnings(mlgpd(1:5,mu=0,sigma=1,iterlim=1), classes = "univariateML_restricted"),"converge")
  for(h in list(list(sigma=1),list(sigma=1,xi=0)))
    expect_error(do.call(mlgpd,c(list(x=1:5,mu=NULL),h)),"Estimating `mu` is not supported")
  expect_true(is.finite(logLik(suppressWarnings(mlgpd(1,mu=0,sigma=2), classes = "univariateML_restricted"))))
})

test_that("gpd evaluates its attained xi=-1 endpoint", {
  fit <- suppressWarnings(mlgpd(c(2,3,4), mu=2, sigma=3, xi=-1))
  expect_equal(dml(5, fit), 1/3)
  expect_equal(dml(5, fit, log=TRUE), -log(3))
  expect_equal(pml(5, fit), 1)
  expect_equal(pml(5, fit, lower.tail=FALSE, log.p=TRUE), -Inf)
  expect_equal(qml(1, fit), 5)
  expect_equal(qml(-Inf, fit, lower.tail=FALSE, log.p=TRUE), 5)
})

test_that("GPD exhausted tail bracket is a numerical stop", {
  x <- c(.1,.3,.5,1,2,4)
  for (args in list(list(),list(sigma=1))) {
    err <- tryCatch(do.call(mlgpd,c(list(x=x,mu=0,iterlim=1),args)),error=identity)
    expect_s3_class(err,"univariateML_stop")
    expect_equal(err$kind,"numerical")
    expect_false(inherits(err,"univariateML_budget"))
  }
  flat <- tryCatch(mlgpd(c(0,0,0),mu=0,sigma=2),error=identity)
  expect_s3_class(flat,"univariateML_stop")
  expect_equal(flat$kind,"tie")
  collapse <- tryCatch(mlgpd(c(0,1,2),mu=0),error=identity)
  expect_s3_class(collapse,"univariateML_stop")
  expect_equal(collapse$kind,"nomle")
  expect_equal(collapse$loglik_upper,Inf)
})


test_that("GPD threshold equality records its unattained finite supremum", {
  limit <- tryCatch(mlgpd(c(0,1,2),mu=0,xi=2),error=identity)
  expect_s3_class(limit,"univariateML_limit")
  expect_match(conditionMessage(limit),"finite unattained")
  expect_equal(limit$limit_loglik,-1.5*log(8),tolerance=1e-12)
  expect_equal(limit$loglik_upper,limit$limit_loglik)
})

test_that("GPD arithmetic guards expose a structured numerical stop", {
  for (smallest in c(.Machine$double.xmin*.Machine$double.eps,1e-100)) {
    err <- tryCatch(mlgpd(c(smallest,1),mu=0),error=identity)
    expect_s3_class(err,"univariateML_numerical")
    expect_equal(err$kind,"numerical")
    expect_match(conditionMessage(err),"comparisons unresolved|unrepresentable")
  }
})

test_that("GPD finite competing maxima give a structured tie", {
  a <- 0.07880675592383729
  err <- tryCatch(mlgpd(c(a,a,1),mu=0),error=identity)
  expect_s3_class(err,"univariateML_tie")
  expect_gte(nrow(err$candidates),2L)
  expect_true(all(err$candidates$loglik_lower<=err$candidates$loglik_upper))
  expect_lt(err$loglik_upper-err$loglik_lower,1e-7)
})

test_that("GPD exponential scale rejects an inaccurate subnormal mean", {
  tiny <- .Machine$double.xmin*.Machine$double.eps
  err <- tryCatch(mlgpd(c(tiny,2*tiny),mu=0,xi=0),error=identity)
  expect_s3_class(err,"univariateML_numerical")
  fit <- mlgpd(c(tiny,3*tiny),mu=0,xi=0)
  expect_identical(unname(fit[["sigma"]]),2*tiny)
})
