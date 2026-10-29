test_that("mlgpd agrees with multistart constrained density oracles", {
  for (shape in c(.3, 0, -.3, -.8)) {
    x <- extraDistr::qgpd(((1:200)-.5)/200, sigma = 2, xi = shape)
    fit <- mlgpd(x, mu = 0)
    objective <- function(p) {
      if (p[2] < -.5) return(1e100)
      ans <- -sum(extraDistr::dgpd(x, sigma = exp(p[1]), xi = p[2], log = TRUE))
      if (is.finite(ans)) ans else 1e100
    }
    fits <- lapply(c(-.5, -.2, .01, .3, 1), function(k)
      optim(c(log(2), k), objective, method = "Nelder-Mead",
            control = list(reltol = 1e-12, maxit = 2000)))
    best <- min(vapply(fits, function(z) z$value, numeric(1)))
    expect_equal(-as.numeric(logLik(fit)), best, tolerance = 1e-7)
    expect_equal(attr(fit, "fixed"), "mu")
    expect_equal(attr(logLik(fit), "df"), 2L)
    if (shape == -.8) expect_equal(unname(fit[["xi"]]), -.5)
    shifted <- mlgpd(x + 10, mu = 10)
    expect_equal(unname(shifted[["mu"]]), 10)
    expect_equal(unname(shifted[c("sigma", "xi")]), unname(fit[c("sigma", "xi")]), tolerance = 1e-8)
    for (k in c(-.5, -.3, 0, .3, 2)) {
      held <- mlgpd(x + 10, mu = 10, xi = k)
      lower <- if (k < 0) log(-k*max(x)) else -30
      oracle <- optimize(function(s) -sum(extraDistr::dgpd(x, sigma = exp(s), xi = k, log = TRUE)), c(lower + 1e-10, 20), tol = 1e-11)
      expect_equal(unname(held[["xi"]]), k)
      expect_equal(attr(held, "fixed"), c("mu", "xi"))
      expect_equal(attr(logLik(held), "df"), 1L)
      expect_equal(-as.numeric(logLik(held)), oracle$objective, tolerance = 1e-8)
      if (k == 0) expect_equal(unname(held[["sigma"]]), 1/unname(mlexp(x)[["rate"]]))
    }
  }
})
test_that("mlgpd rejects invalid thresholds, shapes and observations", {
  expect_error(mlgpd(c(1, 2, 3), mu = 1), "strictly above")
  expect_error(mlgpd(c(-1, 2, 3), mu = 0), "strictly above")
  expect_error(mlgpd(c(1, 2, 3), mu = Inf), "finite")
  for (k in c(-.51, Inf, NaN)) expect_error(mlgpd(c(1,2,3), xi = k, mu = 0), "at least -0.5")
  expect_error(mlgpd(c(1, 2, 3), iterlim = 1, mu = 0), "converge")
  expect_error(mlgpd(c(1, 2, NA), mu = 0))
})

test_that("gpd: new holds agree with independent multistart reference fits", {
  set.seed(81029)
  id <- "gpd"
  pars <- c("mu","sigma","xi")
  masks <- c(3,7)
  samples <- list(
    extraDistr::rgpd(20, xi=.3),
    extraDistr::rgpd(100, xi=.3),
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

test_that("GPD scale holds validate support and retain free-threshold rejection", {
  for(v in c(0,-1,Inf,NaN)) expect_error(mlgpd(1:5,mu=0,sigma=v),"positive.*finite")
  expect_error(mlgpd(1:5,mu=0,sigma=1,xi=-.5),"support|holds")
  expect_error(mlgpd(1:5,mu=0,sigma=1,iterlim=1),"converge")
  for(h in list(list(sigma=1),list(sigma=1,xi=0)))
    expect_error(do.call(mlgpd,c(list(x=1:5,mu=NULL),h)),"Estimating `mu` is not supported")
  expect_true(is.finite(logLik(mlgpd(1,mu=0,sigma=2))))
})
