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

test_that("frechet: held location, attained shape boundary and escape", {
 x <- qfrechet(p = (1:100 - 0.5)/100, lambda = 0.7, mu = 2, sigma = 3)
 f <- suppressWarnings(mlfrechet(x,mu=2), classes = "univariateML_restricted")
 b <- mlinvweibull(x-2,reltol=1e-10)
 expect_equal(unname(f["lambda"]),unname(b["shape"]),tolerance=1e-10)
 expect_equal(unname(f["sigma"]),unname(b["scale"]),tolerance=1e-10)
 expect_equal(attr(f,"logLik"),attr(b,"logLik"),tolerance=1e-10)
 free <- suppressWarnings(mlfrechet(x, mu = NULL), classes = "univariateML_restricted")
 expect_equal(unname(free["lambda"]),1)
 expect_error(suppressWarnings(mlfrechet(c(1,1,2), mu = NULL), classes = "univariateML_restricted"),"tie")
 expect_error(suppressWarnings(mlfrechet(1:5,mu=1), classes = "univariateML_restricted"),"strictly")
 expect_error(suppressWarnings(mlfrechet(1:5,mu=Inf), classes = "univariateML_restricted"),"finite")
 expect_error(suppressWarnings(mlfrechet(rep(1,5),mu=0), classes = "univariateML_restricted"),"nonconstant")
 expect_error(suppressWarnings(mlfrechet(x,iterlim=1, mu = NULL), classes = "univariateML_restricted"),"converge")
 expect_error(suppressWarnings(mlfrechet(x,mu=2,iterlim=1), classes = "univariateML_restricted"),"converge")
 expect_error(suppressWarnings(mlfrechet(qgumbel(p = (1:100 - 0.5)/100, mu = 0, sigma = 1), mu = NULL), classes = "univariateML_restricted"),"mlgumbel")
 expect_equal(sum(dml(x,f,log=TRUE)),attr(f,"logLik"),tolerance=1e-10)
 set.seed(12); a<-rml(20,f);set.seed(12);expect_identical(rml(20,f),a)
 for(shape in c(.5,1,3)) {
   y <- qfrechet(p = (1:80 - 0.5)/80, lambda = shape, mu = 0, sigma = 1)
   fit <- suppressWarnings(mlfrechet(y, mu = NULL), classes = "univariateML_restricted")
   objective <- function(z) -sum(dfrechet(x = y, lambda = 1 + exp(z[1]), mu = min(y) - exp(z[2]), sigma = exp(z[3]), log = TRUE))
   for(delta in c(-.3,0,.3)) {
     start <- c(log(max(fit["lambda"]-1,1e-6)),log(min(y)-fit["mu"]),log(fit["sigma"]))
     o<-optim(start+delta,objective,method="Nelder-Mead",control=list(maxit=3000,reltol=1e-12))
     expect_lte(-o$value,attr(fit,"logLik")+1e-6*length(y))
   }
 }
})

test_that("frechet and fpareto log-tail repairs remain finite", {
 q <- qfrechet(-1000,2,mu=0,sigma=1,lower.tail=FALSE,log.p=TRUE)
 expect_equal(log(q),500)
 p <- pfrechet(q,2,mu=0,sigma=1,lower.tail=FALSE,log.p=TRUE)
 expect_equal(p,-1000)
 q <- qfpareto(-40,shape1=.3,shape2=.7,shape3=.4,min=0,scale=1,lower.tail=FALSE,log.p=TRUE)
 expect_true(is.finite(q))
 expect_equal(pfpareto(q = q, shape1 = 0.3, shape2 = 0.7, shape3 = 0.4, scale = 1/1, min = 0, lower.tail = FALSE, log.p = TRUE),-40,tolerance=1e-12)
})

test_that("frechet: reference mapping and unbounded unrestricted path", {
 x <- c(.1,.5,1,2,10,100)
 expect_lt(max(abs((dfrechet(x = x, lambda = 2, mu = 0, sigma = 2, log = TRUE))-(dinvweibull(x = x, shape = 2, scale = 2, log = TRUE)))),1e-12)
 y <- c(1,2,4,7)
 ll <- sapply(c(1e-3,1e-6,1e-9),function(e) sum(dfrechet(x = y, lambda = 0.1, mu = 1 - e, sigma = e, log = TRUE)))
 expect_true(all(diff(ll)>0))
})

test_that("frechet: an independent Gumbel-face oracle finds no better member", {
  skip_unless_tier("full")
 y <- qgumbel(p = (1:80 - 0.5)/80, mu = 0, sigma = 1)
 g <- mlgumbel(y)
 nll <- function(z) {
   if(any(!is.finite(z)) || any(abs(z)>500) || z[1]>log(1e5)) return(1e100)
   ll <- sum(dfrechet(x = y, lambda = 1 + exp(z[1]), mu = min(y) - exp(z[2]), sigma = exp(z[3]), log = TRUE))
   if(is.finite(ll)) -ll else 1e100
 }
 for(lambda in c(3,30,300)) {
   sf <- as.numeric(g["sigma"])*lambda
   start <- c(log(lambda-1),log(min(y)-as.numeric(g["mu"])+sf),log(sf))
   o <- optim(start,nll,method="Nelder-Mead",control=list(maxit=2000,reltol=1e-12))
   expect_lte(-o$value,attr(g,"logLik")+1e-6*length(y))
 }
 expect_error(suppressWarnings(mlfrechet(y, mu = NULL), classes = "univariateML_restricted"),"mlgumbel")
})

test_that("threshold tail repairs retain finite answers under extreme rescaling", {
 q <- qfrechet(-100,.1,mu=0,sigma=1e-300,
   lower.tail=FALSE,log.p=TRUE)
 expect_equal(log(q),log(1e-300)+1000,tolerance=1e-12)
 expect_equal(pfrechet(q,.1,mu=0,sigma=1e-300,
   lower.tail=FALSE,log.p=TRUE),-100,tolerance=1e-12)
 q <- qfpareto(-40,shape1=.1,shape2=.4,shape3=.4,min=0,scale=1e-300,
   lower.tail=FALSE,log.p=TRUE)
 expect_equal(pbeta(plogis(-.4*(log(q)-log(1e-300))),.1,.4,log.p=TRUE),
   -40,tolerance=1e-12)
})

test_that("Frechet free location accepts ties above the minimum", {
  x <- c(0,1,2,2,2)
  f <- tryCatch(suppressWarnings(mlfrechet(x, mu = NULL), classes = "univariateML_restricted"), error=identity)
  if (inherits(f,"error")) expect_match(conditionMessage(f), "mlgumbel")
  else expect_equal(as.numeric(logLik(f)), sum(dfrechet(x = x, lambda = f[1], mu = f[2], sigma = f[3], log = TRUE)), tolerance=1e-8)
})

test_that("Frechet free fit compares half-minimum ties with the free collapse face", {
  # TASK-85/87: with mu, lambda and sigma all free, this used to be
  # rejected outright. Mirrors gev/test.R's identical test and finding
  # (dev/research/evaluations/2026-10-29-tail85-report.md): every sample tried
  # gives the collapse-face branch, not the interior-fit one.
  for(x in list(c(1,1,2,3),c(0,0,.01,100),c(1,1,1,2,5,20))) {
    f <- tryCatch(suppressWarnings(mlfrechet(x, mu = NULL), classes = "univariateML_restricted"), error=identity)
    if (inherits(f,"error")) expect_match(conditionMessage(f), "scale collapse")
    else {
      m <- min(x)
      face <- length(x)*log(2)-length(x)-2*sum(log(x[x>m]-m))
      expect_gt(as.numeric(logLik(f)),face)
    }
  }
  # Strict majority (k=3 of n=4) still diverges outright, unaffected.
  expect_error(suppressWarnings(mlfrechet(c(1,1,1,2), mu = NULL), classes = "univariateML_restricted"))
})

test_that("frechet: new holds agree with independent multistart reference fits", {
  skip_unless_tier("full")
  set.seed(81029)
  id <- "frechet"
  pars <- c("lambda","mu","sigma")
  masks <- c(1,4,3,5,6,7)
  samples <- list(
    rfrechet(n = 20, lambda = 2, mu = 0, sigma = 1),
    rfrechet(n = 100, lambda = 2, mu = 0, sigma = 1),
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

test_that("Frechet holds diagnose collapse, constant shape divergence and domains", {
  for(v in c(0,-1,Inf,NaN)) {
    expect_error(suppressWarnings(mlfrechet(1:5,mu=0,lambda=v), classes = "univariateML_restricted"),"positive.*finite")
    expect_error(suppressWarnings(mlfrechet(1:5,mu=0,sigma=v), classes = "univariateML_restricted"),"positive.*finite")
  }
  expect_error(suppressWarnings(mlfrechet(1:5,mu=1,lambda=2), classes = "univariateML_restricted"),"strictly")
  expect_error(suppressWarnings(mlfrechet(c(1,1,2,3),mu=NULL,lambda=1), classes = "univariateML_restricted"),"collapse")
  expect_error(suppressWarnings(mlfrechet(rep(2,4),mu=NULL,sigma=1), classes = "univariateML_restricted"),"unbounded")
  expect_error(suppressWarnings(mlfrechet(rep(2,4),mu=0,sigma=2), classes = "univariateML_restricted"),"unbounded")
  expect_error(suppressWarnings(mlfrechet(1:5,mu=0,sigma=1,iterlim=1), classes = "univariateML_restricted"),"converge")
  for(h in list(list(mu=0,lambda=.5),list(mu=NULL,lambda=.5,sigma=1),list(mu=0,sigma=1))) {
    f <- do.call(mlfrechet,c(list(x=2),h))
    expect_true(is.finite(logLik(f)))
  }
  # All held is an evaluation, including a single point at the scale.
  expect_equal(as.numeric(logLik(suppressWarnings(mlfrechet(2,mu=0,lambda=.5,sigma=2), classes = "univariateML_restricted"))),log(.5/2)-1)
})

test_that("Frechet fixed shape rejects a constant sample with both scales free", {
  expect_error(suppressWarnings(mlfrechet(rep(2,4),mu=NULL,lambda=2), classes = "univariateML_restricted"),"collapse")
})

test_that("Frechet tiny held shape retains its exact scale limit", {
  f <- suppressWarnings(mlfrechet(c(1,4),mu=0,lambda=1e-20), classes = "univariateML_restricted")
  expect_equal(unname(f["sigma"]),2,tolerance=1e-14)
  expect_identical(unname(f["lambda"]),1e-20)
})

test_that("Frechet fixed shape certificate stops carry typed details", {
  e <- tryCatch(mlfrechet(c(0,.2,.5,1,3,20),mu=NULL,lambda=2,iterlim=1),error=identity)
  expect_s3_class(e,"univariateML_numerical")
  expect_gt(nrow(e$candidates),0)
  expect_identical(e$loglik_upper,Inf)
  e <- tryCatch(mlfrechet(c(0,0,1,2),mu=NULL,lambda=1),error=identity)
  expect_s3_class(e,"univariateML_nomle")
  expect_equal(e$loglik_upper,4*log(2)-4-2*log(2))
})

test_that("Frechet collapse supremum remains finite for an overflowing range", {
  e <- tryCatch(mlfrechet(c(-1e308,1e308),mu=NULL,lambda=1),error=identity)
  expect_s3_class(e,"univariateML_nomle")
  expect_equal(e$loglik_upper,-2-2*log(1e308),tolerance=1e-12)
})

test_that("Frechet positive-exponent range overflow is numerical", {
  e <- tryCatch(mlfrechet(c(-1e308,1e308),mu=NULL,lambda=2),error=identity)
  expect_s3_class(e,"univariateML_numerical")
})

test_that("Frechet rejects an unresolved enormous held power", {
  e <- tryCatch(mlfrechet(c(0,.2,.5,1,3,20),mu=NULL,lambda=1e100),error=identity)
  expect_s3_class(e,"univariateML_numerical")
})

test_that("Frechet half-minimum pair certificate stops for equal and unequal upper values", {
  for(x in list(c(0,0,1,2),c(0,0,2,2))) {
    e <- tryCatch(mlfrechet(x,mu=NULL),error=identity)
    expect_s3_class(e,"univariateML_nomle")
  }
})

test_that("Frechet all-free two-point face comparison is certified and typed", {
  x <- c(rep(0,9),rep(1,11))
  f <- suppressWarnings(mlfrechet(x,mu=NULL),classes="univariateML_restricted")
  expect_equal(unname(f["lambda"]),1)
  expect_equal(unname(f["sigma"]),.1,tolerance=1e-10)
  e <- tryCatch(mlfrechet(c(0,rep(1,19)),mu=NULL),error=identity)
  expect_s3_class(e,"univariateML_limit")
  expect_identical(e$model,"gumbel")
  expect_true(all(c("mu","sigma") %in% names(e$estimates)))
  expect_lt(e$loglik_upper-e$loglik_lower,1e-7)
  e <- tryCatch(mlfrechet(c(0,rep(1,19)),mu=NULL,iterlim=1),error=identity)
  expect_s3_class(e,"univariateML_numerical")
})

test_that("Frechet compatible public scale keeps the certified member", {
  f <- suppressWarnings(mlfrechet(c(rep(0,7),rep(1,9)),mu=NULL,sigma=.125),classes="univariateML_restricted")
  expect_equal(unname(f["sigma"]),.125)
  expect_equal(unname(f["lambda"]),1)
})

test_that("Frechet two-point solver stops preserve finite supremum bounds", {
  e <- tryCatch(mlfrechet(c(0,rep(1,19)),mu=NULL,iterlim=10),error=identity)
  expect_s3_class(e,"univariateML_numerical")
  expect_true(is.finite(e$loglik_upper))
  expect_gt(nrow(e$candidates),0)
})

test_that("Frechet compatible hold failures retain constrained witnesses", {
  e <- tryCatch(mlfrechet(c(rep(0,7),rep(1,9)),mu=NULL,sigma=.125,reltol=1e-15),error=identity)
  expect_s3_class(e,"univariateML_numerical")
  e <- tryCatch(mlfrechet(c(0,rep(1,15)),mu=NULL,sigma=.875,iterlim=10),error=identity)
  expect_s3_class(e,"univariateML_numerical")
  expect_equal(e$loglik_lower,max(e$candidates$loglik_lower))
  expect_true(all(e$candidates$sigma==.875))
})

test_that("general Frechet certificate reports a proper Gumbel closure maximum", {
  x <- -log(-log(((1:80)-.5)/80))
  e <- tryCatch(mlfrechet(x,mu=NULL),error=identity)
  expect_s3_class(e,"univariateML_limit")
  expect_equal(e$model,"gumbel")
  expect_true(is.finite(e$loglik_upper))
  expect_true(e$loglik_lower<=e$loglik_upper)
  expect_true(e$estimates[["sigma"]]>0)
})

test_that("all-free Frechet minimum majorities are statistical nonexistence stops", {
  e <- tryCatch(mlfrechet(c(0,0,0,1,2),mu=NULL),error=identity)
  expect_s3_class(e,"univariateML_nomle")
  expect_match(conditionMessage(e),"minimum majority")
})

test_that("held-location inverse-Weibull restoration preserves certification", {
  x <- 1 + (0:4) * .Machine$double.eps
  fit <- mlfrechet(x, mu = 0, reltol = 1.220703e-4)
  expect_false(attr(fit, "certified"))
  expect_true(is.finite(as.numeric(logLik(fit))))
})
