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
  do.call(getExportedValue("extraDistr", paste0("d",id)), c(list(x=x,log=log),a))
}

test_that("frechet: held location, attained shape boundary and escape", {
 x <- extraDistr::qfrechet((1:100-.5)/100,lambda=.7,mu=2,sigma=3)
 f <- suppressWarnings(mlfrechet(x,mu=2), classes = "univariateML_restricted")
 b <- mlinvweibull(x-2,reltol=1e-10)
 expect_equal(unname(f["lambda"]),unname(b["shape"]),tolerance=1e-10)
 expect_equal(unname(f["sigma"]),1/unname(b["rate"]),tolerance=1e-10)
 expect_equal(attr(f,"logLik"),attr(b,"logLik"),tolerance=1e-10)
 free <- suppressWarnings(mlfrechet(x, mu = NULL), classes = "univariateML_restricted")
 expect_equal(unname(free["lambda"]),1)
 expect_error(suppressWarnings(mlfrechet(c(1,1,2), mu = NULL), classes = "univariateML_restricted"),"tie")
 expect_error(suppressWarnings(mlfrechet(1:5,mu=1), classes = "univariateML_restricted"),"strictly")
 expect_error(suppressWarnings(mlfrechet(1:5,mu=Inf), classes = "univariateML_restricted"),"finite")
 expect_error(suppressWarnings(mlfrechet(rep(1,5),mu=0), classes = "univariateML_restricted"),"nonconstant")
 expect_error(suppressWarnings(mlfrechet(x,iterlim=1, mu = NULL), classes = "univariateML_restricted"),"converge")
 expect_error(suppressWarnings(mlfrechet(x,mu=2,iterlim=1), classes = "univariateML_restricted"),"converge")
 expect_error(suppressWarnings(mlfrechet(extraDistr::qgumbel((1:100-.5)/100), mu = NULL), classes = "univariateML_restricted"),"mlgumbel")
 expect_equal(sum(dml(x,f,log=TRUE)),attr(f,"logLik"),tolerance=1e-10)
 set.seed(12); a<-rml(20,f);set.seed(12);expect_identical(rml(20,f),a)
 for(shape in c(.5,1,3)) {
   y <- extraDistr::qfrechet((1:80-.5)/80,lambda=shape)
   fit <- suppressWarnings(mlfrechet(y, mu = NULL), classes = "univariateML_restricted")
   objective <- function(z) -sum(extraDistr::dfrechet(y,lambda=1+exp(z[1]),mu=min(y)-exp(z[2]),sigma=exp(z[3]),log=TRUE))
   for(delta in c(-.3,0,.3)) {
     start <- c(log(max(fit["lambda"]-1,1e-6)),log(min(y)-fit["mu"]),log(fit["sigma"]))
     o<-optim(start+delta,objective,method="Nelder-Mead",control=list(maxit=3000,reltol=1e-12))
     expect_lte(-o$value,attr(fit,"logLik")+1e-6*length(y))
   }
 }
})

test_that("frechet and fpareto log-tail repairs remain finite", {
 source_fun <- univariateML:::univariateML_reference_fixes
 q <- source_fun[["extraDistr::qfrechet"]]$replacement(-1000,2,lower.tail=FALSE,log.p=TRUE)
 expect_equal(log(q),500)
 p <- source_fun[["extraDistr::pfrechet"]]$replacement(q,2,lower.tail=FALSE,log.p=TRUE)
 expect_equal(p,-1000)
 q <- source_fun[["actuar::qfpareto"]]$replacement(-40,0,.3,.7,.4,lower.tail=FALSE,log.p=TRUE)
 expect_true(is.finite(q))
 expect_equal(actuar::pfpareto(q,0,.3,.7,.4,lower.tail=FALSE,log.p=TRUE),-40,tolerance=1e-12)
})

test_that("frechet: reference mapping and unbounded unrestricted path", {
 x <- c(.1,.5,1,2,10,100)
 expect_lt(max(abs((extraDistr::dfrechet(x,2,0,2,log=TRUE))-(actuar::dinvweibull(x,2,scale=2,log=TRUE)))),1e-12)
 y <- c(1,2,4,7)
 ll <- sapply(c(1e-3,1e-6,1e-9),function(e) sum(extraDistr::dfrechet(y,.1,1-e,e,log=TRUE)))
 expect_true(all(diff(ll)>0))
})

test_that("frechet: an independent Gumbel-face oracle finds no better member", {
 y <- extraDistr::qgumbel((1:80-.5)/80)
 g <- mlgumbel(y)
 nll <- function(z) {
   if(any(!is.finite(z)) || any(abs(z)>500) || z[1]>log(1e5)) return(1e100)
   ll <- sum(extraDistr::dfrechet(y,lambda=1+exp(z[1]),
     mu=min(y)-exp(z[2]),sigma=exp(z[3]),log=TRUE))
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
 fixes <- univariateML:::univariateML_reference_fixes
 q <- fixes[["extraDistr::qfrechet"]]$replacement(-100,.1,sigma=1e-300,
   lower.tail=FALSE,log.p=TRUE)
 expect_equal(log(q),log(1e-300)+1000,tolerance=1e-12)
 expect_equal(fixes[["extraDistr::pfrechet"]]$replacement(q,.1,sigma=1e-300,
   lower.tail=FALSE,log.p=TRUE),-100,tolerance=1e-12)
 q <- fixes[["actuar::qfpareto"]]$replacement(-40,0,.1,.4,.4,scale=1e-300,
   lower.tail=FALSE,log.p=TRUE)
 expect_equal(pbeta(plogis(-.4*(log(q)-log(1e-300))),.1,.4,log.p=TRUE),
   -40,tolerance=1e-12)
})

test_that("Frechet free location accepts ties above the minimum", {
  x <- c(0,1,2,2,2)
  f <- tryCatch(suppressWarnings(mlfrechet(x, mu = NULL), classes = "univariateML_restricted"), error=identity)
  if (inherits(f,"error")) expect_match(conditionMessage(f), "mlgumbel")
  else expect_equal(as.numeric(logLik(f)), sum(extraDistr::dfrechet(x,f[1],f[2],f[3],log=TRUE)), tolerance=1e-8)
})

test_that("frechet: new holds agree with independent multistart reference fits", {
  set.seed(81029)
  id <- "frechet"
  pars <- c("lambda","mu","sigma")
  masks <- c(1,4,3,5,6,7)
  samples <- list(
    extraDistr::rfrechet(20, lambda=2),
    extraDistr::rfrechet(100, lambda=2),
    c(.2,.5,.5,1,2,3,4), c(.2,.8),
    if(id=="frechet") extraDistr::qgumbel((1:20-.5)/20) else rexp(20),
    if(id=="gev") extraDistr::qgev((1:20-.5)/20,xi=-.9) else runif(20,.1,2))
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
