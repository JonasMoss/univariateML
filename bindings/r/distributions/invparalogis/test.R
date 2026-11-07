test_that("invparalogis matches a multistart density oracle", {
  skip_unless_tier("full")
  set.seed(871)
  x <- rinvparalogis(n = 300, shape = 1.3, scale = 2.7)
  fit <- mlinvparalogis(x, gradtol=1e-10, iterlim=1000)
  objective <- function(p) -sum(dinvparalogis(x = x, shape = exp(p[1]), scale = exp(p[2]), log = TRUE))
  oracles <- lapply(c(.4,1,3), function(s) optim(rep(log(s), 2), objective, method="L-BFGS-B", lower=c(-5,-5), upper=c(5,5), control=list(factr=1e4,maxit=2000)))
  oracle <- oracles[[which.min(vapply(oracles, function(o) o$value, 0.0))]]
  expect_equal(as.numeric(fit),exp(oracle$par),tolerance=1e-4)
  expect_equal(attr(fit,"logLik"),-oracle$value,tolerance=1e-8)
})
test_that("invparalogis fixed scale matches density oracle", {
  skip_unless_tier("full")
 x<-c(.03,.1,.4,.7,1,2,4,12)
 held<-3.7
 fit<-mlinvparalogis(x,scale=held)
 objective<-function(p) -sum(dinvparalogis(x = x, shape = exp(p), scale = held, log = TRUE))
 oracles<-lapply(c(.2,1,4),function(s) optim(log(s),objective,method="BFGS",control=list(reltol=1e-13)))
 oracle<-oracles[[which.min(vapply(oracles,function(o)o$value,0.0))]]
 expect_equal(unname(fit["shape"]),exp(oracle$par),tolerance=1e-6)
 expect_identical(unname(fit["scale"]),held)
 expect_identical(attr(fit,"fixed"),"scale")
 expect_equal(attr(logLik(fit),"df"),1)
 expect_equal(attr(fit,"logLik"),-oracle$value,tolerance=1e-10)
 expect_equal(dinvparalogis(x = x, shape = 2.3, scale = held, log = TRUE),dparalogis(x = 1/x, shape = 2.3, scale = 1/held, log = TRUE)-2*log(x))
 expect_length(attr(mlinvparalogis(x),"fixed"),0)
 expect_equal(attr(logLik(mlinvparalogis(x)),"df"),2)
 for(bad in c(0,-1,Inf,NaN)) expect_error(mlinvparalogis(x,scale=bad),"positive finite")
 expect_true(is.finite(attr(mlinvparalogis(rep(1,10),scale=held),"logLik")))
})

test_that("inverse paralogistic diagnoses an unrepresentable extreme-range fit", {
  x<-c(1e-300,1,1e300); lx<-log(x)
  # Optimize in (log(shape), shape*log(reciprocal scale)), where the
  # mathematical optimum stays finite although the reported scale cannot.
  objective<-function(p) {
    a<-exp(p[1]); t<--a*lx-p[2]
    soft<-pmax(t,0)+log1p(exp(-abs(t)))
    -(2*length(x)*p[1]-a*sum(lx)-length(x)*p[2]-sum(lx)-(a+1)*sum(soft))
  }
  o<-optim(c(-6,-6),objective,method="BFGS",control=list(reltol=1e-12,maxit=1000))
  expect_gt(abs(o$par[2]/exp(o$par[1])),log(.Machine$double.xmax))
  expect_error(mlinvparalogis(x),"double precision")
})

test_that("constant free-scale invparalogis data have an unbounded likelihood", {
  expect_error(mlinvparalogis(rep(2,5)),"does not exist")
})


test_that("P2 invparalogis holds agree with independent multistart reference likelihoods", {
  skip_unless_tier("full")
  pars <- c(shape=2, scale=1)
  thresholds <- c()
  regular <- setdiff(names(pars), thresholds)
  density <- dinvparalogis
  samples <- lapply(c(3, 20, 100), function(n)
    do.call(qinvparalogis, c(list(p = (seq_len(n)-.5)/n), as.list(pars))))
  samples[[4]] <- rep(samples[[2]][seq(1, 20, 2)], each=2)
  # These samples deliberately include proper limit-model data.
  samples[[5]] <- qexp(((1:20)-.5)/20)
  samples[[6]] <- c(.3,.8,1,1,1)*pars["scale"]
  samples[[7]] <- c(1.1,1.4,2,3,5)*pars["scale"]
  for (x in samples) for (mask in seq_len(2^length(regular)-1)) {
    held <- c(thresholds, regular[as.logical(intToBits(mask)[seq_along(regular)])])
    free <- setdiff(names(pars), held)
    args <- as.list(pars[held])
    fit <- tryCatch(do.call(mlinvparalogis, c(list(x=x),args)), error=identity)
    always <- TRUE
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

test_that("P2 invparalogis validates new held values", {
  for (v in c(0,-1,Inf,NA_real_)) expect_error(mlinvparalogis(c(.2,.3,.5), shape=v),"positive|finite")
})

 test_that("P2 invparalogis held shape fits constant observations", {
   for(args in list(list(shape=2),list(shape=2,scale=3))) {
     f <- do.call(mlinvparalogis,c(list(x=rep(2,5)),args))
     expect_identical(unname(f["shape"]),2)
     expect_true(is.finite(attr(f,"logLik")))
   }
 })

test_that("invparalogis: P3 unfinished optimizer stops", {
  g <- jsonlite::fromJSON(file.path(Sys.getenv("UMVL_DISTRIBUTIONS_DIR"), "invparalogis", "golden.json"))
  x <- g$data
  expect_error(mlinvparalogis(x, iterlim=1, gradtol=1e-10),
               "[Ff]ail|converge|unresolved|exhausted|score check")
})

test_that("invparalogis: P3 held profile exhaustion stops", {
  expect_error(mlinvparalogis(c(.3,.8,1.7,4.2),shape=2,iterlim=1), "[Ff]ail|converge|unresolved|exhausted")
})


test_that("invparalogis: certification stops carry structured witnesses", {
  x <- c(.03,.1,.4,.7,1,2,4,12)
  e <- tryCatch(mlinvparalogis(x,iterlim=1,gradtol=1e-10),error=identity)
  expect_s3_class(e,"univariateML_numerical")
  expect_identical(e$kind,"numerical")
  expect_true(nrow(e$candidates)>=1)
  expect_true(all(c("shape","scale","loglik_lower","loglik_upper") %in% names(e$candidates)))
  expect_true(all(e$candidates$loglik_lower<=e$candidates$loglik_upper))
  e <- tryCatch(mlinvparalogis(rep(2,5)),error=identity)
  expect_s3_class(e,"univariateML_nomle")
  expect_identical(e$kind,"nomle")
  expect_identical(e$loglik_lower,Inf)
})

test_that("reciprocal rounding ties are numerical rather than nonexistence", {
  x <- c(1.9,1.9+.Machine$double.eps)
  expect_false(x[1]==x[2])
  expect_true(1/x[1]==1/x[2])
  e <- tryCatch(mlinvparalogis(x),error=identity)
  expect_s3_class(e,"univariateML_numerical")
  expect_false(inherits(e,"univariateML_nomle"))
})

test_that("invparalogis held-scale competing mode preserves the Jacobian", {
  x <- exp(.001)
  f <- mlinvparalogis(x,scale=1)
  g <- mlparalogis(1/x,scale=1)
  expect_equal(unname(f["shape"]),unname(g["shape"]),tolerance=1e-10)
  expect_equal(attr(f,"logLik"),attr(g,"logLik")-2*log(x),tolerance=1e-10)
})
