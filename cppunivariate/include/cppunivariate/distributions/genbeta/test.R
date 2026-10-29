genbeta_sample <- function(n=500) actuar::qgenbeta(((1:n)-.5)/n,.4,.3,2)
genbeta_ll <- function(x,v) sum(actuar::dgenbeta(x,v[1],v[2],v[3],scale=v[4],log=TRUE))

test_that("genbeta requires the endpoint and validates support and controls", {
  x<-genbeta_sample()
  expect_error(mlgenbeta(x),"likelihood is unbounded.*mlgenbeta")
  for(z in c(0,-1,Inf,NA_real_)) expect_error(mlgenbeta(x,scale=z))
  for(z in c(0,1,2,-1,Inf)) expect_error(mlgenbeta(c(x,z),scale=1))
  expect_error(mlgenbeta(rep(.5,4),scale=1),"constant")
  expect_error(mlgenbeta(x,scale=1,shape1=-1),"positive finite")
  expect_error(mlgenbeta(x,scale=1,iterlim=1),"unresolved")
  expect_error(mlgenbeta(x,scale=1,gradtol=0))
})

test_that("genbeta profiles agree with independent reference-density searches", {
  for(v in list(c(.4,.3,2,1),c(3,2,.7,1),c(.3,4,3,1))) {
    x<-actuar::qgenbeta(((1:300)-.5)/300,v[1],v[2],v[3])
    f<-mlgenbeta(x,scale=1)
    expect_equal(attr(f,"logLik"),genbeta_ll(x,f),tolerance=1e-9)
    for(start in list(c(.1,.2,.1),c(1,1,1),c(10,10,10),v[1:3])) {
      fn<-function(z) {ans<- -genbeta_ll(x,c(exp(z),1));if(is.finite(ans))ans else 1e100}
      o<-optim(log(start),fn,method="Nelder-Mead",control=list(maxit=4000,reltol=1e-12))
      expect_gte(attr(f,"logLik"),-o$value-1e-6)
    }
  }
})

test_that("all held-shape subsets retain the constrained likelihood and degrees of freedom", {
  x<-genbeta_sample(300);truth<-c(shape1=.4,shape2=.3,shape3=2)
  for(mask in 0:7) {
    held<-names(truth)[as.logical(intToBits(mask)[1:3])]
    f<-do.call(mlgenbeta,c(list(x,scale=1),as.list(truth[held])))
    expect_equal(attr(f,"fixed"),c(held,"scale"))
    expect_equal(attr(logLik(f),"df"),3L-length(held))
    expect_equal(as.numeric(f[held]),unname(truth[held]))
    expect_equal(attr(f,"logLik"),genbeta_ll(x,f),tolerance=1e-9)
    expect_gte(attr(f,"logLik"),genbeta_ll(x,c(truth,1))-1e-7)
    free<-setdiff(names(truth),held)
    if(length(free)) {
      fn<-function(z){v<-truth;v[free]<-exp(z);ans<- -genbeta_ll(x,c(v,1));if(is.finite(ans))ans else 1e100}
      o<-optim(log(truth[free]),fn,method=if(length(free)==1) "BFGS" else "Nelder-Mead",control=list(maxit=4000,reltol=1e-12))
      expect_equal(attr(f,"logLik"),-o$value,tolerance=1e-6)
    }
  }
})

test_that("attained beta Kumaraswamy and power members are fitted correctly", {
  u<-((1:400)-.5)/400
  x<-qbeta(u,.4,.3)
  f<-mlgenbeta(x,scale=1,shape3=1)
  expect_equal(as.numeric(f[1:2]),as.numeric(mlbeta(x)),tolerance=1e-6)
  x<-extraDistr::qkumar(u,2,.3)
  f<-mlgenbeta(x,scale=1,shape1=1)
  expect_equal(as.numeric(f[c(3,2)]),as.numeric(mlkumar(x)),tolerance=1e-6)
  x<-u^2
  f<-mlgenbeta(x,scale=1,shape2=1)
  expect_equal(f[[1]]*f[[3]],-1/mean(log(x)),tolerance=1e-12)
  expect_equal(f[[3]],1)
  expect_equal(attr(logLik(f),"df"),1L)
  expect_equal(attr(f,"fixed"),c("shape2","shape3","scale"))
  for(a in c(.1,1,10)) {
    g<-mlgenbeta(x,scale=1,shape1=a,shape2=1)
    expect_equal(g[[1]]*g[[3]],f[[1]],tolerance=1e-12)
  }
})

test_that("face fits include unit gamma and the moving effective endpoint", {
  u<-((1:300)-.5)/300
  x<-exp(-qgamma(u,2,rate=3))
  expect_error(mlgenbeta(x,scale=1),"mllgamma")
  expect_error(mlgenbeta(x,scale=1,shape2=2),"mllgamma")
  x<-.7*u^2
  expect_error(mlgenbeta(x,scale=1),"mlpower")
  g<-mllgamma(1/exp(-qgamma(u,2,rate=3)))
  expect_equal(as.numeric(g),as.numeric(mlgamma(qgamma(u,2,rate=3))),tolerance=1e-8)
})

test_that("genbeta is scale equivariant and seeded random evaluation uses the repaired actuar law", {
  x<-genbeta_sample();f<-mlgenbeta(x,scale=1)
  for(s in c(1e-200,7,1e200)) {
    g<-mlgenbeta(s*x,scale=s)
    expect_equal(as.numeric(g[1:3]),as.numeric(f[1:3]),tolerance=1e-5)
    expect_equal(attr(g,"logLik")+length(x)*log(s),attr(f,"logLik"),tolerance=1e-8)
  }
  set.seed(81);a<-rml(20,f)
  set.seed(81);b<-univariateML_reference_fixes[["actuar::qgenbeta"]]$replacement(runif(20),f[1],f[2],f[3],scale=f[4])
  expect_equal(a,b)
})

test_that("genbeta reference repairs preserve extreme log tails and finite quantiles", {
  d<-univariateML_reference_fixes[["actuar::dgenbeta"]]$replacement
  p<-univariateML_reference_fixes[["actuar::pgenbeta"]]$replacement
  q<-univariateML_reference_fixes[["actuar::qgenbeta"]]$replacement
  # alpha*tau=1 cancels the x-dependent powers exactly near zero.
  expect_equal(d(1e-300,.01,2,100,log=TRUE),log(100)-lbeta(.01,2),tolerance=1e-10)
  expect_equal(p(1e-300,.01,2,100,log.p=TRUE),log(1e-300)-log(.01)-lbeta(.01,2),tolerance=1e-10)
  z<-q(.5,.0001,2,100)
  expect_gt(z,0);expect_lt(z,1)
  expect_equal(p(z,.0001,2,100),.5,tolerance=1e-10)
  for(tail in c(TRUE,FALSE)) for(lp in c(-2,-40,-1000)) {
    z<-q(lp,2,3,.01,lower.tail=tail,log.p=TRUE)
    if(z>0&&z<1)expect_equal(p(z,2,3,.01,lower.tail=tail,log.p=TRUE),lp,tolerance=1e-7)
  }
  expect_equal(d(0,.5,2,2),exp(log(2)-lbeta(.5,2)))
  expect_equal(d(1,2,1,3),6)
  expect_equal(d(.5,1e12,2,1e-12,log=TRUE),log(-log(.5)),tolerance=1e-9)
  expect_equal(p(c(-1,2),2,3,4),c(0,1))
})

test_that("public genbeta dml pml qml dispatch installs tail repairs when needed", {
  f<-mlgenbeta(c(.1,.2),scale=1,shape1=.01,shape2=2,shape3=100)
  expect_equal(dml(1e-300,f,log=TRUE),log(100)-lbeta(.01,2),tolerance=1e-10)
  expect_equal(pml(1e-300,f,log.p=TRUE),log(1e-300)-log(.01)-lbeta(.01,2),tolerance=1e-10)
  g<-mlgenbeta(c(.1,.2),scale=1,shape1=.0001,shape2=2,shape3=100)
  h<-mlgenbeta(c(.1,.2),scale=1,shape1=1e12,shape2=2,shape3=1e-12)
  expect_equal(dml(.5,h,log=TRUE),log(-log(.5)),tolerance=1e-9)
  z<-qml(.5,g)
  expect_gt(z,0)
  expect_equal(pml(z,g),.5,tolerance=1e-10)
})


test_that("genbeta RNG repair preserves the seed during detection and avoids saturation", {
  fix<-univariateML_reference_fixes[["actuar::rgenbeta"]]
  set.seed(81);old<-.Random.seed;fix$broken()
  expect_identical(.Random.seed,old)
  set.seed(81);z<-fix$replacement(20,.0001,2,100)
  expect_equal(anyDuplicated(z),0L)
  set.seed(81);expect_equal(fix$replacement(20,.0001,2,100),z)
  expect_equal(length(fix$replacement(1:7,.4,.3,2)),7L)
})


test_that("P2 genbeta holds agree with independent multistart reference likelihoods", {
  pars <- c(shape1=0.4, shape2=0.3, shape3=2, scale=1)
  thresholds <- c("scale")
  regular <- setdiff(names(pars), thresholds)
  density <- actuar::dgenbeta
  samples <- lapply(c(3, 20, 100), function(n)
    do.call(actuar::qgenbeta, c(list(p = (seq_len(n)-.5)/n), as.list(pars))))
  samples[[4]] <- rep(samples[[2]][seq(1, 20, 2)], each=2)
  # These samples deliberately include proper limit-model data.
  samples[[5]] <- exp(-qgamma(((1:20)-.5)/20,2,rate=3))
  for (x in samples) for (mask in seq_len(2^length(regular)-1)) {
    held <- c(thresholds, regular[as.logical(intToBits(mask)[seq_along(regular)])])
    free <- setdiff(names(pars), held)
    args <- as.list(pars[held])
    fit <- tryCatch(do.call(mlgenbeta, c(list(x=x),args)), error=identity)
    always <- "shape3" %in% held || all(c("shape1","shape2") %in% held)
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

test_that("P2 genbeta constant samples with coercive holds fit", {
  expect_true(is.finite(attr(mlgenbeta(rep(.4,5),scale=1,shape1=2,shape3=1),"logLik")))
  expect_true(is.finite(attr(mlgenbeta(rep(.4,5),scale=1,shape2=3,shape3=1),"logLik")))
  expect_true(is.finite(attr(mlgenbeta(rep(.4,5),scale=1,shape1=2,shape2=3),"logLik")))
  expect_true(is.finite(attr(mlgenbeta(rep(.4,5),scale=1,shape2=1),"logLik")))
})

test_that("genbeta: P3 unfinished optimizer stops", {
  g <- jsonlite::fromJSON(file.path(Sys.getenv("UMVL_DISTRIBUTIONS_DIR"), "genbeta", "golden.json"))
  x <- g$data
  expect_error(mlgenbeta(x, iterlim=1, gradtol=1e-10, scale=1),
               "[Ff]ail|converge|unresolved|exhausted|score check")
})

test_that("genbeta: P3 held profile exhaustion stops", {
  expect_error(mlgenbeta(c(.2,.3,.5,.8),scale=1,shape1=2,shape2=3,iterlim=1), "[Ff]ail|converge|unresolved|exhausted")
})
