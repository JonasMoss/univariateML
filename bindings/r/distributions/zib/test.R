test_that("zib agrees with a multi-start reference-density oracle", {
  skip_unless_tier("full")
  set.seed(12670)
  x <- do.call(rzib, list(250, size = 8, prob = 0.4, pi = 0.4))
  fit <- mlzib(x, size = NULL)
  density <- dzib
  ll <- function(p) sum(do.call(density, c(list(x = x), as.list(p), list(log = TRUE))))
  expect_equal(attr(fit, "logLik"), ll(setNames(as.numeric(fit), names(fit))), tolerance=1e-8)
  oracle <- function(held = NULL) {
    free <- setdiff(names(fit), names(held))
    objective <- function(t) {
      p <- setNames(as.numeric(fit), names(fit)); p[names(held)] <- held
      p[free] <- vapply(seq_along(free), function(j) if(free[j] %in% c("size", "lambda")) exp(t[j]) else plogis(t[j]), numeric(1))
      val <- -ll(p); if(is.finite(val)) val else 1e100
    }
    t0 <- vapply(free, function(v) if(v %in% c("size","lambda")) log(fit[v]) else qlogis(fit[v]), numeric(1))
    min(vapply(c(-.7,0,.7), function(delta) optim(t0+delta,objective,method="BFGS",control=list(maxit=2000,reltol=1e-12))$value,numeric(1)))
  }
  best <- min(vapply(seq.int(max(x), max(80, fit["size"]+20)), function(s) oracle(c(size=s)), numeric(1)))
  expect_gte(attr(fit,"logLik"), -best-1e-6)
  fixed <- mlzib(x, size=15)
  expect_identical(attr(fixed,"fixed"), "size")
  expect_equal(unname(fixed["size"]), 15)
  expect_equal(attr(logLik(fixed),"df"), 2L)
  expect_gte(attr(fixed,"logLik"), -oracle(c(size=15))-1e-6)
  for(bad in c(-1,Inf,NaN,0,.5)) expect_error(mlzib(x,size=bad),"size")
  expect_error(mlzib_(integer(), size=NULL),"nonempty")
  expect_error(mlzib_(c(-1L,1L), size=NULL),"count")
})

test_that("zib declares its infinite-size escape", {
 x <- c(0,0,0,0,0,0,0,0,1,1,1,2,3,7,12)
 expect_error(mlzib(x, size = NULL), "Use `mlpois.*zero = .inflated")
 expect_s3_class(mlzib(x,size=20), "univariateML")
})

test_that("zib preserves fixed size at attained boundaries", {
 zero <- mlzib(c(0,0),size=5)
 expect_equal(unname(zero["size"]),5)
 expect_identical(attr(zero,"fixed"),"size")
 expect_equal(attr(logLik(zero),"df"),2L)
 one <- mlzib(c(1,1),size=5)
 expect_equal(unname(one["size"]),5)
})

test_that("zib accepts fixed zero size for all-zero data", {
 fit <- mlzib(c(0,0),size=0)
 expect_equal(unname(fit["size"]),0)
 expect_identical(attr(fit,"fixed"),"size")
 expect_equal(attr(logLik(fit),"df"),2L)
 expect_equal(attr(fit,"logLik"),0)
})

 test_that("zib: new holds match reference-density integer and multistart oracles", {
  skip_unless_tier("full")
  set.seed(810)
  samples <- c(lapply(c(20,100),function(n) rzib(n = n, size = 8, prob = 0.4, pi = 0.3)),list(c(1,2,2),rep(2,4),c(1,1,2,3)))
  for(x in samples) {
    for(fixed_size in c(TRUE,FALSE)) {
      held <- list(prob=.4)
      for (mixture in c(FALSE,TRUE)) {
      held <- list(prob=.4); if(mixture) held$pi <- .3
      args <- c(list(x=x,size=if(fixed_size) 8 else NULL),held)
      fit <- suppressWarnings(do.call(mlzib,args))
      sizes <- if(fixed_size) 8 else seq(max(x),200)
      ll <- function(size, mixture=.3) sum(dzib(x = x, size = size, prob = 0.4, pi = mixture, log = TRUE))
      oracle <- max(vapply(sizes,function(size) {
        if(mixture) return(ll(size))
        max(c(ll(size,0),ll(size,1),vapply(c(.05,.4,.9),function(start) -optim(start,function(p) -ll(size,p),method="L-BFGS-B",lower=1e-9,upper=1-1e-9)$value,0.)))
      },0.))
      expect_equal(attr(fit,"logLik"),oracle,tolerance=1e-6)
      for(name in names(held)) expect_identical(unname(fit[name]),held[[name]])
      expect_setequal(attr(fit,"fixed"),c(if(fixed_size) "size",names(held)))
      }
    }
  }
  for(p in c(-.1,1.1,NA_real_,Inf)) expect_error(mlzib(c(1,2),size=8,prob=p),"prob")
  expect_error(mlzib(c(1,2),size=8,prob=1))
  expect_error(mlzib(c(1,2),size=NULL,prob=0))
})

test_that("zib: held pi profiles the remaining probability", {
 set.seed(22)
 for(n in c(20,100)) for(x in list(rbinom(n,8,.4),c(rep(0,n/2),rbinom(n/2,8,.4)),c(0,1,2,2,3),rep(0,n))) {
   for(mix in c(0,.2,.8)) {
     f<-suppressWarnings(mlzib(x,size=8,pi=mix))
     ll<-function(p)sum(dzib(x = x, size = 8, prob = p, pi = mix, log = TRUE))
     oracle<-max(c(ll(0),ll(1),vapply(c(.02,.2,.5,.9,.99),function(start) -optim(start,function(p)-ll(p),method="L-BFGS-B",lower=1e-9,upper=1-1e-9)$value,0.)))
     expect_equal(attr(f,"logLik"),oracle,tolerance=1e-6)
     expect_identical(unname(f["pi"]),mix)
   }
 }
 for(v in c(-1,Inf,NA_real_)) expect_error(mlzib(1:3,size=8,pi=v),"pi")
 expect_error(mlzib(1:3,size=8,pi=1))
})

test_that("zib: free-size held-mixture fits and Poisson escapes", {
 for(x in list(c(0,1,2,2,3),c(1,2,3,3,4))) {
  f<-mlzib(x,size=NULL,pi=.2)
  ll<-function(k,p)sum(dzib(x = x, size = k, prob = p, pi = 0.2, log = TRUE))
  oracle<-max(vapply(seq(max(x),50),function(k)max(c(ll(k,0),ll(k,1),vapply(c(.1,.5,.9),function(start)-optim(start,function(p)-ll(k,p),method="L-BFGS-B",lower=1e-8,upper=1-1e-8)$value,0.))),0.))
  expect_equal(attr(f,"logLik"),oracle,tolerance=1e-6)
 }
 expect_error(mlzib(c(1,1,1,2,3,7,12),size=NULL,pi=0),"mlpois.*zero = .inflated")
 f<-mlzib(rep(0,3),size=8,prob=.4,pi=.6)
 expect_identical(unname(f[c("prob","pi")]),c(.4,.6))
 expect_error(mlzib(1:3,size=8,prob=.4,iterlim=0))
})

test_that("zib: P3 unfinished optimizer stops", {
  g <- jsonlite::fromJSON(file.path(Sys.getenv("UMVL_DISTRIBUTIONS_DIR"), "zib", "golden.json"))
  x <- g$data
  expect_error(mlzib(x, iterlim=1, reltol=1e-10, size=NULL),
               "[Ff]ail|converge|unresolved|exhausted|score check")
})

test_that("zib: free size, probability and mixture agree with a reference-density oracle (n=20, n=100, ties, near-Poisson)", {
  skip_unless_tier("full")
  # TASK-85/87: same exponential-grid caveat as ztbinom's analogous test;
  # cross-checks the specific cases the task calls out.
  set.seed(85087)
  oracle_ll <- function(x, size) {
    objective <- function(par) {
      p <- plogis(par[1]); mix <- plogis(par[2])
      val <- -sum(dzib(x = x, size = size, prob = p, pi = mix, log = TRUE))
      if (is.finite(val)) val else 1e100
    }
    best <- Inf
    for (start in list(c(-2,-2), c(0,0), c(2,-2), c(-2,2), c(2,2))) {
      o <- optim(start, objective, method = "BFGS", control = list(reltol = 1e-12, maxit = 2000))
      if (o$value < best) best <- o$value
    }
    # Boundary mixture (pi=0, i.e. plain ztbinom*zero-mass) and pi=1
    # (point mass at zero) are not reached by the logit reparametrization.
    ll0 <- sum(dzib(x = x, size = size, prob = max(mean(x)/size, 1e-09), pi = 0, log = TRUE))
    max(-best, ll0)
  }
  oracle <- function(x, sizes) max(vapply(sizes, function(s) oracle_ll(x, s), 0))

  samples <- list(
    n20 = rzib(n = 20, size = 8, prob = 0.4, pi = 0.3),
    n100 = rzib(n = 100, size = 8, prob = 0.4, pi = 0.3),
    ties = c(0, 0, 2, 2, 2, 2, 3, 4, 5, 5),
    near_poisson = rzib(n = 100, size = 500, prob = 0.02, pi = 0.2)
  )
  for (x in samples) {
    # The near-Poisson sample can legitimately hit the infinite-size
    # escape instead of a finite fit (R6: a data-dependent "use mlzip",
    # not a failure); accept either outcome.
    fit <- tryCatch(suppressWarnings(mlzib(x, size = NULL)), error = identity)
    if (inherits(fit, "error")) {
      expect_match(conditionMessage(fit), "mlpois.*zero = .inflated")
      next
    }
    expect_true(is.finite(attr(fit, "logLik")))
    best <- oracle(x, unique(pmax(max(x), round(unname(fit["size"]) * c(.25,.5,.9,1,1.1,2,4)))))
    expect_gte(attr(fit, "logLik") + 1e-5, best)
    expect_equal(attr(fit, "logLik"),
                 sum(dzib(x = x, size = unname(fit["size"]), prob = unname(fit["prob"]), pi = unname(fit["pi"]), log = TRUE)),
                 tolerance = 1e-8)
  }
})

test_that("zib attained point member evaluates and samples exactly", {
  fit <- mlzib(rep(5,4),size=5,prob=1,pi=0)
  expect_equal(dml(c(4,5),fit),c(0,1))
  expect_equal(pml(c(4,5),fit),c(0,1))
  expect_equal(qml(.5,fit),5)
  expect_equal(rml(10,fit),rep(5,10))
})

test_that("zib all-zero free-size held subsets attain likelihood one", {
  for (p in list(NULL, 0, .5, 1)) for (mix in list(NULL, 0, .2, 1)) {
    f <- mlzib(c(0, 0, 0), size=NULL, prob=p, pi=mix)
    expect_equal(unname(f["size"]), if (is.null(p) && is.null(mix)) 1 else 0)
    expect_equal(unname(f["prob"]), if (is.null(p)) 0 else p)
    expect_equal(unname(f["pi"]), if (is.null(mix)) 0 else mix)
    expect_equal(attr(f, "logLik"), 0)
    expect_equal(sum(dml(c(0, 0, 0), f, log=TRUE)), 0)
    expect_setequal(attr(f, "fixed"), c(character(), if (!is.null(p)) "prob", if (!is.null(mix)) "pi"))
    expect_equal(attr(logLik(f), "df"), 3L - as.integer(!is.null(p)) - as.integer(!is.null(mix)))
  }
  f <- mlzib(c(0, 0, 0), size=NULL, prob=.5, pi=.2)
  expect_gt(attr(f, "logLik"), 3*log(.6))
})

test_that("zib J7a held inflation uses a constrained finite certificate", {
  x <- c(rep(0,5),1,2,2,3,3,3,4,4,5,5)
  f <- mlzib(x,size=NULL,pi=.2)
  expect_equal(unname(f["size"]),6)
  e <- tryCatch(mlzib(c(rep(0,8),1,1,1,2,3,7,12),size=NULL,pi=.2),error=identity)
  expect_s3_class(e,"univariateML_limit")
  expect_true(is.finite(e$loglik_upper))
})


test_that("zib proved held-inflation faces stop as limits", {
  e <- tryCatch(mlzib(c(rep(0,8),1,1,1,2,3,7,12),size=NULL,pi=.2),error=identity)
  expect_identical(e$kind,"limit")
  expect_identical(class(e)[1],"univariateML_limit")
})

test_that("zib validates tiny probabilities before selecting the inflation branch", {
  x <- c(rep(0,4),1:4)
  e <- tryCatch(mlzib(x,size=1e100),error=identity)
  expect_s3_class(e,"univariateML_numerical")
  expect_identical(e$kind,"numerical")
  expect_equal(nrow(e$candidates),1)
  expect_equal(e$loglik_upper,Inf)
  f <- mlzib(x,size=1e100,iterlim=1000)
  face <- mlzip(x)
  expect_equal(as.numeric(f["prob"])*1e100,as.numeric(face["lambda"]),tolerance=1e-9)
  expect_equal(as.numeric(f["pi"]),as.numeric(face["pi"]),tolerance=1e-9)
})

test_that("zib signed faces preserve the inflation constraint", {
  x <- c(rep(0,8),1,1,1,2,3,7,12)
  for (mix in list(NULL,0)) {
    e <- tryCatch(mlzib(x,size=NULL,pi=mix),error=identity)
    expect_s3_class(e,"univariateML_limit")
    expect_identical(e$kind,"limit")
    expect_true(is.finite(e$loglik_upper))
    expect_identical(e$model,"zip")
    face <- if (is.null(mix)) mlzip(x) else mlzip(x,pi=mix)
    expect_equal(e$limit_loglik,attr(face,"logLik"),tolerance=1e-9)
  }
  positive <- c(1,1,1,2,3,7,12)
  a <- tryCatch(mlzib(positive,size=NULL,pi=.2),error=identity)
  b <- tryCatch(mlzib(positive,size=NULL),error=identity)
  expect_s3_class(a,"univariateML_limit")
  expect_s3_class(b,"univariateML_limit")
  expect_equal(a$limit_loglik,b$limit_loglik+length(positive)*log(.8),tolerance=1e-9)
})

test_that("zib localizes a held face when the inflation-uniform gap is insufficient", {
  e <- tryCatch(mlzib(c(rep(0,5),1,1,2,2,4),size=NULL,pi=.2),error=identity)
  expect_s3_class(e,"univariateML_limit")
  expect_identical(e$kind,"limit")
  expect_true(is.finite(e$loglik_upper))
  expect_true(is.finite(e$loglik_lower))
})

test_that("zib near-zero local face coefficients return an uncertified finite fit", {
  q <- exp(-1.6)
  f <- mlzib(c(rep(0,5),1,1,2,2,4),size=NULL,pi=.75*q/(.25+.75*q))
  expect_identical(attr(f,"certified"),FALSE)
  expect_true(is.finite(as.numeric(logLik(f))))
})

test_that("zib proves escape even when the winning ZIP rate is tied", {
  e <- tryCatch(mlzib(c(rep(0,198),1,19),size=NULL,pi=.7254971052291191),error=identity)
  expect_s3_class(e,"univariateML_tie")
  expect_identical(e$kind,"tie")
  expect_identical(e$model,"zip")
  expect_true(is.finite(e$loglik_upper))
  peaks <- e$candidates[e$candidates$model=="zip",]
  expect_equal(nrow(peaks),2)
  expect_equal(sort(peaks$lambda),c(.5223041246623353,9.982727928366345),tolerance=1e-9)
  expect_gte(nrow(e$candidates),3)
})

test_that("zib certifies noninflated faces including equal mean and variance", {
  for (x in list(c(0,1,1,1,2,4),c(0,1,1,1,2,3,4,4))) {
    e <- tryCatch(mlzib(x,size=NULL),error=identity)
    expect_s3_class(e,"univariateML_limit")
    expect_identical(e$kind,"limit")
    expect_true(is.finite(e$loglik_upper))
    expect_equal(unname(e$estimates["pi"]),0)
    expect_equal(e$limit_loglik,attr(mlzip(x),"logLik"),tolerance=1e-9)
  }
})

test_that("zib retains subnormal zero masses and resolves the held-inflation root", {
  mix <- .Machine$double.xmin*.Machine$double.eps
  x <- c(0,500)
  f <- mlzib(x,size=1075,prob=.5,pi=mix)
  expect_equal(attr(f,"logLik"),sum(dml(x,f,log=TRUE)),tolerance=1e-10)
  g <- mlzib(c(rep(535,1000),0),size=1075,pi=mix)
  expect_equal(unname(g["prob"]),.4971795798757613128165584136,tolerance=1e-10)
  expect_equal(attr(g,"logLik"),-4455.659181549239638934,tolerance=1e-10)
  tiny <- mlzib(0,size=1,prob=1e-300,pi=.5)
  expect_lt(attr(tiny,"logLik"),0)
  expect_equal(as.numeric(attr(tiny,"logLik"))/(-5e-301),1,tolerance=1e-10)
})

test_that("zib native binding reports finite log-likelihood overflow numerically", {
  # Exercise the adapter independently of the public median-evaluation guard.
  e <- getFromNamespace("mlzib_","univariateML")(0L,size=1e308,prob=.9,pi=0)
  expect_s3_class(e,"univariateML_numerical")
  expect_identical(e$kind,"numerical")
  expect_equal(e$loglik_upper,Inf)
})

test_that("zib preserves the unique all-zero inflation even for a tiny held probability", {
  f <- mlzib(0,size=1,prob=1e-300)
  expect_equal(unname(f["pi"]),1)
  expect_equal(as.numeric(attr(f,"logLik")),0)
})

test_that("zib fixed models surface failed median evaluations", {
  expect_error(mlzib(0,size=1e308,prob=.9,pi=0),
               "median quantile evaluation failed.*numerical failure",
               class="univariateML_evaluation_error")
  expect_error(mlzib(size=1e308,prob=.9,pi=0),
               "median quantile evaluation failed.*numerical failure",
               class="univariateML_evaluation_error")
  expect_equal(as.numeric(qml(.5,mlzib(size=1e308,prob=1,pi=0))),1e308)
})


test_that("zib log half respects the exact side of a two-point jump", {
  model <- mlzib(size=10,prob=1,pi=.5)
  expect_equal(qml(.5,model),0)
  expect_equal(qml(log(.5),model,log.p=TRUE),10)
  expect_equal(qml(log(.5),model,lower.tail=FALSE,log.p=TRUE),0)
})

test_that("zib returns uncertified fits at both adjacent critical inflations", {
  x <- c(0,0,0,0,0,1,1,2,2,4)
  for (pi in c(.3772146069463776,.37721460694637765)) {
    f <- mlzib(x,size=NULL,pi=pi)
    expect_identical(attr(f,"certified"),FALSE)
    expect_equal(unname(f["pi"]),pi,tolerance=0)
    expect_true(is.finite(as.numeric(logLik(f))))
  }
})

test_that("zib exposes the uncertified held-probability prefix", {
  f <- mlzib(0:3,size=NULL,prob=1e-9,iterlim=1)
  expect_identical(attr(f,"certified"),FALSE)
  expect_equal(unname(f["prob"]),1e-9,tolerance=0)
  expect_true(is.finite(as.numeric(logLik(f))))
})

test_that("zib retains a stationary fit at an ambiguous turning discriminant", {
  f <- mlzib(c(rep(0,34000),rep(1,4000),rep(2,30000)),size=2,pi=.2)
  expect_identical(attr(f,"certified"),FALSE)
  expect_true(is.finite(as.numeric(logLik(f))))
  expect_equal(unname(f[c("size","pi")]),c(2,.2),tolerance=0)
})

test_that("large held counts return the successful zib prefix uncertified", {
  f <- mlzib(c(1000000,1000001,1000003,1000008,1000011),size=NULL,prob=.4)
  expect_identical(attr(f,"certified"),FALSE)
  expect_equal(unname(f["prob"]),.4,tolerance=0)
  expect_true(is.finite(as.numeric(logLik(f))))
})

test_that("an exact turning discriminant leaves a flat probability root uncertified", {
  f <- mlzib(c(rep(0,17),rep(1,2),rep(2,15)),size=2,pi=54/343)
  expect_identical(attr(f,"certified"),FALSE)
  expect_true(is.finite(as.numeric(logLik(f))))
})

test_that("a closed binomial mean respects the requested coefficient precision", {
  e <- tryCatch(mlzib(c(1,1,2),size=2,pi=0,reltol=1e-18),error=identity)
  expect_s3_class(e,"univariateML_numerical")
  expect_match(conditionMessage(e),"numerical accuracy failure")
  expect_true(any(is.finite(e$candidates$loglik_lower)))
  g <- tryCatch(mlzib(c(0,1,1),size=1,prob=1,reltol=1e-18),error=identity)
  expect_s3_class(g,"univariateML_numerical")
  expect_equal(tail(g$candidates$pi,1),1/3,tolerance=0)
})

test_that("uncertified zib coefficients meet the local accuracy contract", {
  f <- mlzib(c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2),size=2,pi=0.15743440233236153)
  expect_identical(attr(f,"certified"),FALSE)
  expect_equal(unname(f["size"]),2,tolerance=0)
  expect_lte(abs(unname(f["prob"])-(0.8235304158971448)),1e-10*1)
  expect_equal(unname(f["pi"]),0.15743440233236153,tolerance=0)
})

test_that("ZIB reports the attainable tolerance at a rational maximizer", {
  e <- tryCatch(mlzib(c(0,1,1),size=1,pi=0,reltol=1e-20),error=identity)
  expect_s3_class(e,"univariateML_tolerance")
  expect_s3_class(e,"univariateML_numerical")
  expect_identical(e$kind,"tolerance")
  expect_equal(e$attainable_tol,1/27021597764222976,tolerance=1e-30)
  expect_match(conditionMessage(e),"tol must be at least .* here")
  expect_true(any(e$candidates$prob==2/3))
})
