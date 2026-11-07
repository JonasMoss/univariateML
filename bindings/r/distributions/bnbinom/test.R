context("mlbnbinom")

# Independent exact lgamma/lbeta oracle, on aggregated counts. This deliberately
# uses neither the C++ objective nor its optimisation algorithm. Beyond moderate
# shapes gamma subtraction loses accuracy; fit the actual faces separately.
bnbinom_exact <- function(x, p) {
  lgamma(p[1]+x)-lgamma(p[1])-lgamma(x+1)+
    lbeta(p[2]+p[1],p[3]+x)-lbeta(p[2],p[3])
}
bnbinom_oracle <- function(x, fixed=list()) {
  tab <- table(x); values <- as.numeric(names(tab)); counts <- as.numeric(tab)
  free <- setdiff(c("size","alpha","beta"),names(fixed))
  objective <- function(t) {
    p <- c(size=1,alpha=1,beta=1); p[names(fixed)] <- unlist(fixed)
    p[free] <- exp(t)
    z <- -sum(counts*bnbinom_exact(values,p))
    if (is.finite(z)) z else 1e100
  }
  if (!length(free)) return(-objective(numeric()))
  starts <- expand.grid(size=c(.3,2,12),alpha=c(.5,3),beta=c(.5,6))
  best <- Inf
  for (i in seq_len(nrow(starts))) {
    start <- log(as.numeric(starts[i,free,drop=FALSE]))
    fit <- nlminb(start,objective,lower=rep(-12,length(free)),upper=rep(12,length(free)),
                  control=list(iter.max=1500,eval.max=3000,rel.tol=1e-12))
    best <- min(best,fit$objective)
  }
  -best
}

test_that("closed-form pmf agrees with the internal density and its mirror", {
  for (p in list(c(.4,.6,3),c(1,2,5),c(4,12,2))) {
    x <- 0:60
    expect_equal(bnbinom_exact(x,p),dbnbinom(x = x, size = p[1], alpha = p[2], beta = p[3], log = TRUE),tolerance=1e-11)
    expect_equal(bnbinom_exact(x,p),bnbinom_exact(x,p[c(3,2,1)]),tolerance=1e-11)
  }
  # r = beta = 1: X+1 is Yule--Simon(alpha).
  expect_equal(exp(bnbinom_exact(0:30,c(1,2,1))),2*beta(1:31,3),tolerance=1e-12)
})

test_that("all fixed subsets agree with an independent multistart oracle", {
  skip_unless_tier("full")
  set.seed(4821)
  x <- rbnbinom(n = 1200, size = 0.7, alpha = 1.8, beta = 6)
  fixed_sets <- list(list(),list(size=.7),list(alpha=1.8),list(beta=6),
    list(size=.7,alpha=1.8),list(size=.7,beta=6),list(alpha=1.8,beta=6),
    list(size=.7,alpha=1.8,beta=6))
  for (fixed in fixed_sets) {
    fit <- do.call(mlbnbinom,c(list(x),fixed))
    expect_equal(attr(fit,"logLik"),bnbinom_oracle(x,fixed),tolerance=2e-7)
    expect_equal(attr(fit,"logLik"),sum(bnbinom_exact(x,as.numeric(fit))),tolerance=1e-9)
    expect_setequal(attr(fit,"fixed"),if (length(fixed)) names(fixed) else character())
    expect_equal(attr(logLik(fit),"df"),as.integer(3-length(fixed)))
    if (length(fixed)) expect_equal(unname(fit[names(fixed)]),unname(unlist(fixed)))
    if (!any(c("size","beta") %in% names(fixed))) expect_lte(fit["size"],fit["beta"])
  }
})

test_that("reachable faces and fully held likelihoods behave correctly", {
  expect_error(mlbnbinom(rep(c(0,5),each=20)),"Use `mlnbinom`")
  expect_error(mlbnbinom(rep(2,50)),"Use `mlpois`")
  expect_error(mlbnbinom(rep(2,50),size=1),"Use `mlnbinom`")
  expect_error(mlbnbinom(rep(2,50),beta=1),"Use `mlnbinom`")
  for (fixed in list(list(),list(alpha=2),list(size=1,beta=3),
                     list(size=1,alpha=2),list(alpha=2,beta=3)))
    expect_error(do.call(mlbnbinom,c(list(rep(0,10)),fixed)),"point mass")
  expect_equal(attr(logLik(mlbnbinom(0:4,size=1,alpha=2,beta=3)),"df"),0L)
  for (param in c("size","alpha","beta")) for (bad in c(0,-1,Inf,NaN))
    expect_error(do.call(mlbnbinom,c(list(0:4),setNames(list(bad),param))),"positive finite")
  expect_error(mlbnbinom(0:3,iterlim=0),"positive iterlim")
})

test_that("quantiles invert the internal CDF", {
  fit <- mlbnbinom(0:4,size=1,alpha=2,beta=3)
  u <- c(.01,.3,.7,.95)
  q <- qml(u,fit)
  expect_true(all(pml(q-1,fit)<u))
  expect_true(all(u<=pml(q,fit)))
  expect_equal(qml(c(0,1),fit),c(0,Inf))
  expect_equal(qml(log(u),fit,log.p=TRUE),q)
  expect_equal(qml(1-u,fit,lower.tail=FALSE),q)

})

test_that("model selection tolerates a failed beta negative binomial fit", {
  expect_no_error(model_select(rep(2,30),type="discrete"))
})

test_that("bnbinom: P3 unfinished optimizer stops", {
  x <- c(0,1,2,3,6,12,24)
  expect_error(mlbnbinom(x, iterlim=1, gradtol=1e-10, alpha=2,beta=3),
               "[Ff]ail|converge|unresolved|exhausted|score check")
})


test_that("unit beta-negative-binomial quantiles allow neighbouring integers at jumps", {
  f <- mlbnbinom()
  expect_equal(qml(.83, f), 4)
  expect_equal(qml(.17, f, lower.tail=FALSE), 4)
  expect_true(qml(-0.6931471805599453, f, log.p=TRUE) %in% c(0, 1))
  expect_true(qml(-4.605170185988092, f, lower.tail=FALSE, log.p=TRUE) %in% c(98, 99))
  expect_true(qml(1e-6, f, lower.tail=FALSE) %in% c(999998, 999999))
})

test_that("one-free-shape certificates recover exact roots and structured stops", {
  for (free in c("size","alpha","beta")) {
    fixed <- as.list(c(size=1,alpha=1,beta=1)); fixed[[free]] <- NULL
    f <- do.call(mlbnbinom,c(list(rep(1L,12)),fixed))
    expect_equal(unname(f[free]),sqrt(2),tolerance=1e-8)
    expect_equal(attr(f,"logLik"),12*log(sqrt(2)/((sqrt(2)+1)*(sqrt(2)+2))),tolerance=1e-12)
    e <- tryCatch(do.call(mlbnbinom,c(list(rep(1L,12),iterlim=1),fixed)),error=identity)
    expect_s3_class(e,"univariateML_budget")
    expect_equal(e$kind,"budget")
    expect_equal(nrow(e$candidates),1L)
    expect_true(is.infinite(e$loglik_upper))
    expect_true(all(c("size","alpha","beta") %in% names(e$candidates)))
  }
})

test_that("scalar cancellation is a structured numerical stop", {
  e <- tryCatch(mlbnbinom(rep(1L,8),size=1,beta=1,gradtol=1e-20),error=identity)
  expect_s3_class(e,"univariateML_numerical")
  expect_equal(e$kind,"numerical")
  expect_equal(nrow(e$candidates),1L)
})

test_that("alpha-held certificate preserves off-diagonal maxima and the singular seam", {
  root <- Sys.getenv("UMVL_DISTRIBUTIONS_DIR")
  skip_if(!nzchar(root), "no distribution goldens")
  x <- jsonlite::fromJSON(file.path(root,"bnbinom","golden.json"))$data
  fit <- mlbnbinom(x,alpha=2)
  expect_equal(unname(fit[c("size","beta")]),c(1.15097395202066,4.50374529147196),tolerance=1e-8)
  expect_equal(attr(fit,"logLik"),-1275.720827702446,tolerance=1e-12)
  raw <- getFromNamespace("mlbnbinom_","univariateML")(as.integer(x),alpha=2)
  expect_equal(raw$swap_maximum,unname(raw$estimates[c("beta","alpha","size")]))
  expect_equal(raw$diagonal_witness[1],raw$diagonal_witness[3])
  expect_lt(sum(bnbinom_exact(x,raw$diagonal_witness)),attr(fit,"logLik")-.1)
  e <- tryCatch(mlbnbinom(c(0,0,1,2,3,6,12,24),alpha=2,iterlim=1),error=identity)
  expect_s3_class(e,"univariateML_budget")
  expect_equal(e$kind,"budget")
  expect_gte(nrow(e$candidates),1L)
  expect_gte(e$loglik_upper,e$loglik_lower)
})

test_that("alpha-held diagonal maximum passes the full pair certificate", {
  fit <- mlbnbinom(c(0,0,1,2,3,6,12,24),alpha=.5)
  expect_equal(unname(fit[c("size","beta")]),rep(1.1088158545389952,2),tolerance=1e-8)
  expect_equal(attr(fit,"fixed"),"alpha")
  expect_equal(attr(logLik(fit),"df"),2L)
})

test_that("alpha-held range exhaustion is a structured numerical condition", {
  e <- tryCatch(mlbnbinom(c(0,0,1,2,3,6,12,24),alpha=1e308),error=identity)
  expect_s3_class(e,"univariateML_numerical")
  expect_equal(e$kind,"numerical")
  expect_true(all(is.finite(e$candidates$loglik_lower)))
  expect_true(all(is.finite(e$candidates$loglik_upper)))
})
