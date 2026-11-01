context("mlbbinom")

bb_sample <- function() {
  set.seed(720)
  rbbinom(n = 500, size = 10, alpha = 2, beta = 3)
}

test_that("known size and fixed shapes have correct attributes", {
  x <- bb_sample()
  for (held in list(list(), list(alpha=2), list(beta=3))) {
    fit <- do.call(mlbbinom, c(list(x=x, size=10), held))
    expect_equal(names(fit), c("size", "alpha", "beta"))
    expect_equal(attr(fit,"fixed"), c("size", names(held)))
    expect_equal(attr(logLik(fit),"df"), 2L-length(held))
    expect_equal(attr(fit,"logLik"), sum(dbbinom(x = x, size = 10, alpha = fit[2], beta = fit[3], log = TRUE)), tolerance=1e-9)
    expect_equal(AIC(fit), -2*as.numeric(logLik(fit))+2*(2-length(held)))
    if (length(held)) expect_equal(unname(fit[names(held)]), held[[1]])
  }
  expect_error(mlbbinom(x), "requires `size`")
  expect_error(mlbbinom(x,size=9), "smaller")
  expect_equal(attr(mlbbinom(x,size=10,alpha=2,beta=3), "fixed"), c("size","alpha","beta"))
  for (size in c(-1, 1.5, Inf)) expect_error(mlbbinom(x,size=size), "nonnegative integer")
})

test_that("faces and nonidentification are explicit", {
  expect_error(mlbbinom(c(4,5,5,6,5,4,6,5,5,5),size=10), "Use `mlbinom\\(x, size = 10\\)`")
  expect_error(mlbbinom(c(0,7,0,7),size=7), "two-point distribution")
  expect_error(mlbbinom(c(0,0),size=7), "point mass")
  expect_error(mlbbinom(c(0,1,1,0),size=1), "not identified")
  expect_error(mlbbinom(c(0,0),size=0), "not identified")
  fit <- mlbbinom(c(0,1,1,0,1),size=1,alpha=2)
  expect_equal(unname(fit["beta"]),4/3,tolerance=1e-8)
})

test_that("fit agrees with slow multistart reference oracle", {
  skip_unless_tier("full")
  x <- bb_sample()
  for (held in list(list(),list(alpha=.4),list(beta=8))) {
    objective <- function(theta) {
      ab <- if (!length(held)) exp(theta) else if (names(held)=="alpha") c(held[[1]], exp(theta)) else c(exp(theta),held[[1]])
      -sum(dbbinom(x = x, size = 10, alpha = ab[1], beta = ab[2], log = TRUE))
    }
    values <- vapply(c(.05,.5,2,10,100), function(start) {
      optim(rep(log(start),if (length(held)) 1 else 2),objective,method="L-BFGS-B",lower=-10,upper=10,control=list(maxit=2000,factr=1e3))$value
    },numeric(1))
    fit <- do.call(mlbbinom,c(list(x=x,size=10),held))
    expect_equal(attr(fit,"logLik"),-min(values),tolerance=1e-7)
  }
})

test_that("beta-binomial quantiles invert the internal CDF", {
  fit <- mlbbinom(bb_sample(),size=10)
  for (lower in c(TRUE,FALSE)) for (log in c(TRUE,FALSE)) {
    u <- c(0,1e-12,.1,.5,.9,1-1e-12,1)
    p <- if (log) log(u) else u
    q <- qml(p,fit,lower.tail=lower,log.p=log)
    expect_equal(q[c(1,7)],if (lower) c(0,10) else c(10,0))
    f <- pml(q,fit,lower.tail=lower,log.p=log)
    prev <- pml(q-1,fit,lower.tail=lower,log.p=log)
    if (lower) {
      expect_true(all(f>=p))
      expect_true(all(prev[q>0]<p[q>0]))
    } else {
      expect_true(all(f<=p))
      expect_true(all(prev[q>0]>p[q>0]))
    }
  }
  expect_warning(expect_true(is.nan(qml(-.1,fit))),"NaNs")
  expect_true(is.na(qml(NA_real_,fit)))

})


test_that("bbinom: both held shapes permit exact size profiling", {
 set.seed(88)
 for(x in c(lapply(c(20,100),function(n) rbbinom(n = n, size = 8, alpha = 2, beta = 3)),list(c(0,0),c(0,1),c(2,2,3),rep(3,4)))) {
  for(size in list(8,NULL)) {
   f<-mlbbinom(x,size=size,alpha=2,beta=3)
   grid<-if(is.null(size)) seq(max(x),100) else size
   oracle<-max(vapply(grid,function(k)sum(dbbinom(x = x, size = k, alpha = 2, beta = 3, log = TRUE)),0.))
   expect_equal(attr(f,"logLik"),oracle,tolerance=1e-9)
   expect_identical(unname(f[c("alpha","beta")]),c(2,3))
   expect_setequal(attr(f,"fixed"),c(if(!is.null(size)) "size","alpha","beta"))
  }
 }
 expect_error(mlbbinom(1:3,size=NULL),"together with both shapes is not implemented")
 expect_error(mlbbinom(1:3,size=NULL,alpha=2),"with alpha held is not implemented")
 expect_error(mlbbinom(rep(3,3),size=NULL,beta=3),"point mass")
})

test_that("bbinom: P3 unfinished optimizer stops", {
  g <- jsonlite::fromJSON(file.path(Sys.getenv("UMVL_DISTRIBUTIONS_DIR"), "bbinom", "golden.json"))
  x <- g$data
  expect_error(mlbbinom(x, iterlim=1, reltol=1e-10, size=10),
               "[Ff]ail|converge|unresolved|exhausted|score check")
})

test_that("bbinom: retained free-size guards identify the unresolved profile", {
  expect_error(mlbbinom(c(0,1,2,4,8),size=NULL), "binomial and two-point limits.*no usable bound")
  expect_error(mlbbinom(c(0,1,2,4,8),size=NULL,alpha=2), "negative binomial with the same alpha.*no usable bound")
  expect_error(mlbbinom(c(0,1,2,4,8),size=NULL,beta=1e9), "work budget")
  # Fixed alpha with beta proportional to size approaches NB with that
  # alpha held. Reflecting x to N-x would change the observed sample.
  x <- 0:10; alpha <- 2; mu <- 3
  for(N in c(1000,10000)) {
    finite <- dbbinom(x=x,size=N,alpha=alpha,beta=N*alpha/mu)
    face <- dnbinom(x,size=alpha,mu=mu)
    expect_lt(max(abs(finite-face)),2/N)
  }
})


test_that("held beta free-size fit has a certified tail", {
 x <- c(0, 1, 2, 4)
 f <- mlbbinom(x, size=NULL, beta=.5)
 expect_equal(unname(f["size"]), 4)
 expect_equal(attr(f,"fixed"), "beta")
 grid <- vapply(4:30, function(n) attr(mlbbinom(x,size=n,beta=.5),"logLik"), 0.)
 expect_equal(attr(f,"logLik"), max(grid), tolerance=1e-8)
 expect_error(mlbbinom(rep(3,4),size=NULL,beta=2), "point mass")
 expect_error(mlbbinom(c(0,0),size=NULL,beta=2), "not identified")
 expect_error(mlbbinom(x,size=NULL,beta=1e9), "work budget")
})

test_that("seeded held-beta profiles agree with dense size enumeration", {
  set.seed(113)
  for (rep in 1:6) {
    x <- c(0, 4, sample(0:4, 8, replace=TRUE))
    b <- if (rep %% 2) .5 else 1.2
    fit <- mlbbinom(x, size=NULL, beta=b)
    oracle <- vapply(4:40, function(n) attr(mlbbinom(x,size=n,beta=b),"logLik"), 0.)
    expect_gte(attr(fit,"logLik"), max(oracle)-1e-7)
  }
})
