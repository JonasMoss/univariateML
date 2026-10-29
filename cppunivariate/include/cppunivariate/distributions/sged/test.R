context("mlsged")

## Deterministic interior data for successful-fit contracts.
set.seed(313)
small_data <- qsged(((1:200) - .5)/200, 2, 3, 4)
tiny_data <- qsged(((1:100) - .5)/100, 2, 3, 4)

## Finds errors with na and data out of bounds.
expect_error(suppressWarnings(mlsged(c(tiny_data, NA)), classes = "univariateML_restricted"))

## Checks that na.rm works as intended.
expect_equal(
  coef(suppressWarnings(mlsged(small_data), classes = "univariateML_restricted")),
  coef(suppressWarnings(mlsged(c(small_data, NA), na.rm = TRUE), classes = "univariateML_restricted"))
)

## Is the log-likelihood correct?
est <- suppressWarnings(mlsged(small_data, na.rm = TRUE), classes = "univariateML_restricted")
expect_equal(
  sum(dsged(small_data, est[1], est[2], est[3], est[4], log = TRUE)),
  attr(est, "logLik")
)

## Are the fallback methods working correctly?
expect_equal(
  log(dml(small_data, est)),
  dml(small_data, est, log = TRUE)
)
expect_equal(
  log(pml(small_data, est)),
  pml(small_data, est, log.p = TRUE)
)
expect_equal(
  1 - pml(small_data, est),
  pml(small_data, est, lower.tail = FALSE)
)
expect_equal(
  qml(1:3 / 4, est),
  qml(log(1:3 / 4), est, log.p = TRUE)
)
expect_equal(
  qml(1 - 1:3 / 4, est),
  qml(1:3 / 4, est, lower.tail = FALSE)
)


## Check class.
expect_equal(attr(est, "model"), "Skew Generalized Error")
expect_equal(class(est), "univariateML")


## Check support.
expect_equal(class(attr(est, "support"))[[1]], "numeric")

# Decision D6: attained Laplace, uniform and constrained interior oracle.
expect_error(suppressWarnings(mlsged(seq(-1, 1, length.out = 40)), classes = "univariateML_restricted"), "Use `mlunif`")
x <- c(seq(-.02, .02, length.out = 18), -10, 10)
f <- suppressWarnings(mlsged(x), classes = "univariateML_restricted")
expect_equal(unname(f["nu"]), 1)
expect_equal(attr(f, "logLik"), sum(dsged(x, mean=f[1], sd=f[2], nu=1, xi=f[4], log=TRUE)))
# Exact fGarch mapping of arbitrary two-piece Laplace parameters.
a <- .7; b <- 2; c <- -.3; grid <- seq(-4, 4, length.out=101)
expected <- -log(a+b) - ifelse(grid<c, (c-grid)/a, (grid-c)/b)
expect_equal(dsged(grid, c+b-a, sqrt(a*a+b*b), 1, sqrt(b/a), log=TRUE), expected)
expect_error(suppressWarnings(mlsged(-log(((0:99)+.5)/100)), classes = "univariateML_restricted"), "sample minimum")
expect_error(suppressWarnings(mlsged(log(((0:99)+.5)/100)), classes = "univariateML_restricted"), "sample maximum")

set.seed(812)
for (shape in c(1.5, 3)) {
  x <- rged(500, nu=shape)
  f <- suppressWarnings(mlsged(x, gradtol=1e-6, iterlim=1000), classes = "univariateML_restricted")
  expect_gt(unname(f["nu"]), 1)
  objective <- function(p) {
    nu <- 1+exp(p[3])
    lambda <- exp((-2/nu*log(2)+lgamma(1/nu)-lgamma(3/nu))/2)
    z <- (x-p[1])/exp(p[2])
    factor <- -p[2]
    xi <- exp(p[4])
    m1 <- exp(lgamma(2/nu)-(lgamma(1/nu)+lgamma(3/nu))/2)
    su <- sqrt((1-m1*m1)*(xi*xi+1/(xi*xi))+2*m1*m1-1)
    z <- z*su+m1*(xi-1/xi)
    z <- z/ifelse(z>0,xi,1/xi)
    factor <- factor+log(su)+log(2/(xi+1/xi))
    value <- -sum(factor+log(nu)-log(lambda)-(1+1/nu)*log(2)-
                    lgamma(1/nu)-.5*(abs(z)/lambda)^nu)
    if (is.finite(value)) value else 1e100
  }
  values <- sapply(c(1.1,1.5,2,4,8), function(nu) {
    optim(c(mean(x), log(sd(x)), log(nu-1), 0), objective,
          method="Nelder-Mead", control=list(maxit=10000, reltol=1e-11))$value
  })
  expect_gte(attr(f,"logLik") + 1e-4, -min(values))
}

test_that("sged scale extremes preserve fit or declared escape", {
  set.seed(10)
  x <- simulate_default("mlsged",50)
  base <- tryCatch(suppressWarnings(mlsged(x), classes = "univariateML_restricted"),error=function(e) conditionMessage(e))
  for(s in c(1e-300,1e300)) {
    f <- tryCatch(suppressWarnings(mlsged(x*s), classes = "univariateML_restricted"),error=function(e) conditionMessage(e))
    if(is.character(base)) expect_identical(f,base) else {
      expect_false(is.character(f))
      if(!is.character(f)) expect_equal(attr(f,"logLik")+length(x)*log(s),attr(base,"logLik"),tolerance=1e-4)
    }
  }
})

test_that("sged large-sample fitting preserves likelihood under replication", {
  p<-((1:1000)-.5)/1000
  x<-qsged(p,nu=3,xi=2)
  base<-suppressWarnings(mlsged(x), classes = "univariateML_restricted")
  f<-suppressWarnings(mlsged(rep(x,100)), classes = "univariateML_restricted")
  expect_equal(attr(f,"logLik")/100,attr(base,"logLik"),tolerance=1e-5)
})

# Package-owned dpqr contract, independent of the optional reference package.
test_that("sged dpqr integrate, invert and preserve extreme tails", {
  pars <- list(mean = .3, sd = 1.7, nu = 1.4, xi = 1.8)
  d <- function(x, ...) do.call(dsged, c(list(x), pars, list(...)))
  p <- function(q, ...) do.call(psged, c(list(q), pars, list(...)))
  q <- function(p, ...) do.call(qsged, c(list(p), pars, list(...)))
  for (x in c(-2, .3, 3)) {
    expect_equal(p(x), integrate(d, -Inf, x, rel.tol = 2e-10, subdivisions = 1000L)$value,
                 tolerance = 2e-9)
    expect_equal(p(x, lower.tail = FALSE),
                 integrate(d, x, Inf, rel.tol = 2e-10, subdivisions = 1000L)$value,
                 tolerance = 2e-9)
  }
  expect_equal(integrate(d, -Inf, Inf, rel.tol = 1e-9, subdivisions = 1000L)$value,
               1, tolerance = 2e-9)
  for (lower in c(TRUE, FALSE)) {
    lp <- c(-1000, -700, -100, -30, -2, -.1, -1e-12)
    expect_equal(p(q(lp, lower.tail = lower, log.p = TRUE), lower.tail = lower,
                   log.p = TRUE), lp, tolerance = 2e-11)
    prob <- c(1e-100, 1e-20, .01, .5, .99)
    expect_equal(p(q(prob, lower.tail = lower), lower.tail = lower), prob,
                 tolerance = 2e-12)
  }
  expect_equal(d(c(-Inf, Inf)), c(0, 0))
  expect_equal(d(c(-Inf, Inf), log = TRUE), c(-Inf, -Inf))
  expect_equal(p(c(-Inf, Inf)), c(0, 1))
  expect_equal(q(c(0, 1)), c(-Inf, Inf))
  expect_equal(q(c(-Inf, 0), log.p = TRUE), c(-Inf, Inf))
  expect_true(all(is.finite(d(c(-1000, 1000), log = TRUE))))
})

test_that("sged dpqr recycle, propagate missing values and use R RNG", {
  expect_length(dsged(numeric()), 0)
  expect_equal(dsged(c(0, 1, 2), mean = c(0, 1)),
    c(dsged(0), dsged(1, mean = 1), dsged(2)))
  expect_identical(dsged(NA_real_), NA_real_)
  expect_true(is.nan(psged(NaN)))
  expect_warning(expect_true(is.nan(dsged(0, sd = -1))), "NaNs produced")
  expect_warning(expect_true(is.nan(qsged(1.1))), "NaNs produced")
  expect_warning(expect_true(is.nan(rsged(1, sd = -1))), "NaNs produced")
  set.seed(81); first <- rsged(100)
  set.seed(81); expect_identical(rsged(100), first)
  expect_length(rsged(1:7), 7)
  expect_length(rsged(0), 0)
  expect_length(rsged(3, mean = numeric()), 0)
  expect_identical(rsged(1, mean = NA_real_), NA_real_)
  set.seed(712)
  draws <- rsged(5000)
  for (prob in c(.1, .5, .9))
    expect_lt(abs(mean(draws <= qsged(prob)) - prob), .025)
  for (bad in c(-1, 0, Inf)) {
    expect_warning(expect_true(is.nan(dsged(0, sd = bad))), "NaNs produced")
    expect_warning(expect_true(is.nan(psged(0, sd = bad))), "NaNs produced")
    expect_warning(expect_true(is.nan(qsged(.5, sd = bad))), "NaNs produced")
  }
  fit <- suppressWarnings(mlsged(small_data), classes = "univariateML_restricted")
  expect_equal(as.numeric(logLik(fit)), sum(dml(small_data, fit, log = TRUE)),
               tolerance = 1e-10)
  expect_equal(pml(0, fit), do.call(psged, c(list(0), as.list(coef(fit)))))
  expect_equal(qml(.2, fit), do.call(qsged, c(list(.2), as.list(coef(fit)))))
  set.seed(93); expected <- do.call(rsged, c(list(10), as.list(coef(fit))))
  set.seed(93); expect_identical(rml(10, fit), expected)
})

test_that("sged matches frozen fGarch values (reference.json)", {
  expect_reference_values("sged")
})

test_that("sged parameter domain is enforced", {
  for (bad in c(-1, 0, Inf))
    expect_warning(expect_true(is.nan(dsged(0, nu = bad))), "NaNs produced")
})

test_that("sged skew parameter domain is enforced", {
  for (bad in c(-1, 0, Inf))
    expect_warning(expect_true(is.nan(dsged(0, xi = bad))), "NaNs produced")
})

test_that("sged log tails exceed the backend exponent range", {
  for (lower in c(TRUE, FALSE)) {
    x <- qsged(-20000, lower.tail = lower, log.p = TRUE)
    expect_true(is.finite(x))
    expect_equal(psged(x, lower.tail = lower, log.p = TRUE), -20000,
                 tolerance = 1e-10)
    x <- qsged(-1e-30, lower.tail = lower, log.p = TRUE)
    expect_equal(psged(x, lower.tail = lower, log.p = TRUE)/(-1e-30), 1,
                 tolerance = 1e-10)
  }
})

# Independent README density (R normal/t primitives and gamma functions).
sged_hold_logref <- function(x,p) {
  xi <- p['xi']; nu <- p['nu']
  m <- exp(lgamma(2/nu)-.5*(lgamma(1/nu)+lgamma(3/nu)))
  v <- (1-m*m)*(xi^2+xi^(-2))+2*m*m-1
  z <- sqrt(v)*(x-p['mean'])/p['sd']+m*(xi-1/xi)
  z <- ifelse(z>0,z/xi,z*xi)
  loga <- .5*(lgamma(1/nu)-lgamma(3/nu)); core <- log(nu)-log(2)-loga-lgamma(1/nu)-exp(nu*(log(abs(z))-loga))
  .5*log(v)-log(p['sd'])+log(2/(xi+1/xi))+core
}

test_that("sged classified holds agree with independent multistart oracles", {
  set.seed(81029)
  p <- c(mean=.2,sd=1.4,nu=1.7,xi=1.3)
  subsets <- list(c('mean','nu','xi'),c('sd','nu','xi'),names(p))
  samples <- list(rnorm(20),rnorm(100),round(rnorm(20),1),c(-1,0,1),
                  rexp(20),-rexp(100),abs(rnorm(20)),runif(100,-2,2))
  for(x in samples) for(held in subsets) {
    free <- setdiff(names(p),held)
    fit <- tryCatch(do.call(mlsged,c(list(x=x),as.list(p[held]))),error=identity)
    if(inherits(fit,'error')) {
      expect_match(conditionMessage(fit),'half-normal|unbounded|collapse|converge')
      next
    }
    expect_identical(unname(as.numeric(fit[held])),unname(p[held]))
    expect_setequal(attr(fit,'fixed'),held)
    expect_equal(as.numeric(logLik(fit)),sum(sged_hold_logref(x,coef(fit))),tolerance=1e-9)
    if(!length(free)) next
    pack <- function(q) { q <- q[free]; q[names(q)!='mean'] <- log(q[names(q)!='mean']); unname(q) }
    objective <- function(t) {
      q <- p; q[free] <- t
      positive <- intersect(free,c('sd','xi'))
      q[positive] <- exp(q[positive])
      val <- -sum(sged_hold_logref(x,q))
      if(is.finite(val)) val else 1e100
    }
    refs <- lapply(c(-.8,0,.8),function(delta)
      optim(pack(p)+delta,objective,method=if(length(free)==1) 'BFGS' else 'Nelder-Mead',
            control=list(maxit=3000,reltol=1e-12))$value)
    expect_gte(as.numeric(logLik(fit)),-min(unlist(refs))-1e-6*length(x))
  }
})

test_that("sged holds validate values, ties, and all-held evaluation", {
  x <- c(-1,0,1)
  p <- c(mean=.2,sd=1.4,nu=1.7,xi=1.3)
  for(v in names(p)) for(bad in c(NA_real_,Inf,-Inf)) {
    args <- as.list(p); args[[v]] <- bad
    expect_error(do.call(mlsged,c(list(x=x),args)), 'held')
  }
  args <- as.list(p)
  fit <- do.call(mlsged,c(list(x=c(0,0,0)),args))
  expect_equal(as.numeric(logLik(fit)),sum(sged_hold_logref(c(0,0,0),p)),tolerance=1e-10)
  expect_error(suppressWarnings(mlsged(rep(.2,3),mean=.2,nu=1.7,xi=1.3), classes = "univariateML_restricted"),'unbounded')
  expect_error(suppressWarnings(mlsged(x,xi=1), classes = "univariateML_restricted"),'unresolved')
  below <- suppressWarnings(mlsged(x,sd=1,nu=.7,xi=1.2), classes = "univariateML_restricted")
  expect_identical(unname(below['nu']),.7)
})

test_that("sged held domains and controls reject invalid requests", {
  x <- c(-1,0,1)
  for(v in c('sd','nu','xi')) for(bad in c(0,-1)) {
    args <- list(x=x,mean=.2,sd=1.4,nu=1.7,xi=1.3); args[[v]] <- bad
    expect_error(do.call(mlsged,args),'held')
  }
  expect_error(suppressWarnings(mlsged(x,mean=.2,nu=1.7,xi=1,iterlim=0), classes = "univariateML_restricted"),'requires')
})
