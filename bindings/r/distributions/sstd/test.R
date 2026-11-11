context("mlsstd")

## Deterministic interior data for successful-fit contracts.
set.seed(313)
small_data <- qsstd(p = ((1:200) - 0.5)/200, mean = 0, sd = 1, nu = 5, xi = 1.5)
tiny_data <- qsstd(p = ((1:100) - 0.5)/100, mean = 0, sd = 1, nu = 5, xi = 1.5)

## Finds errors with na and data out of bounds.
expect_error(mlsstd(c(tiny_data, NA)))

## Checks that na.rm works as intended.
expect_equal(
  coef(mlsstd(small_data)),
  coef(mlsstd(c(small_data, NA), na.rm = TRUE))
)

## Is the log-likelihood correct?
est <- mlsstd(small_data, na.rm = TRUE)
expect_equal(
  sum(dsstd(x = small_data, mean = est[1], sd = est[2], nu = est[3], xi = est[4], log = TRUE)),
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
expect_equal(attr(est, "model"), "Skew Student-t")
expect_equal(class(est), "univariateML")


## Check support.
expect_equal(class(attr(est, "support"))[[1]], "numeric")

## Boundary faces (decision D1; dev/research/ml-behaviour.tex).
test_that("mlsstd stops on the skew-normal face for normal data", {
  set.seed(1)
  expect_error(mlsstd(rnorm(200)), "skew-normal distribution. Use `mlsnorm`")
})

test_that("mlsstd stops on the translated half-normal and half-t faces", {
  x <- qnorm(0.5 + 0.5 * ppoints(200))
  expect_error(mlsstd(x), "endpoint at the sample minimum. Use `mlhnorm` to fit")
  expect_error(mlsstd(-x), "Use `mlhnorm` on `-x`")
  set.seed(3)
  y <- rexp(300)
  expect_error(mlsstd(y), "xi -> Inf, i.e. by a translated half-t with [0-9.]+ degrees of freedom and endpoint at the sample minimum")
  expect_error(mlsstd(-y), "xi -> 0, i.e. by a translated half-t with [0-9.]+ degrees of freedom and endpoint at the sample maximum")
})

test_that("mlsstd stops on the df-2 face for skewed Cauchy data", {
  z <- qcauchy(ppoints(200))
  expect_error(mlsstd(ifelse(z > 0, 2 * z, z)), "nu -> 2, i.e. by a two-piece t with 2 degrees of freedom")
})

## Slow oracle: the interior over dsstd, and each face fitted directly.
sstd_oracle <- function(x) {
  ll <- function(p) {
    v <- suppressWarnings(dsstd(x = x, mean = p[1], sd = exp(p[2]), nu = 2 + exp(p[3]), xi = exp(p[4]), log = TRUE))
    if (any(!is.finite(v))) -1e300 else sum(v)
  }
  interior <- -Inf
  for (nu in c(2.3, 4, 10, 100)) for (xi in c(0.5, 1, 2)) {
    o <- optim(c(mean(x), log(sd(x)), log(nu - 2), log(xi)), ll,
               control = list(fnscale = -1, maxit = 4000, reltol = 1e-14))
    o <- optim(o$par, ll, method = "BFGS", control = list(fnscale = -1, reltol = 1e-15))
    interior <- max(interior, o$value)
  }
  half_t <- -Inf
  for (y in list(x - min(x), max(x) - x)) for (nu in c(2.1, 4, 20, 1e3)) {
    f <- function(p) sum(log(2) - p[1] + dt(y / exp(p[1]), 2 + exp(p[2]), log = TRUE))
    o <- optim(c(log(sqrt(mean(y^2))), log(nu - 2)), f, control = list(fnscale = -1, reltol = 1e-14, maxit = 4000))
    half_t <- max(half_t, o$value)
  }
  t2 <- -Inf
  for (q in quantile(x, c(.25, .5, .75))) for (k in c(-1, 0, 1)) {
    f <- function(p) { d <- x - p[1]
      sum(log(2) - log(exp(p[2]) + exp(p[3])) + dt(ifelse(d < 0, d / exp(p[2]), d / exp(p[3])), 2, log = TRUE)) }
    s <- log(IQR(x) / 1.6)
    t2 <- max(t2, optim(c(q, s - k, s + k), f, control = list(fnscale = -1, reltol = 1e-14, maxit = 4000))$value)
  }
  hn <- function(y) length(y) * (0.5 * log(2 / pi) - log(sqrt(mean(y^2))) - 0.5)
  snorm <- tryCatch(attr(mlsnorm(x), "logLik"),
                    error = function(e) max(hn(x - min(x)), hn(max(x) - x)))
  list(interior = interior, faces = c(snorm = snorm, half_t = half_t, t2 = t2))
}

test_that("mlsstd agrees with the slow oracle on interior and boundary samples", {
  skip_unless_tier("full")
  face_of <- function(msg) {
    if (grepl("skew-normal|half-normal", msg)) "snorm"
    else if (grepl("half-t", msg)) "half_t" else if (grepl("nu -> 2", msg)) "t2" else msg
  }
  # n = 20 family samples whose supremum is interior or on each face.
  cases <- list(c(5, 1.5, 5), c(5, 1.5, 7), c(5, 1.5, 1), c(5, 1.5, 2), c(10, 1, 8))
  for (case in cases) {
    set.seed(1000 * case[1] + case[3])
    x <- rsstd(n = 20, mean = 0, sd = 1, nu = case[1], xi = case[2])
    o <- sstd_oracle(x)
    fit <- tryCatch(mlsstd(x), error = function(e) conditionMessage(e))
    if (is.character(fit)) {
      face <- face_of(fit)
      expect_true(face %in% names(o$faces), info = fit)
      expect_gte(o$faces[[face]], max(o$faces) - 1e-6)
      expect_lte(o$interior, max(o$faces) + 1e-6)
    } else {
      expect_gte(attr(fit, "logLik"), max(o$interior, o$faces) - 1e-6)
    }
  }
  set.seed(313)
  x <- rsstd(n = 500, mean = 0, sd = 1, nu = 5, xi = 1.5)
  fit <- mlsstd(x)
  o <- sstd_oracle(x)
  expect_equal(attr(fit, "logLik"), o$interior, tolerance = 1e-8)
  expect_gt(attr(fit, "logLik"), max(o$faces))
})

test_that("sstd scale extremes preserve fit or declared escape", {
  set.seed(10)
  x <- ml_simulate("mlsstd",50)
  base <- tryCatch(mlsstd(x),error=function(e) conditionMessage(e))
  for(s in c(1e-300,1e300)) {
    f <- tryCatch(mlsstd(x*s),error=function(e) conditionMessage(e))
    if(is.character(base)) expect_identical(f,base) else {
      expect_false(is.character(f))
      if(!is.character(f)) expect_equal(attr(f,"logLik")+length(x)*log(s),attr(base,"logLik"),tolerance=1e-4)
    }
  }
})

test_that("sstd large-sample fitting preserves likelihood under replication", {
  skip_unless_tier("full")
  x<-qt(((1:1000)-.5)/1000,df=4)/sqrt(2)
  base<-mlsstd(x)
  y<-rep(x,100)
  f<-mlsstd(y)
  expect_equal(attr(f,"logLik")/100,attr(base,"logLik"),tolerance=1e-5)
})

test_that("skew t detects tie collapse before free df and face fits", {
  expect_error(mlsstd(c(rep(0,8),-1,1,2)),"unbounded.*multiplicity")
  for(k in c(5L,6L,7L)) {
    x <- c(rep(0,k),1,2,3)
    if(k==7L) expect_error(mlsstd(x),"unbounded")
    if(k==6L) expect_error(mlsstd(x),"equality.*not certified")
    if(k==5L) {
      f <- tryCatch(mlsstd(x),error=identity)
      if(inherits(f,"error")) expect_false(grepl("unbounded|equality",conditionMessage(f)))
      else expect_true(all(is.finite(f)))
    }
  }
})

# Package-owned dpqr contract, independent of the optional reference package.
test_that("sstd dpqr integrate, invert and preserve extreme tails", {
  pars <- list(mean = .3, sd = 1.7, nu = 5, xi = 1.8)
  d <- function(x, ...) do.call(dsstd, c(list(x), pars, list(...)))
  p <- function(q, ...) do.call(psstd, c(list(q), pars, list(...)))
  q <- function(p, ...) do.call(qsstd, c(list(p), pars, list(...)))
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

test_that("sstd dpqr recycle, propagate missing values and use R RNG", {
  expect_length(dsstd(x = numeric(), mean = 0, sd = 1, nu = 5, xi = 1.5), 0)
  expect_equal(dsstd(x = c(0, 1, 2), mean = c(0, 1), sd = 1, nu = 5, xi = 1.5),
    c(dsstd(x = 0, mean = 0, sd = 1, nu = 5, xi = 1.5), dsstd(x = 1, mean = 1, sd = 1, nu = 5, xi = 1.5), dsstd(x = 2, mean = 0, sd = 1, nu = 5, xi = 1.5)))
  expect_identical(dsstd(x = NA_real_, mean = 0, sd = 1, nu = 5, xi = 1.5), NA_real_)
  expect_true(is.nan(psstd(q = NaN, mean = 0, sd = 1, nu = 5, xi = 1.5)))
  expect_warning(expect_true(is.nan(dsstd(x = 0, mean = 0, sd = -1, nu = 5, xi = 1.5))), "NaNs produced")
  expect_warning(expect_true(is.nan(qsstd(p = 1.1, mean = 0, sd = 1, nu = 5, xi = 1.5))), "NaNs produced")
  expect_warning(expect_true(is.nan(rsstd(n = 1, mean = 0, sd = -1, nu = 5, xi = 1.5))), "NaNs produced")
  set.seed(81); first <- rsstd(n = 100, mean = 0, sd = 1, nu = 5, xi = 1.5)
  set.seed(81); expect_identical(rsstd(n = 100, mean = 0, sd = 1, nu = 5, xi = 1.5), first)
  expect_length(rsstd(n = 1:7, mean = 0, sd = 1, nu = 5, xi = 1.5), 7)
  expect_length(rsstd(n = 0, mean = 0, sd = 1, nu = 5, xi = 1.5), 0)
  expect_length(rsstd(n = 3, mean = numeric(), sd = 1, nu = 5, xi = 1.5), 0)
  expect_identical(rsstd(n = 1, mean = NA_real_, sd = 1, nu = 5, xi = 1.5), NA_real_)
  set.seed(712)
  draws <- rsstd(n = 5000, mean = 0, sd = 1, nu = 5, xi = 1.5)
  for (prob in c(.1, .5, .9))
    expect_lt(abs(mean(draws <= qsstd(p = prob, mean = 0, sd = 1, nu = 5, xi = 1.5)) - prob), .025)
  for (bad in c(-1, 0, Inf)) {
    expect_warning(expect_true(is.nan(dsstd(x = 0, mean = 0, sd = bad, nu = 5, xi = 1.5))), "NaNs produced")
    expect_warning(expect_true(is.nan(psstd(q = 0, mean = 0, sd = bad, nu = 5, xi = 1.5))), "NaNs produced")
    expect_warning(expect_true(is.nan(qsstd(p = 0.5, mean = 0, sd = bad, nu = 5, xi = 1.5))), "NaNs produced")
  }
  fit <- mlsstd(small_data)
  expect_equal(as.numeric(logLik(fit)), sum(dml(small_data, fit, log = TRUE)),
               tolerance = 1e-10)
  expect_equal(pml(0, fit), do.call(psstd, c(list(0), as.list(coef(fit)))))
  expect_equal(qml(.2, fit), do.call(qsstd, c(list(.2), as.list(coef(fit)))))
  set.seed(93); expected <- do.call(rsstd, c(list(10), as.list(coef(fit))))
  set.seed(93); expect_identical(rml(10, fit), expected)
})

test_that("sstd matches frozen fGarch values (reference.json)", {
  expect_reference_values("sstd")
})

test_that("sstd parameter domain is enforced", {
  for (bad in c(-1, 0, 2, Inf))
    expect_warning(expect_true(is.nan(dsstd(x = 0, mean = 0, sd = 1, nu = bad, xi = 1.5))), "NaNs produced")
})

test_that("sstd skew parameter domain is enforced", {
  for (bad in c(-1, 0, Inf))
    expect_warning(expect_true(is.nan(dsstd(x = 0, mean = 0, sd = 1, nu = 5, xi = bad))), "NaNs produced")
})

test_that("sstd log tails exceed the backend exponent range", {
  for (lower in c(TRUE, FALSE)) {
    x <- qsstd(p = -20000, mean = 0, sd = 1, nu = 1000, xi = 1.5, lower.tail = lower, log.p = TRUE)
    expect_true(is.finite(x))
    expect_equal(psstd(q = x, mean = 0, sd = 1, nu = 1000, xi = 1.5, lower.tail = lower, log.p = TRUE), -20000,
                 tolerance = 1e-10)
    x <- qsstd(p = -1e-30, mean = 0, sd = 1, nu = 1000, xi = 1.5, lower.tail = lower, log.p = TRUE)
    expect_equal(psstd(q = x, mean = 0, sd = 1, nu = 1000, xi = 1.5, lower.tail = lower, log.p = TRUE)/(-1e-30), 1,
                 tolerance = 1e-10)
  }
})

# Independent README density (R normal/t primitives and gamma functions).
sstd_hold_logref <- function(x,p) {
  xi <- p['xi']; nu <- p['nu']
  m <- exp(lgamma((nu-1)/2)-lgamma(nu/2)+.5*log((nu-2)/pi))
  v <- (1-m*m)*(xi^2+xi^(-2))+2*m*m-1
  z <- sqrt(v)*(x-p['mean'])/p['sd']+m*(xi-1/xi)
  z <- ifelse(z>0,z/xi,z*xi)
  s <- sqrt((nu-2)/nu); core <- dt(z/s,nu,log=TRUE)-log(s)
  .5*log(v)-log(p['sd'])+log(2/(xi+1/xi))+core
}

test_that("sstd classified holds agree with independent multistart oracles", {
  skip_unless_tier("full")
  set.seed(81029)
  p <- c(mean=.2,sd=1.4,nu=4.2,xi=1.3)
  subsets <- list(c('mean','nu','xi'),c('sd','nu','xi'),names(p))
  samples <- list(rnorm(20),rnorm(100),round(rnorm(20),1),c(-1,0,1),
                  rexp(20),-rexp(100),abs(rnorm(20)),runif(100,-2,2))
  for(x in samples) for(held in subsets) {
    free <- setdiff(names(p),held)
    fit <- tryCatch(do.call(mlsstd,c(list(x=x),as.list(p[held]))),error=identity)
    if(inherits(fit,'error')) {
      expect_match(conditionMessage(fit),'half-normal|unbounded|collapse|converge')
      next
    }
    expect_identical(unname(as.numeric(fit[held])),unname(p[held]))
    expect_setequal(attr(fit,'fixed'),held)
    expect_equal(as.numeric(logLik(fit)),sum(sstd_hold_logref(x,coef(fit))),tolerance=1e-9)
    if(!length(free)) next
    pack <- function(q) { q <- q[free]; q[names(q)!='mean'] <- log(q[names(q)!='mean']); unname(q) }
    objective <- function(t) {
      q <- p; q[free] <- t
      positive <- intersect(free,c('sd','xi'))
      q[positive] <- exp(q[positive])
      val <- -sum(sstd_hold_logref(x,q))
      if(is.finite(val)) val else 1e100
    }
    refs <- lapply(c(-.8,0,.8),function(delta)
      optim(pack(p)+delta,objective,method=if(length(free)==1) 'BFGS' else 'Nelder-Mead',
            control=list(maxit=3000,reltol=1e-12))$value)
    expect_gte(as.numeric(logLik(fit)),-min(unlist(refs))-1e-6*length(x))
  }
})

test_that("sstd holds validate values, ties, and all-held evaluation", {
  x <- c(-1,0,1)
  p <- c(mean=.2,sd=1.4,nu=4.2,xi=1.3)
  for(v in names(p)) for(bad in c(NA_real_,Inf,-Inf)) {
    args <- as.list(p); args[[v]] <- bad
    expect_error(do.call(mlsstd,c(list(x=x),args)), 'held')
  }
  args <- as.list(p)
  fit <- do.call(mlsstd,c(list(x=c(0,0,0)),args))
  expect_equal(as.numeric(logLik(fit)),sum(sstd_hold_logref(c(0,0,0),p)),tolerance=1e-10)
  expect_error(mlsstd(rep(.2,3),mean=.2,nu=4.2,xi=1.3),'unbounded')
  expect_error(mlsstd(c(0,0,0,1),mean=0,nu=3,xi=1),'equality')
  expect_error(mlsstd(c(0,0,0,1),mean=0,nu=2.5,xi=1),'unbounded')
})

test_that("sstd held domains and controls reject invalid requests", {
  x <- c(-1,0,1)
  for(v in c('sd','nu','xi')) for(bad in c(0,-1)) {
    args <- list(x=x,mean=.2,sd=1.4,nu=4.2,xi=1.3); args[[v]] <- bad
    expect_error(do.call(mlsstd,args),'held')
  }
  expect_error(mlsstd(x,mean=.2,nu=2,xi=1),'held')
  expect_error(mlsstd(x,mean=.2,nu=4.2,xi=1,iterlim=0),'requires')
})


test_that("sstd P0b subsets agree with independent README oracles", {
  skip_unless_tier("full")
  set.seed(931029)
  p <- c(mean=.2, sd=1.4, nu=4.2, xi=1.3)
  subsets <- list('mean','sd','nu','xi',c('mean','sd'),c('mean','nu'),
    c('mean','xi'),c('sd','nu'),c('sd','xi'),c('nu','xi'),
    c('mean','sd','nu'),c('mean','sd','xi'))
  samples <- list(rnorm(20), rnorm(100), round(rnorm(20),1), c(-1,0,1),
    rexp(20), -rexp(20), abs(rt(20,3)), runif(100,-2,2))
  # Independent two-piece draws from the fitted family, in addition to
  # the tied, small and limit-law samples above.
  nu <- p['nu']; xi <- p['xi']
  m <- exp(lgamma((nu-1)/2)-lgamma(nu/2)+.5*log((nu-2)/pi))
  for(n in c(20,100)) {
    z <- abs(rt(n,nu))*sqrt((nu-2)/nu)
    u <- ifelse(runif(n)<1/(1+xi^2),-z/xi,z*xi)
    scale <- sqrt(1+(1-m*m)*(xi-1/xi)^2)
    samples[[length(samples)+1]] <- p['mean']+p['sd']*(u-m*(xi-1/xi))/scale
  }
  returned <- integer(length(subsets))
  for(x in samples) for(j in seq_along(subsets)) {
    held <- subsets[[j]]; free <- setdiff(names(p),held)
    objective <- function(theta) {
      q <- p
      for(i in seq_along(free)) {
        nm <- free[i]
        q[nm] <- if(nm=='mean') theta[i] else if(nm=='nu') 2+exp(theta[i]) else exp(theta[i])
      }
      if(any(!is.finite(q)) || q['nu']>1e5) return(1e100)
      val <- -sum(sstd_hold_logref(x,q))
      if(is.finite(val)) val else 1e100
    }
    pack <- function(q) vapply(free,function(nm)
      if(nm=='mean') q[nm] else if(nm=='nu') log(max(q[nm]-2,1e-8)) else log(q[nm]), 0.0)
    refs <- vapply(c(-.6,0,.6),function(delta) {
      initial <- pack(p)+delta
      a <- optim(initial, objective, method='BFGS',control=list(maxit=1500,reltol=1e-12))
      b <- if(length(free)==1) optim(initial, objective, method='Brent', lower=-18, upper=11) else
        optim(initial, objective, method='Nelder-Mead',control=list(maxit=2500,reltol=1e-12))
      min(a$value,b$value)
    },0.0)
    fit <- tryCatch(suppressWarnings(do.call(mlsstd,
      c(list(x=x,gradtol=1e-6,iterlim=1000),as.list(p[held]))),
      classes='univariateML_restricted'),error=identity)
    if(inherits(fit,'error')) {
      expect_match(conditionMessage(fit),'half|uniform|skew-normal|nu -> 2|unbounded|collapse|score check')
      message <- conditionMessage(fit)
      expect_false(grepl('score check',message),
        info=paste('held',paste(held,collapse=','),'n',length(x),'first',x[1],message))
      if(grepl('uniform',message)) {
        width <- if('sd' %in% held) sqrt(12)*p['sd'] else if('mean' %in% held)
          2*max(abs(x-p['mean'])) else diff(range(x))
        expect_gte(-length(x)*log(width),-min(refs)-1e-5*length(x))
      } else if(grepl('half|skew-normal|nu -> 2',message)) {
        # Independent approach to the reported face. Refit all other free
        # coordinates with R's optimizers, pinning only the escaping one.
        theta <- pack(p); pinned <- integer()
        if(grepl('half',message)) {
          pinned <- match('xi',free)
          if(!is.na(pinned)) theta[pinned] <- if(grepl('xi -> 0|reflected|sample maximum',message)) -12 else 12
        }
        if(grepl('skew-normal|half-normal|nu -> 2',message)) {
          pos <- match('nu',free)
          if(!is.na(pos)) {
            pinned <- c(pinned,pos)
            theta[pos] <- if(grepl('nu -> 2',message)) log(1e-7) else log(60000-2)
            if(grepl('nu -> 2',message) && 'sd' %in% free)
              theta[match('sd',free)] <- theta[match('sd',free)]+.5*log(1e7)
          }
        }
        pinned <- unique(pinned[!is.na(pinned)])
        remaining <- setdiff(seq_along(free),pinned)
        approach <- function(v) { t <- theta; t[remaining] <- v; objective(t) }
        values <- objective(theta)
        if(length(remaining)) for(delta in c(-.5,0,.5)) {
          initial <- theta[remaining]+delta
          values <- c(values,optim(initial,approach,method='BFGS',
            control=list(maxit=2000,reltol=1e-12))$value)
          if(length(remaining)>1) values <- c(values,optim(initial,approach,method='Nelder-Mead',
            control=list(maxit=3000,reltol=1e-12))$value)
        }
        if(length(remaining)==1) {
          upper <- if(free[remaining]=='nu') 11 else 18
          values <- c(values,optim(theta[remaining],approach,method='Brent',lower=-18,upper=upper)$value)
        }
        if(grepl('half',message) && !('mean' %in% held)) {
          # Free endpoint has an exact profile: avoid a finite-xi optimizer
          # trying to approach a support discontinuity in mean.
          left <- grepl('xi -> 0|reflected|sample maximum',message)
          y <- if(left) max(x)-x else x-min(x)
          face_at_shape <- function(nu) {
          m <- exp(.5*log(nu/pi)+lgamma((nu-1)/2)-lgamma(nu/2))
          v <- nu/(nu-2)
          scale <- if('sd' %in% held) p['sd']/sqrt(v-m*m) else {
            score <- function(ls) sum((nu+1)*y*y/(nu*exp(2*ls)+y*y))-length(y)
            exp(uniroot(score,c(-30,30),tol=1e-11)$root)
          }
          -sum(log(2)+dt(y/scale,nu,log=TRUE)-log(scale))
          }
          if('nu' %in% held) values <- c(values,face_at_shape(p['nu'])) else {
            profile <- function(t) face_at_shape(2+exp(t))
            grid <- seq(-15,11,length.out=101)
            raw <- vapply(grid,profile,0.0)
            values <- c(values,raw)
            for(k in 2:100) if(is.finite(raw[k]) && raw[k]<=raw[k-1] && raw[k]<=raw[k+1])
              values <- c(values,optimize(profile,grid[c(k-1,k+1)],tol=1e-10)$objective)
          }
        }
        face_tol <- if(grepl('half',message) && !('mean' %in% held)) 1e-5 else 2e-3
        expect_true(-min(values)>=-min(refs)-face_tol*length(x),
          info=paste('held',paste(held,collapse=','),'n',length(x),message,
            'face',min(values),'reference',min(refs)))
      }
    } else {
      returned[j] <- returned[j]+1L
      expect_identical(unname(as.numeric(fit[held])),unname(p[held]))
      expect_setequal(attr(fit,'fixed'),held)
      expect_equal(as.numeric(logLik(fit)),sum(sstd_hold_logref(x,coef(fit))),tolerance=2e-8)
      expect_true(as.numeric(logLik(fit))>=-min(refs)-2e-5*length(x),
        info=paste('held',paste(held,collapse=','),'n',length(x),'first',x[1],
          'fit',paste(signif(coef(fit),6),collapse=','),'ll',as.numeric(logLik(fit)),'oracle',-min(refs)))
    }
  }
  # Each newly implemented interface must return an independently checked fit.
  expect_true(all(returned>0))
})

test_that("sstd P0b collapse, equality and moment constraints are enforced", {
  heavy <- c(rep(0,7),1,2)
  for(held in list(list(mean=0),list(sd=1),list(nu=3),list(xi=1),
    list(mean=0,sd=1),list(mean=0,nu=3),list(mean=0,xi=1),
    list(sd=1,xi=1),list(nu=3,xi=1),list(mean=0,sd=1,xi=1)))
    expect_error(do.call(mlsstd,c(list(x=heavy),held)),'unbounded')
  expect_error(mlsstd(c(rep(0,4),1,2),sd=1),'equality')
  for(side in c(-1,1))
    expect_error(mlsstd(side*c(.5,1,1.5,2,2.5,3,10),mean=0,sd=1,nu=5),'half-t')
})

test_that("sstd conditional optimizer failure stops the wrapper", {
  x <- c(-2,-1,-.4,0,.2,.3,.5,1,2,5)
  expect_error(mlsstd(x,sd=1,nu=4,iterlim=1),'failed.*score check')
})

test_that("sstd scalar certificates preserve ties, holds and nonunit scale branches", {
  expect_error(mlsstd(c(-2,2),sd=1,nu=3,xi=1),class="univariateML_tie")
  x <- c(-35.321072334324725,11.459074428000962,-.015351125824354262,
         -.2513099360462538,-297.3459307209368,.5881721220894813)
  f <- mlsstd(x,mean=0,nu=3,xi=10)
  expect_equal(log(unname(coef(f)["sd"])),5.894765235143508,tolerance=1e-8)
  expect_equal(as.numeric(logLik(f)),-38.8073025560888,tolerance=1e-11)
  expect_equal(unname(coef(f)[c("mean","nu","xi")]),c(0,3,10))
  expect_error(mlsstd(c(-2,2),sd=1,nu=3,xi=1,iterlim=1),class="univariateML_budget")
  expect_error(mlsstd(c(0,0,0,1),mean=0,nu=3,xi=1),class="univariateML_nomle")
  expect_error(mlsstd(c(0,0,0,1),mean=0,nu=3,xi=2),class="univariateML_numerical")
})

test_that("sstd held unit skew retains the certified symmetric closure", {
  expect_error(mlsstd(c(-2,-1,0,1,2),xi=1),class="univariateML_limit")
  expect_error(mlsstd(c(-2,-1,0,1,2),xi=1),"mlnorm")
})

test_that("sstd xi-only certificate compares the complete skew range", {
  x <- c(-2,-1,-.4,0,.2,.3,.5,1,2,5)
  f <- mlsstd(x,mean=0,sd=1,nu=5)
  expect_equal(unname(coef(f)[c("mean","sd","nu")]),c(0,1,5))
  reflected <- mlsstd(-x*1e-20,mean=0,sd=1e-20,nu=5)
  expect_equal(unname(coef(f)["xi"]*coef(reflected)["xi"]),1,tolerance=1e-9)
  expect_error(mlsstd(x,mean=0,sd=1,nu=5,iterlim=1),class="univariateML_budget")
  expect_error(mlsstd(c(-1,1),mean=0,sd=1,nu=5),class="univariateML_tie")
  expect_error(mlsstd(c(.5,1,1.5,2,2.5,3,10),mean=0,sd=1,nu=5),class="univariateML_limit")
})

test_that("sstd xi-only large held shape uses stable moments", {
  f <- mlsstd(c(-2,-1,0,1,3),mean=0,sd=1,nu=1e12)
  expect_true(is.finite(coef(f)["xi"]))
  expect_true(all(is.finite(vcov(f))))
  expect_true(vcov(f)[1,1] > 0)
  expect_equal(as.numeric(logLik(f)),sum(dsstd(c(-2,-1,0,1,3),0,1,1e12,coef(f)["xi"],log=TRUE)),tolerance=1e-12)
})

test_that("sstd large shapes retain the independent high-precision density", {
  for (nu in c(128,1e4,1e8,1e12,1e16,1e100,.Machine$double.xmax)) {
    expected <- switch(as.character(nu), "128"=.36125492616400606,
      "10000"=.3597599586665302, "1e+08"=.359740838771626,
      "1e+12"=.35974083685996705, .35974083685977598)
    expect_equal(dsstd(0,0,1,nu,2),expected,tolerance=5e-15)
    if (nu <= 1e16) {
      q <- qsstd(c(.1,.5,.9),0,1,nu,2)
      expect_equal(psstd(q,0,1,nu,2),c(.1,.5,.9),tolerance=1e-11)
    }
    expect_true(all(is.finite(rsstd(10,0,1,nu,2))))
  }
})

test_that("sstd refitted likelihood seam agrees at returned coefficients", {
  x <- c(0.999999999998,0.999999999999,1.0,1.000000000001,1.000000000003)
  f <- mlsstd(x,nu=2.9622019441967624,xi=1.9114750836827714)
  ll <- sum(dsstd(x,mean=f[["mean"]],sd=f[["sd"]],nu=f[["nu"]],xi=f[["xi"]],log=TRUE))
  expect_lte(abs(as.numeric(logLik(f))-ll),1e-10*max(1,abs(ll)))
})
