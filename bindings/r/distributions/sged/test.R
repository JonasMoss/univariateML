context("mlsged")

## Deterministic interior data for successful-fit contracts.
set.seed(313)
small_data <- qsged(p = ((1:200) - 0.5)/200, mean = 2, sd = 3, nu = 4, xi = 1.5)
tiny_data <- qsged(p = ((1:100) - 0.5)/100, mean = 2, sd = 3, nu = 4, xi = 1.5)

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
  sum(dsged(x = small_data, mean = est[1], sd = est[2], nu = est[3], xi = est[4], log = TRUE)),
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
expect_equal(attr(f, "logLik"), sum(dsged(x = x, mean = f[1], sd = f[2], nu = 1, xi = f[4], log = TRUE)))
# Exact fGarch mapping of arbitrary two-piece Laplace parameters.
a <- .7; b <- 2; c <- -.3; grid <- seq(-4, 4, length.out=101)
expected <- -log(a+b) - ifelse(grid<c, (c-grid)/a, (grid-c)/b)
expect_equal(dsged(x = grid, mean = c + b - a, sd = sqrt(a * a + b * b), nu = 1, xi = sqrt(b/a), log = TRUE), expected)
expect_error(suppressWarnings(mlsged(-log(((0:99)+.5)/100)), classes = "univariateML_restricted"), "sample minimum")
expect_error(suppressWarnings(mlsged(log(((0:99)+.5)/100)), classes = "univariateML_restricted"), "sample maximum")

test_that("sged interior multistart oracle", {
  skip_unless_tier("full")
  set.seed(812)
  for (shape in c(1.5, 3)) {
    x <- rged(n = 500, mean = 0, sd = 1, nu = shape)
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

})

test_that("sged scale extremes preserve fit or declared escape", {
  set.seed(10)
  x <- ml_simulate("mlsged",50)
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
  skip_unless_tier("full")
  p<-((1:1000)-.5)/1000
  x<-qsged(p = p, mean = 0, sd = 1, nu = 3, xi = 2)
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
  expect_length(dsged(x = numeric(), mean = 0, sd = 1, nu = 2, xi = 1.5), 0)
  expect_equal(dsged(x = c(0, 1, 2), mean = c(0, 1), sd = 1, nu = 2, xi = 1.5),
    c(dsged(x = 0, mean = 0, sd = 1, nu = 2, xi = 1.5), dsged(x = 1, mean = 1, sd = 1, nu = 2, xi = 1.5), dsged(x = 2, mean = 0, sd = 1, nu = 2, xi = 1.5)))
  expect_identical(dsged(x = NA_real_, mean = 0, sd = 1, nu = 2, xi = 1.5), NA_real_)
  expect_true(is.nan(psged(q = NaN, mean = 0, sd = 1, nu = 2, xi = 1.5)))
  expect_warning(expect_true(is.nan(dsged(x = 0, mean = 0, sd = -1, nu = 2, xi = 1.5))), "NaNs produced")
  expect_warning(expect_true(is.nan(qsged(p = 1.1, mean = 0, sd = 1, nu = 2, xi = 1.5))), "NaNs produced")
  expect_warning(expect_true(is.nan(rsged(n = 1, mean = 0, sd = -1, nu = 2, xi = 1.5))), "NaNs produced")
  set.seed(81); first <- rsged(n = 100, mean = 0, sd = 1, nu = 2, xi = 1.5)
  set.seed(81); expect_identical(rsged(n = 100, mean = 0, sd = 1, nu = 2, xi = 1.5), first)
  expect_length(rsged(n = 1:7, mean = 0, sd = 1, nu = 2, xi = 1.5), 7)
  expect_length(rsged(n = 0, mean = 0, sd = 1, nu = 2, xi = 1.5), 0)
  expect_length(rsged(n = 3, mean = numeric(), sd = 1, nu = 2, xi = 1.5), 0)
  expect_identical(rsged(n = 1, mean = NA_real_, sd = 1, nu = 2, xi = 1.5), NA_real_)
  set.seed(712)
  draws <- rsged(n = 5000, mean = 0, sd = 1, nu = 2, xi = 1.5)
  for (prob in c(.1, .5, .9))
    expect_lt(abs(mean(draws <= qsged(p = prob, mean = 0, sd = 1, nu = 2, xi = 1.5)) - prob), .025)
  for (bad in c(-1, 0, Inf)) {
    expect_warning(expect_true(is.nan(dsged(x = 0, mean = 0, sd = bad, nu = 2, xi = 1.5))), "NaNs produced")
    expect_warning(expect_true(is.nan(psged(q = 0, mean = 0, sd = bad, nu = 2, xi = 1.5))), "NaNs produced")
    expect_warning(expect_true(is.nan(qsged(p = 0.5, mean = 0, sd = bad, nu = 2, xi = 1.5))), "NaNs produced")
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
    expect_warning(expect_true(is.nan(dsged(x = 0, mean = 0, sd = 1, nu = bad, xi = 1.5))), "NaNs produced")
})

test_that("sged skew parameter domain is enforced", {
  for (bad in c(-1, 0, Inf))
    expect_warning(expect_true(is.nan(dsged(x = 0, mean = 0, sd = 1, nu = 2, xi = bad))), "NaNs produced")
})

test_that("sged log tails exceed the backend exponent range", {
  for (lower in c(TRUE, FALSE)) {
    x <- qsged(p = -20000, mean = 0, sd = 1, nu = 2, xi = 1.5, lower.tail = lower, log.p = TRUE)
    expect_true(is.finite(x))
    expect_equal(psged(q = x, mean = 0, sd = 1, nu = 2, xi = 1.5, lower.tail = lower, log.p = TRUE), -20000,
                 tolerance = 1e-10)
    x <- qsged(p = -1e-30, mean = 0, sd = 1, nu = 2, xi = 1.5, lower.tail = lower, log.p = TRUE)
    expect_equal(psged(q = x, mean = 0, sd = 1, nu = 2, xi = 1.5, lower.tail = lower, log.p = TRUE)/(-1e-30), 1,
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
  skip_unless_tier("full")
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


test_that("sged P0b subsets agree with independent README oracles", {
  skip_unless_tier("full")
  set.seed(931029)
  p <- c(mean=.2, sd=1.4, nu=1.7, xi=1.3)
  subsets <- list('mean','sd','nu','xi',c('mean','sd'),c('mean','nu'),
    c('mean','xi'),c('sd','nu'),c('sd','xi'),c('nu','xi'),
    c('mean','sd','nu'),c('mean','sd','xi'))
  samples <- list(rnorm(20), rnorm(100), round(rnorm(20),1), c(-1,0,1),
    rexp(20), -rexp(20), abs(rt(20,3)), runif(100,-2,2))
  # Independent two-piece draws from the fitted family, in addition to
  # the tied, small and limit-law samples above.
  nu <- p['nu']; xi <- p['xi']
  loga <- .5*(lgamma(1/nu)-lgamma(3/nu))
  m <- exp(lgamma(2/nu)-.5*(lgamma(1/nu)+lgamma(3/nu)))
  for(n in c(20,100)) {
    z <- exp(loga)*rgamma(n,shape=1/nu)^(1/nu)
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
        q[nm] <- if(nm=='mean') theta[i] else if(nm=='nu') 1+exp(theta[i]) else exp(theta[i])
      }
      if(any(!is.finite(q)) || q['nu']>1e5) return(1e100)
      val <- -sum(sged_hold_logref(x,q))
      if(is.finite(val)) val else 1e100
    }
    pack <- function(q) vapply(free,function(nm)
      if(nm=='mean') q[nm] else if(nm=='nu') log(max(q[nm]-1,1e-8)) else log(q[nm]), 0.0)
    refs <- vapply(c(-.6,0,.6),function(delta) {
      initial <- pack(p)+delta
      a <- optim(initial, objective, method='BFGS',control=list(maxit=1500,reltol=1e-12))
      b <- if(length(free)==1) optim(initial, objective, method='Brent', lower=-18, upper=11) else
        optim(initial, objective, method='Nelder-Mead',control=list(maxit=2500,reltol=1e-12))
      min(a$value,b$value)
    },0.0)
    fit <- tryCatch(suppressWarnings(do.call(mlsged,
      c(list(x=x,gradtol=1e-6,iterlim=1000),as.list(p[held]))),
      classes='univariateML_restricted'),error=identity)
    if(inherits(fit,'error')) {
      if(inherits(fit,'univariateML_tie')) {
        expect_gte(nrow(fit$candidates),2L)
        expect_true(is.finite(fit$loglik_upper))
        expect_gte(fit$loglik_upper,-min(refs)-1e-5*length(x))
        for(index in seq_len(nrow(fit$candidates))) {
          row <- fit$candidates[index,]
          if(row$model=='') {
            q <- c(mean=row$mean,sd=row$sd,nu=row$nu,xi=row$xi)
            value <- sum(sged_hold_logref(x,q))
          } else if(row$model %in% c('unavailable:half-ged','unavailable:reflected-half-ged')) {
            side <- if(row$model=='unavailable:half-ged') 1 else -1
            z <- side*(x-row$mu)/row$scale
            expect_true(all(z>=0))
            value <- length(x)*(log(row$nu)-lgamma(1/row$nu)-log(row$scale))-sum(z^row$nu)
          } else if(row$model=='unif') {
            expect_true(all(x>=row$a & x<=row$b))
            value <- -length(x)*log(row$b-row$a)
          } else stop('Unexpected SGED comparison model in independent oracle test')
          expect_gte(value,row$loglik_lower-1e-8*length(x))
          expect_lte(value,row$loglik_upper+1e-8*length(x))
        }
        next
      }
      if(identical(held,c('mean','sd','nu')) && inherits(fit,c('univariateML_numerical','univariateML_budget'))) {
        expect_gte(nrow(fit$candidates),1L)
        expect_identical(fit$loglik_upper,Inf)
        next
      }
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
            theta[pos] <- if(grepl('nu -> 2',message)) log(1e-7) else log(60000-1)
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
          m <- exp(lgamma(2/nu)-lgamma(1/nu))
          v <- exp(lgamma(3/nu)-lgamma(1/nu))
          scale <- if('sd' %in% held) p['sd']/sqrt(v-m*m) else
            exp((log(nu)+log(mean(y^nu)))/nu)
          -sum(log(nu)-lgamma(1/nu)-log(scale)-(y/scale)^nu)
          }
          if('nu' %in% held) values <- c(values,face_at_shape(p['nu'])) else {
            profile <- function(t) {
              val <- face_at_shape(exp(t)); if(is.finite(val)) val else 1e100
            }
            grid <- seq(0,11,length.out=101)
            raw <- vapply(grid,profile,0.0)
            values <- c(values,raw)
            for(k in 2:100) if(is.finite(raw[k]) && raw[k]<1e90 && raw[k]<=raw[k-1] && raw[k]<=raw[k+1])
              values <- c(values,optimize(profile,grid[c(k-1,k+1)],tol=1e-10)$objective)
          }
        }
        if(grepl('half',message) && 'mean' %in% held) {
          side <- if(grepl('reflected|sample maximum',message)) -1 else 1
          d <- side*(x-p['mean'])
          face_at_shape <- function(nu) {
            m <- exp(lgamma(2/nu)-lgamma(1/nu))
            v <- exp(lgamma(3/nu)-lgamma(1/nu))
            at_scale <- function(lt) {
              w <- m+d/exp(lt)
              if(any(w < -1e-12)) return(1e100)
              val <- length(x)*(lt-log(nu)+lgamma(1/nu))+sum(pmax(0,w)^nu)
              if(is.finite(val)) val else 1e100
            }
            if('sd' %in% held) return(at_scale(log(p['sd']/sqrt(v-m*m))))
            lo <- log(max(exp(-20),max(-d/m)))
            min(at_scale(lo),optimize(at_scale,c(lo,20),tol=1e-10)$objective)
          }
          if('nu' %in% held) values <- c(values,face_at_shape(p['nu'])) else {
            profile <- function(t) {
              val <- face_at_shape(exp(t)); if(is.finite(val)) val else 1e100
            }
            grid <- seq(0,11,length.out=101)
            raw <- vapply(grid,profile,0.0)
            values <- c(values,raw)
            for(k in 2:100) if(is.finite(raw[k]) && raw[k]<1e90 && raw[k]<=raw[k-1] && raw[k]<=raw[k+1])
              values <- c(values,optimize(profile,grid[c(k-1,k+1)],tol=1e-10)$objective)
          }
        }
        face_tol <- if(grepl('half',message)) 1e-5 else 2e-3
        expect_true(-min(values)>=-min(refs)-face_tol*length(x),
          info=paste('held',paste(held,collapse=','),'n',length(x),message,
            'face',min(values),'reference',min(refs)))
      }
    } else {
      returned[j] <- returned[j]+1L
      expect_identical(unname(as.numeric(fit[held])),unname(p[held]))
      expect_setequal(attr(fit,'fixed'),held)
      expect_equal(as.numeric(logLik(fit)),sum(sged_hold_logref(x,coef(fit))),tolerance=2e-8)
      expect_true(as.numeric(logLik(fit))>=-min(refs)-2e-5*length(x),
        info=paste('held',paste(held,collapse=','),'n',length(x),'first',x[1],
          'fit',paste(signif(coef(fit),6),collapse=','),'ll',as.numeric(logLik(fit)),'oracle',-min(refs)))
    }
  }
  # Each newly implemented interface must return an independently checked fit.
  expect_true(all(returned>0))
})

test_that("sged P0b collapse, restriction and forced half faces are enforced", {
  for(held in list(list(mean=0),list(nu=2),list(xi=1),list(mean=0,nu=2),
    list(mean=0,xi=1),list(nu=2,xi=1)))
    expect_error(do.call(mlsged,c(list(x=rep(0,3)),held)),'unbounded')
  x <- c(-10,-.5,-.2,-.1,0,.1,.2,.5,10)
  expect_warning(fit <- mlsged(x,mean=0,sd=1,xi=1),class='univariateML_restricted')
  expect_identical(unname(coef(fit)['nu']),1)
  expect_identical(attr(fit,'restricted'),c(nu='lower'))
  held <- mlsged(x,nu=.8,xi=1)
  expect_identical(unname(coef(held)['nu']),.8)
  expect_length(attr(held,'restricted'),0)
  for(side in c(-1,1))
    expect_error(mlsged(side*c(.5,1,1.5,2,2.5,3,10),mean=0,sd=1,nu=2),'half-GED')
})

test_that("sged rounded uniform endpoints stop while finite-shape penalties remain", {
  x <- c(-sqrt(3),sqrt(3))
  expect_error(mlsged(x,sd=1,xi=1),class='univariateML_numerical')
  expect_error(mlsged(x,mean=0,sd=1,xi=1),class='univariateML_numerical')
  # Direct README limit with both moments pinned, independently evaluated.
  target <- -2*log(sqrt(12))-2*exp(digamma(1))
  p <- c(mean=0,sd=1,nu=100000,xi=1)
  expect_equal(sum(sged_hold_logref(x,p)),target,tolerance=1e-4)
})

test_that("sged conditional failed iterates are never returned as fits", {
  x <- c(-2,-1,-.4,0,.2,.3,.5,1,2,5)
  expect_error(mlsged(x,sd=1,nu=1.7,iterlim=1),'failed.*score check|half-GED')
})

test_that("sged certified normal submodels agree with sufficient statistics", {
  x <- c(-3,-1,.2,.4,1,2,4)
  joint <- mlsged(x,nu=2,xi=1)
  expect_equal(unname(coef(joint)[c('mean','sd')]),c(mean(x),sqrt(mean((x-mean(x))^2))),tolerance=2e-8)
  held <- mlsged(x,mean=0,nu=2,xi=1)
  expect_equal(unname(coef(held)['sd']),sqrt(mean(x^2)),tolerance=2e-12)
  expect_identical(unname(coef(held)['mean']),0)
  expect_equal(as.numeric(logLik(joint)),sum(dnorm(x,mean(x),sqrt(mean((x-mean(x))^2)),log=TRUE)),tolerance=2e-10)
})

test_that("sged held-shape ties and budgets have structured witnesses", {
  for(nu in c(.5,1)) for(sd in list(NULL,1)) {
    e <- tryCatch(mlsged(c(-1,1),sd=sd,nu=nu,xi=1),error=identity)
    expect_s3_class(e,'univariateML_tie')
    expect_equal(nrow(e$candidates),2L)
  }
  e <- tryCatch(mlsged(0:139,sd=1,nu=.7,xi=1.3,iterlim=1),error=identity)
  expect_s3_class(e,'univariateML_budget')
  expect_true(nrow(e$candidates)>0)
  expect_error(mlsged(c(-3,-1,.2,.4,1,2,4),sd=1,nu=2,xi=1,iterlim=1),class='univariateML_numerical')
  expect_error(mlsged(c(-1,0,1),sd=1,nu=.001,xi=1),class='univariateML_numerical')
})

test_that("sged asymmetric held-shape scale certificate matches a scalar reference", {
  x <- c(-3,-1,-.2,0,.3,.4,2)
  for(nu in c(1,1.1,2,4,20)) for(xi in c(.3,1.3,3)) {
    f <- mlsged(x,mean=.1,nu=nu,xi=xi)
    ref <- optimize(function(ls)-sum(sged_hold_logref(x,c(mean=.1,sd=exp(ls),nu=nu,xi=xi))),c(-5,5),tol=1e-10)
    expect_equal(unname(coef(f)['sd']),exp(ref$minimum),tolerance=2e-7,info=paste(nu,xi))
    # A Laplace join has first-order likelihood loss in scale error.
    expect_equal(as.numeric(logLik(f)),-ref$objective,tolerance=if(nu==1) 2e-8 else 2e-9,info=paste(nu,xi))
  }
  expect_error(mlsged(x,mean=.1,nu=2,xi=3,iterlim=1),class='univariateML_numerical')
})

test_that("SGED held subunit asymmetric scale agrees with the density", {
  x <- c(-3,-1,-.2,0,.3,.4,2)
  for (nu in c(.3,.5,.9)) for (xi in c(.3,1.3,3)) {
    fit <- mlsged(x, mean=.1, nu=nu, xi=xi)
    expect_identical(as.numeric(fit[c("mean","nu","xi")]), c(.1,nu,xi))
    expect_equal(as.numeric(logLik(fit)), sum(dml(x,fit,log=TRUE)), tolerance=2e-7)
    expect_setequal(attr(fit,"fixed"),c("mean","nu","xi"))
  }
})

test_that("SGED subunit separated scale maxima and budgets are structured stops", {
  R <- uniroot(function(r) log(r)-2*(1-1/r),c(4,6),tol=1e-13)$root
  K <- 2*sqrt(R-1)/R
  xi <- sqrt(1+K^2/6)
  e <- tryCatch(mlsged(rep(-1,3),mean=0,nu=.5,xi=xi,iterlim=400),error=identity)
  expect_s3_class(e,"univariateML_tie")
  expect_gte(nrow(e$candidates),2L)
  expect_equal(max(e$candidates$sd)/min(e$candidates$sd),R,tolerance=2e-6)
  expect_true(is.finite(e$loglik_upper))
  for (j in seq_len(nrow(e$candidates))) {
    row <- e$candidates[j,]
    ref <- mlsged(rep(-1,3),mean=0,sd=row$sd,nu=.5,xi=xi)
    expect_gte(as.numeric(logLik(ref))+1e-8,row$loglik_lower)
    expect_lte(as.numeric(logLik(ref))-1e-8,row$loglik_upper)
  }
  e <- tryCatch(mlsged(-1-(0:159)*.1,mean=0,nu=.5,xi=1.3,iterlim=1),error=identity)
  expect_s3_class(e,"univariateML_budget")
  expect_gte(nrow(e$candidates),1L)
  expect_s3_class(tryCatch(mlsged(rep(0,3),mean=0,nu=.5,xi=1.3),error=identity),"univariateML_nomle")
})

test_that("sged held normal shape delegates in public moment coordinates", {
  x <- c(-3,-1,-.5,0,.1,.2,.4,1,2,4)
  for (held in list(list(),list(mean=.2),list(sd=1.4),list(mean=.2,sd=1.4))) {
    a <- tryCatch(do.call(mlsged,c(list(x=x,nu=2,iterlim=1000),held)),error=identity)
    b <- tryCatch(do.call(mlsnorm,c(list(x=x,iterlim=1000),held)),error=identity)
    if(inherits(b,"error")) expect_s3_class(a,"error") else {
      expect_equal(as.numeric(a[c('mean','sd','xi')]),as.numeric(b),tolerance=1e-12)
      expect_equal(as.numeric(logLik(a)),as.numeric(logLik(b)),tolerance=1e-10)
      expect_setequal(attr(a,'fixed'),c(names(held),'nu'))
    }
  }
})
test_that("sged free skew subunit enumerates knots and both faces", {
  for(nu in c(.3,.5,.9,1)) {
    e <- tryCatch(mlsged(c(-1,1),nu=nu),error=identity)
    expect_s3_class(e,'univariateML_tie')
    expect_equal(nrow(e$candidates),2L)
  }
  x <- c(-4,-1,rep(0,6),.3,2)
  for(nu in c(.5,.8,1)) {
    f <- mlsged(x,nu=nu)
    expect_equal(as.numeric(logLik(f)),sum(dml(x,f,log=TRUE)),tolerance=2e-7)
  }
})

test_that("sged normal face conversion reports scale overflow numerically", {
  expect_error(mlsged(rep(0,3),mean=1.1e308,nu=2),class='univariateML_numerical')
})

test_that("sged normal face conversion rejects rounded subnormal scales", {
  expect_error(mlsged(c(0,2^(-1047)),nu=2),class='univariateML_numerical')
})

test_that("SGED held unit skew transports the four free-shape GED fits", {
  skip_if_not_installed("jsonlite")
  root <- Sys.getenv("UMVL_DISTRIBUTIONS_DIR")
  if(!nzchar(root)) root <- testthat::test_path("fixtures")
  g <- jsonlite::fromJSON(file.path(root,"ged","golden.json"))
  for (held in list(list(),list(mean=g$estimates$mean),list(sd=g$estimates$sd),list(mean=g$estimates$mean,sd=g$estimates$sd))) {
    a <- do.call(mlsged,c(list(x=g$data,xi=1),held))
    b <- do.call(mlged,c(list(x=g$data),held))
    expect_identical(as.numeric(a[c("mean","sd","nu")]),as.numeric(b))
    expect_identical(as.numeric(logLik(a)),as.numeric(logLik(b)))
    expect_identical(unname(a['xi']),1)
    expect_setequal(attr(a,'fixed'),c(names(held),'xi'))
    expect_equal(as.numeric(logLik(a)),sum(dml(g$data,a,log=TRUE)),tolerance=2e-9)
  }
})
test_that("SGED symmetric free-shape stops carry SGED and uniform coordinates", {
  e <- tryCatch(mlsged(c(-1,1),sd=.1,xi=1,iterlim=1),error=identity)
  expect_s3_class(e,'univariateML_tie')
  expect_equal(e$candidates$xi,c(1,1))
  expect_equal(e$candidates$nu,c(1,1))
  e <- tryCatch(mlsged(c(-1,1),xi=1),error=identity)
  expect_s3_class(e,'univariateML_limit')
  expect_identical(e$model,'unif')
  expect_equal(unname(e$estimates),c(-1,1))
  for (held in list(list(sd=1),list(mean=0,sd=1)))
    expect_error(do.call(mlsged,c(list(x=c(-sqrt(3),sqrt(3)),xi=1),held)),class='univariateML_numerical')
  expect_error(mlsged(rep(2,3),xi=1),class='univariateML_nomle')
  fit <- suppressWarnings(mlsged(c(-3,-1,0,1,4),sd=.1,xi=1,iterlim=1))
  expect_identical(unname(fit['nu']),1)
  expect_identical(unname(attr(fit,'restricted')['nu']),'lower')
})
test_that("SGED balanced two-point likelihood has a certified uniform supremum", {
  for (xi in list(NULL,.1,.4,2,100)) {
    e <- tryCatch(mlsged(c(-2,-2,3,3),xi=xi,iterlim=1),error=identity)
    expect_s3_class(e,'univariateML_limit')
    expect_identical(e$model,'unif')
    expect_equal(unname(e$estimates),c(-2,3))
    expect_equal(e$limit_loglik,-4*log(5),tolerance=1e-12)
  }
})

test_that("SGED asymmetric held skew and smooth free skew use global free-moment profiles", {
  skip_if_not_installed('jsonlite')
  root <- Sys.getenv('UMVL_DISTRIBUTIONS_DIR');if(!nzchar(root))root <- testthat::test_path('fixtures')
  g <- jsonlite::fromJSON(file.path(root,'sged','golden.json'))
  weighted <- mlsged(g$data,xi=1.4)
  expect_equal(unname(weighted['nu']),3.12977540545,tolerance=2e-8)
  expect_equal(as.numeric(logLik(weighted)),-649.7033316424115,tolerance=2e-9)
  for (held in list(list(xi=1.4),list(nu=1.7),list(nu=4))) {
    f <- do.call(mlsged,c(list(x=g$data),held))
    expect_equal(as.numeric(logLik(f)),sum(dml(g$data,f,log=TRUE)),tolerance=2e-8)
    expect_setequal(attr(f,'fixed'),names(held))
    expect_error(do.call(mlsged,c(list(x=g$data,iterlim=1),held)),class='univariateML_stop')
  }
})
test_that("SGED profile certificates preserve weighted median and reflected face ties", {
  x <- c(-100,rep(-.01,3),rep(.01,15),100)
  e <- tryCatch(mlsged(x,xi=2),error=identity)
  expect_s3_class(e,'univariateML_tie')
  expect_true(all(e$candidates$nu==1))
  expect_true(all(e$candidates$xi==2))
  expect_gte(length(unique(e$candidates$mean)),2)
  for(p in c(1.1,1.7,4,20)) {
    e <- tryCatch(mlsged(c(-1,1),nu=p),error=identity)
    expect_s3_class(e,'univariateML_tie')
    expect_setequal(e$candidates$model,c('unavailable:half-ged','unavailable:reflected-half-ged'))
    expect_true(is.finite(e$loglik_upper))
  }
})

test_that("sged fully held evaluation retains underflowed residual powers", {
  x <- c(-1e-100, 1e-100)
  for (xi in c(1,1.3)) {
    f <- mlsged(x,mean=0,sd=1e300,nu=.02,xi=xi)
    target <- if(xi==1) -612.224579676587918 else -683.90356286107067
    expect_equal(dml(x,f,log=TRUE),rep(target,2),tolerance=2e-12)
    expect_equal(dml(x,f)/exp(target),c(1,1),tolerance=2e-12)
    expect_equal(as.numeric(logLik(f)),2*target,tolerance=2e-12)
    expect_setequal(attr(f,"fixed"),c("mean","sd","nu","xi"))
  }
  f <- mlsged(x,mean=0,sd=1e300,nu=.0005,xi=1)
  for (lower in c(TRUE,FALSE)) {
    lp <- pml(x,f,lower.tail=lower,log.p=TRUE)
    expect_equal(lp,if(lower) c(-41.209468768459971,-1.2675208492151555e-18)
                  else c(-1.2675208492151555e-18,-41.209468768459971),tolerance=2e-10)
    expect_equal(qml(lp,f,lower.tail=lower,log.p=TRUE)/x,c(1,1),tolerance=2e-10)
  }
})

test_that("sged fully held tiny-shape evaluation preserves its centering moment", {
  for (xi in c(.7,1.3)) {
    x <- c(0,1e-100)
    f <- mlsged(x,mean=0,sd=1e300,nu=.0003,xi=xi)
    target <- if(xi<1) -107.064723180909907 else -106.798025104960350
    expect_equal(dml(x,f,log=TRUE),rep(target,2),tolerance=2e-11)
    expect_equal(as.numeric(logLik(f)),2*target,tolerance=2e-11)
    quantile <- if(xi<1) 9.98017740400194973e-80 else -7.94579776743636934e-80
    expect_equal(qml(.1,f)/quantile,1,tolerance=2e-10)
    expect_equal(pml(x,f,lower.tail=xi<1,log.p=TRUE),
                 rep(if(xi<1) -288.220156276318788 else -288.181208135391239,2),
                 tolerance=2e-11)
  }
})


test_that("SGED fixed-shape held-moment skew fits use the constrained global cover", {
  x <- c(-3,-1,-.2,0,.3,.4,2)
  for(p in c(1,1.7)) {
    f <- mlsged(x,mean=.1,sd=1.4,nu=p,gradtol=1e-10)
    ref <- optimize(function(t)-sum(sged_hold_logref(x,c(mean=.1,sd=1.4,nu=p,xi=exp(t)))),c(-2,2),tol=1e-11)
    expect_identical(as.numeric(f[c('mean','sd','nu')]),c(.1,1.4,p))
    expect_setequal(attr(f,'fixed'),c('mean','sd','nu'))
    expect_equal(unname(f['xi']),exp(ref$minimum),tolerance=2e-7)
    expect_equal(as.numeric(logLik(f)),-ref$objective,tolerance=2e-10)
    expect_equal(as.numeric(logLik(f)),sum(dml(x,f,log=TRUE)),tolerance=2e-10)
  }
  expect_error(mlsged(x,mean=.1,sd=1.4,nu=1.7,iterlim=1),class='univariateML_budget')
  expect_error(mlsged(x,mean=.1,sd=1.4,nu=1e308),class='univariateML_numerical')
})

test_that("SGED held moments retain the two finite Laplace skew maxima", {
  e <- tryCatch(mlsged(c(-1.1,1.1),mean=0,sd=1,nu=1,gradtol=1e-9),error=identity)
  expect_s3_class(e,'univariateML_tie')
  expect_gte(nrow(e$candidates),2L)
  expect_true(is.finite(e$loglik_upper))
  expect_true(all(e$candidates$mean==0 & e$candidates$sd==1 & e$candidates$nu==1))
  expect_equal(min(e$candidates$xi)*max(e$candidates$xi),1,tolerance=2e-8)
})

test_that("SGED held moments certify strict-support half-GED winners", {
  p <- 20
  k <- exp(lgamma(2/p)-lgamma(1/p)); v <- exp(lgamma(3/p)-lgamma(1/p))
  for(sign in c(-1,1)) {
    x <- rep(sign*.2,2)
    e <- tryCatch(mlsged(x,mean=0,sd=1,nu=p,gradtol=1e-10),error=identity)
    expect_s3_class(e,'univariateML_limit')
    expect_identical(e$model,if(sign>0) 'unavailable:reflected-half-ged' else 'unavailable:half-ged')
    c <- e$estimates
    expect_equal(unname(c['mu']-sign*k*c['scale']),0,tolerance=1e-12)
    expect_equal(unname(c['scale']*sqrt(v-k*k)),1,tolerance=1e-12)
    reference <- length(x)*(log(p)-lgamma(1/p)-log(c['scale']))-sum((-sign*(x-c['mu'])/c['scale'])^p)
    expect_equal(e$limit_loglik,unname(reference),tolerance=1e-10)
    expect_true(is.finite(e$loglik_upper))
  }
  e <- tryCatch(mlsged(c(0,0),mean=0,sd=1,nu=p,gradtol=1e-10),error=identity)
  expect_s3_class(e,'univariateML_tie')
  expect_setequal(e$candidates$model,c('unavailable:half-ged','unavailable:reflected-half-ged'))
})

test_that("SGED held moment half-GED endpoint contacts remain explicit stops", {
  expect_error(mlsged(c(-1,.2),mean=0,sd=1,nu=1,gradtol=1e-10),class='univariateML_numerical')
})

test_that("sged large shapes retain the high-precision centering moment", {
  for (nu in c(1e8,1e16,1e100,.Machine$double.xmax))
    expect_equal(dsged(0,0,1,nu,2),.2886751345948128822545743902509787278,tolerance=5e-15)
})
