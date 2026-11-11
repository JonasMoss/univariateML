context("mlsnorm")

## Data generation.
set.seed(313)
small_data <- rsnorm(n = 100, mean = 0, sd = 1, xi = 1.5)
tiny_data <- rsnorm(n = 100, mean = 0, sd = 1, xi = 1.5)

## Finds errors with na and data out of bounds.
expect_error(mlsnorm(c(tiny_data, NA)))

## Checks that na.rm works as intended.
expect_equal(
  coef(mlsnorm(small_data)),
  coef(mlsnorm(c(small_data, NA), na.rm = TRUE))
)

## Is the log-likelihood correct?
est <- mlsnorm(small_data, na.rm = TRUE)
expect_equal(
  sum(dsnorm(x = small_data, mean = est[1], sd = est[2], xi = est[3], log = TRUE)),
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
expect_equal(attr(est, "model"), "Skew Normal")
expect_equal(class(est), "univariateML")



## Check support.
expect_equal(class(attr(est, "support"))[[1]], "numeric")

# Independent direct-sum profile oracle: grid, then refine every nominated
# local minimum. Endpoints are included explicitly, never approximated.
snorm_profile_reference <- function(x) {
  objective <- function(c) {
    d <- x - c
    (sum(d[d < 0]^2)^(1/3) + sum(d[d >= 0]^2)^(1/3))^3
  }
  grid <- seq(min(x), max(x), length.out = 2001)
  q <- vapply(grid, objective, numeric(1))
  candidates <- which(q[2:2000] <= q[1:1999] &
                      q[2:2000] <= q[3:2001]) + 1L
  modes <- c(min(x), max(x), vapply(candidates, function(i)
    optimize(objective, grid[c(i-1L, i+1L)], tol = 1e-12)$minimum,
    numeric(1)))
  values <- vapply(modes, objective, numeric(1))
  winner <- which.min(values)
  list(boundary = if (winner <= 2L) winner else 0L,
       logLik = length(x)/2 * (log(2/pi) - log(values[winner]/length(x)) - 1))
}

test_that("exact snorm profile agrees with 240 independent grid searches", {
  skip_unless_tier("full")
  set.seed(28012026)
  for (i in seq_len(240)) {
    n <- c(5L, 10L, 30L, 100L, 250L, 500L)[(i-1L) %% 6L + 1L]
    x <- switch((i-1L) %% 4L + 1L, rnorm(n), rexp(n),
                rsnorm(n = n, mean = 0, sd = 1, xi = 2.5), -rexp(n))
    reference <- snorm_profile_reference(x)
    if (reference$boundary != 0L) {
      expect_error(mlsnorm(x), if (reference$boundary == 1L)
        "mode at the sample minimum.*MLE does not exist" else
        "mode at the sample maximum.*MLE does not exist", info = paste("sample", i))
    } else {
      fit <- mlsnorm(x)
      expect_gte(attr(fit, "logLik"), reference$logLik - 1e-9)
      expect_equal(sum(dsnorm(x = x, mean = fit[1], sd = fit[2], xi = fit[3], log = TRUE)),
                   attr(fit, "logLik"), tolerance = 1e-12)
    }
  }
})

test_that("two-piece conversion matches fGarch log densities", {
  for (a in c(0.2, 1, 3)) for (b in c(0.3, 1, 4)) {
    c <- 0.7
    x <- c + c(-2*a, -0.1*a, 0, 0.1*b, 2*b)
    m1 <- sqrt(2/pi)
    mu <- c + m1*(b-a)
    sd <- sqrt(a*a-a*b+b*b-m1*m1*(b-a)^2)
    expected <- log(2/(a+b)) + dnorm((x-c)/ifelse(x<c,a,b), log=TRUE)
    expect_lt(max(abs(dsnorm(x = x, mean = mu, sd = sd, xi = sqrt(b/a), log = TRUE)-expected)), 1e-14)
  }
})

test_that("half-normal boundary messages name the available fitter", {
  x <- c(0, 0, 0, 1, 2, 8)
  expect_error(mlsnorm(x), "Use `mlhnorm` to fit")
  expect_error(mlsnorm(-x), "Use `mlhnorm` on `-x` \\(a half-normal reflected at the sample maximum\\)")
})

# Package-owned dpqr contract, independent of the optional reference package.
test_that("snorm dpqr integrate, invert and preserve extreme tails", {
  pars <- list(mean = .3, sd = 1.7, xi = 1.8)
  d <- function(x, ...) do.call(dsnorm, c(list(x), pars, list(...)))
  p <- function(q, ...) do.call(psnorm, c(list(q), pars, list(...)))
  q <- function(p, ...) do.call(qsnorm, c(list(p), pars, list(...)))
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

test_that("snorm dpqr recycle, propagate missing values and use R RNG", {
  expect_length(dsnorm(x = numeric(), mean = 0, sd = 1, xi = 1.5), 0)
  expect_equal(dsnorm(x = c(0, 1, 2), mean = c(0, 1), sd = 1, xi = 1.5),
    c(dsnorm(x = 0, mean = 0, sd = 1, xi = 1.5), dsnorm(x = 1, mean = 1, sd = 1, xi = 1.5), dsnorm(x = 2, mean = 0, sd = 1, xi = 1.5)))
  expect_identical(dsnorm(x = NA_real_, mean = 0, sd = 1, xi = 1.5), NA_real_)
  expect_true(is.nan(psnorm(q = NaN, mean = 0, sd = 1, xi = 1.5)))
  expect_warning(expect_true(is.nan(dsnorm(x = 0, mean = 0, sd = -1, xi = 1.5))), "NaNs produced")
  expect_warning(expect_true(is.nan(qsnorm(p = 1.1, mean = 0, sd = 1, xi = 1.5))), "NaNs produced")
  expect_warning(expect_true(is.nan(rsnorm(n = 1, mean = 0, sd = -1, xi = 1.5))), "NaNs produced")
  set.seed(81); first <- rsnorm(n = 100, mean = 0, sd = 1, xi = 1.5)
  set.seed(81); expect_identical(rsnorm(n = 100, mean = 0, sd = 1, xi = 1.5), first)
  expect_length(rsnorm(n = 1:7, mean = 0, sd = 1, xi = 1.5), 7)
  expect_length(rsnorm(n = 0, mean = 0, sd = 1, xi = 1.5), 0)
  expect_length(rsnorm(n = 3, mean = numeric(), sd = 1, xi = 1.5), 0)
  expect_identical(rsnorm(n = 1, mean = NA_real_, sd = 1, xi = 1.5), NA_real_)
  set.seed(712)
  draws <- rsnorm(n = 5000, mean = 0, sd = 1, xi = 1.5)
  for (prob in c(.1, .5, .9))
    expect_lt(abs(mean(draws <= qsnorm(p = prob, mean = 0, sd = 1, xi = 1.5)) - prob), .025)
  for (bad in c(-1, 0, Inf)) {
    expect_warning(expect_true(is.nan(dsnorm(x = 0, mean = 0, sd = bad, xi = 1.5))), "NaNs produced")
    expect_warning(expect_true(is.nan(psnorm(q = 0, mean = 0, sd = bad, xi = 1.5))), "NaNs produced")
    expect_warning(expect_true(is.nan(qsnorm(p = 0.5, mean = 0, sd = bad, xi = 1.5))), "NaNs produced")
  }
  fit <- mlsnorm(small_data)
  expect_equal(as.numeric(logLik(fit)), sum(dml(small_data, fit, log = TRUE)),
               tolerance = 1e-10)
  expect_equal(pml(0, fit), do.call(psnorm, c(list(0), as.list(coef(fit)))))
  expect_equal(qml(.2, fit), do.call(qsnorm, c(list(.2), as.list(coef(fit)))))
  set.seed(93); expected <- do.call(rsnorm, c(list(10), as.list(coef(fit))))
  set.seed(93); expect_identical(rml(10, fit), expected)
})

test_that("snorm matches frozen fGarch values (reference.json)", {
  expect_reference_values("snorm")
})

test_that("snorm skew parameter domain is enforced", {
  for (bad in c(-1, 0, Inf))
    expect_warning(expect_true(is.nan(dsnorm(x = 0, mean = 0, sd = 1, xi = bad))), "NaNs produced")
})

test_that("snorm log tails exceed the backend exponent range", {
  for (lower in c(TRUE, FALSE)) {
    x <- qsnorm(p = -20000, mean = 0, sd = 1, xi = 1.5, lower.tail = lower, log.p = TRUE)
    expect_true(is.finite(x))
    expect_equal(psnorm(q = x, mean = 0, sd = 1, xi = 1.5, lower.tail = lower, log.p = TRUE), -20000,
                 tolerance = 1e-10)
    x <- qsnorm(p = -1e-30, mean = 0, sd = 1, xi = 1.5, lower.tail = lower, log.p = TRUE)
    expect_equal(psnorm(q = x, mean = 0, sd = 1, xi = 1.5, lower.tail = lower, log.p = TRUE)/(-1e-30), 1,
                 tolerance = 1e-10)
  }
})

# Independent README density (R normal/t primitives and gamma functions).
snorm_hold_logref <- function(x,p) {
  xi <- p['xi']; nu <- p['nu']
  m <- sqrt(2/pi)
  v <- (1-m*m)*(xi^2+xi^(-2))+2*m*m-1
  z <- sqrt(v)*(x-p['mean'])/p['sd']+m*(xi-1/xi)
  z <- ifelse(z>0,z/xi,z*xi)
  core <- dnorm(z,log=TRUE)
  .5*log(v)-log(p['sd'])+log(2/(xi+1/xi))+core
}

test_that("snorm classified holds agree with independent multistart oracles", {
  skip_unless_tier("full")
  set.seed(81029)
  p <- c(mean=.2,sd=1.4,xi=1.3)
  subsets <- lapply(1:7,function(mask) names(p)[as.logical(intToBits(mask)[1:3])])
  samples <- list(rnorm(20),rnorm(100),round(rnorm(20),1),c(-1,0,1),
                  rexp(20),-rexp(100),abs(rnorm(20)),runif(100,-2,2))
  for(x in samples) for(held in subsets) {
    free <- setdiff(names(p),held)
    fit <- tryCatch(do.call(mlsnorm,c(list(x=x),as.list(p[held]))),error=identity)
    if(inherits(fit,'error')) {
      expect_match(conditionMessage(fit),'half-normal|unbounded|collapse|converge')
      next
    }
    expect_identical(unname(as.numeric(fit[held])),unname(p[held]))
    expect_setequal(attr(fit,'fixed'),held)
    expect_equal(as.numeric(logLik(fit)),sum(snorm_hold_logref(x,coef(fit))),tolerance=1e-9)
    if(!length(free)) next
    pack <- function(q) { q <- q[free]; q[names(q)!='mean'] <- log(q[names(q)!='mean']); unname(q) }
    objective <- function(t) {
      q <- p; q[free] <- t
      positive <- intersect(free,c('sd','xi'))
      q[positive] <- exp(q[positive])
      val <- -sum(snorm_hold_logref(x,q))
      if(is.finite(val)) val else 1e100
    }
    refs <- lapply(c(-.8,0,.8),function(delta)
      optim(pack(p)+delta,objective,method=if(length(free)==1) 'BFGS' else 'Nelder-Mead',
            control=list(maxit=3000,reltol=1e-12))$value)
    expect_gte(as.numeric(logLik(fit)),-min(unlist(refs))-1e-6*length(x))
  }
})

test_that("snorm holds validate values, ties, and all-held evaluation", {
  x <- c(-1,0,1)
  p <- c(mean=.2,sd=1.4,xi=1.3)
  for(v in names(p)) for(bad in c(NA_real_,Inf,-Inf)) {
    args <- as.list(p); args[[v]] <- bad
    expect_error(do.call(mlsnorm,c(list(x=x),args)), 'held')
  }
  args <- as.list(p)
  fit <- do.call(mlsnorm,c(list(x=c(0,0,0)),args))
  expect_equal(as.numeric(logLik(fit)),sum(snorm_hold_logref(c(0,0,0),p)),tolerance=1e-10)
  expect_error(mlsnorm(rep(.2,3),mean=.2,xi=1.3),'unbounded')
})

test_that("snorm normal member uses exact held normal solutions", {
  x <- c(-2,-.5,0,.1,1,3)
  for(m in list(NULL,.4)) for(s in list(NULL,1.7)) {
    fit <- mlsnorm(x,mean=m,sd=s,xi=1)
    mu <- if(is.null(m)) mean(x) else m
    sigma <- if(is.null(s)) sqrt(mean((x-mu)^2)) else s
    expect_equal(unname(fit['mean']),mu,tolerance=1e-14)
    expect_equal(unname(fit['sd']),sigma,tolerance=1e-14)
    expect_equal(as.numeric(logLik(fit)),sum(dnorm(x,mu,sigma,log=TRUE)),tolerance=1e-12)
  }
  for(v in c('sd','xi')) for(bad in c(0,-1)) {
    args <- list(x=x,mean=.2,sd=1.4,xi=1.3); args[[v]] <- bad
    expect_error(do.call(mlsnorm,args),'held')
  }
})

test_that("snorm held-moment endpoint faces retain shrinking-side penalties", {
  m <- sqrt(2/pi); h <- sqrt(1-m*m)
  for(side in c(-1,1)) {
    x <- side*c(0,.3,1,2)
    p <- c(mean=side*m,sd=h,xi=if(side==1) 1e4 else 1e-4)
    face <- sum(log(2)+dnorm(x,log=TRUE))-m*m/(8*h^4)
    expect_equal(sum(snorm_hold_logref(x,p)),face,tolerance=1e-6)
    objective <- function(lx) {
      p['xi'] <- exp(lx)
      -sum(snorm_hold_logref(x,p))
    }
    candidates <- vapply(c(-4,0,4),function(start)
      optim(start,objective,method='L-BFGS-B',lower=-10,upper=10)$value,0.)
    fit <- tryCatch(mlsnorm(x,mean=side*m,sd=h),error=identity)
    if(inherits(fit,'error')) {
      # At a rounded endpoint the fitted face can differ by one ulp from
      # the mathematical endpoint. Exhausting the search is an honest
      # numerical failure, not evidence that the boundary won.
      expect_match(conditionMessage(fit),'half-normal|did not converge')
      if (grepl('half-normal', conditionMessage(fit)))
        expect_gte(face,-min(candidates)-1e-6)
    } else expect_gte(as.numeric(logLik(fit)),-min(candidates)-1e-6)
  }
})

test_that("snorm R6 collapse diagnostics and held exceptions", {
  for (x in list(0,rep(0,3),rep(2,3))) expect_error(mlsnorm(x),"[Ss]cale collapse.*unbounded")
  expect_error(mlsnorm(rep(2,3),mean=2),"unbounded")
})

test_that("snorm certificate ties and numerical stops are structured", {
  tied <- tryCatch(mlsnorm(c(0,0,0),sd=1),error=identity)
  expect_s3_class(tied,"univariateML_tie")
  expect_equal(nrow(tied$candidates),2L)
  expect_setequal(tied$candidates$model,c("hnorm","unavailable:reflected-half-normal"))
  stopped <- tryCatch(mlsnorm(c(-2,-1,-.4,0,.2,.8,1,1.3,2,3),mean=.2,sd=1.4,iterlim=1),error=identity)
  expect_s3_class(stopped,"univariateML_numerical")
  expect_equal(stopped$loglik_upper,Inf)
  expect_error(mlsnorm(c(-1,0,1),iterlim=0),"iterlim")
})

test_that("snorm finite reflected modes retain both witnesses", {
  e <- tryCatch(mlsnorm(c(-1,0,1),mean=0,sd=.755),error=identity)
  expect_s3_class(e,"univariateML_tie")
  expect_equal(nrow(e$candidates),2L)
  expect_true(all(e$candidates$model==""))
  expect_equal(prod(e$candidates$xi),1,tolerance=1e-7)
})

test_that("snorm constant data away from a held mean stop on the constrained face", {
  m <- sqrt(2/pi)
  for (value in c(-2,2)) {
    e <- tryCatch(mlsnorm(rep(value,2),mean=0,iterlim=1),error=identity)
    expect_s3_class(e,"univariateML_limit")
    expect_equal(e$model,if(value<0) "hnorm" else "unavailable:reflected-half-normal")
    expect_equal(e$estimates,c(mu=value,sigma=2/m),tolerance=1e-12)
    expect_equal(e$limit_loglik,2*log(m*m/2),tolerance=1e-12)
  }
})

test_that("snorm refitted likelihood seam agrees at returned coefficients", {
  x <- c(-0.1400741601380333,-0.14006015552294254,-0.14004615090785175,-0.14003214629276095,-0.1400041370625794)
  f <- mlsnorm(x,mean=-0.020798452221064478,xi=3.394979982095818)
  ll <- sum(dsnorm(x,mean=f[["mean"]],sd=f[["sd"]],xi=f[["xi"]],log=TRUE))
  expect_lte(abs(as.numeric(logLik(f))-ll),1e-10*max(1,abs(ll)))
})
