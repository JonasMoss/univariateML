context("mlged")

## Data generation.
set.seed(313)
small_data <- rged(n = 100, mean = 2, sd = 3, nu = 4)
tiny_data <- rged(n = 100, mean = 2, sd = 3, nu = 4)

## Finds errors with na and data out of bounds.
expect_error(suppressWarnings(mlged(c(tiny_data, NA)), classes = "univariateML_restricted"))

## Checks that na.rm works as intended.
expect_equal(
  coef(suppressWarnings(mlged(small_data), classes = "univariateML_restricted")),
  coef(suppressWarnings(mlged(c(small_data, NA), na.rm = TRUE), classes = "univariateML_restricted"))
)

## Is the log-likelihood correct?
est <- suppressWarnings(mlged(small_data, na.rm = TRUE), classes = "univariateML_restricted")
expect_equal(
  sum(dged(x = small_data, mean = est[1], sd = est[2], nu = est[3], log = TRUE)),
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
expect_equal(attr(est, "model"), "Generalized Error")
expect_equal(class(est), "univariateML")



## Check support.
expect_equal(class(attr(est, "support"))[[1]], "numeric")

# Decision D6: attained Laplace, uniform and constrained interior oracle.
expect_error(suppressWarnings(mlged(seq(-1, 1, length.out = 40)), classes = "univariateML_restricted"), "Use `mlunif`")
x <- c(seq(-.02, .02, length.out = 19), -10, 10)
f <- suppressWarnings(mlged(x), classes = "univariateML_restricted")
expect_equal(unname(f["nu"]), 1)
expect_equal(attr(f, "logLik"), sum(dged(x = x, mean = f[1], sd = f[2], nu = 1, log = TRUE)))
a <- mean(abs(x-median(x)))
expect_equal(attr(f, "logLik"), -length(x)*(log(2*a)+1))

set.seed(812)
for (shape in c(1.5, 3)) {
  x <- rged(n = 500, mean = 0, sd = 1, nu = shape)
  f <- suppressWarnings(mlged(x, gradtol=1e-6, iterlim=1000), classes = "univariateML_restricted")
  expect_gt(unname(f["nu"]), 1)
  objective <- function(p) {
    nu <- 1+exp(p[3])
    lambda <- exp((-2/nu*log(2)+lgamma(1/nu)-lgamma(3/nu))/2)
    z <- (x-p[1])/exp(p[2])
    factor <- -p[2]
    value <- -sum(factor+log(nu)-log(lambda)-(1+1/nu)*log(2)-
                    lgamma(1/nu)-.5*(abs(z)/lambda)^nu)
    if (is.finite(value)) value else 1e100
  }
  values <- sapply(c(1.1,1.5,2,4,8), function(nu) {
    optim(c(mean(x), log(sd(x)), log(nu-1)), objective,
          method="Nelder-Mead", control=list(maxit=10000, reltol=1e-11))$value
  })
  expect_gte(attr(f,"logLik") + 1e-4, -min(values))
}

test_that("ged scale extremes preserve fit or declared escape", {
  set.seed(10)
  x <- ml_simulate("mlged",50)
  base <- tryCatch(suppressWarnings(mlged(x), classes = "univariateML_restricted"),error=function(e) conditionMessage(e))
  for(s in c(1e-300,1e300)) {
    f <- tryCatch(suppressWarnings(mlged(x*s), classes = "univariateML_restricted"),error=function(e) conditionMessage(e))
    if(is.character(base)) expect_identical(f,base) else {
      expect_false(is.character(f))
      if(!is.character(f)) expect_equal(attr(f,"logLik")+length(x)*log(s),attr(base,"logLik"),tolerance=1e-4)
    }
  }
})

test_that("ged large-sample fitting preserves likelihood under replication", {
  skip_unless_tier("full")
  p<-((1:1000)-.5)/1000
  x<-qged(p = p, mean = 0, sd = 1, nu = 3)
  base<-suppressWarnings(mlged(x), classes = "univariateML_restricted")
  f<-suppressWarnings(mlged(rep(x,100)), classes = "univariateML_restricted")
  expect_equal(attr(f,"logLik")/100,attr(base,"logLik"),tolerance=1e-5)
})

# Package-owned dpqr contract, independent of the optional reference package.
test_that("ged dpqr integrate, invert and preserve extreme tails", {
  pars <- list(mean = .3, sd = 1.7, nu = 1.4)
  d <- function(x, ...) do.call(dged, c(list(x), pars, list(...)))
  p <- function(q, ...) do.call(pged, c(list(q), pars, list(...)))
  q <- function(p, ...) do.call(qged, c(list(p), pars, list(...)))
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

test_that("ged dpqr recycle, propagate missing values and use R RNG", {
  expect_length(dged(x = numeric(), mean = 0, sd = 1, nu = 2), 0)
  expect_equal(dged(x = c(0, 1, 2), mean = c(0, 1), sd = 1, nu = 2),
    c(dged(x = 0, mean = 0, sd = 1, nu = 2), dged(x = 1, mean = 1, sd = 1, nu = 2), dged(x = 2, mean = 0, sd = 1, nu = 2)))
  expect_identical(dged(x = NA_real_, mean = 0, sd = 1, nu = 2), NA_real_)
  expect_true(is.nan(pged(q = NaN, mean = 0, sd = 1, nu = 2)))
  expect_warning(expect_true(is.nan(dged(x = 0, mean = 0, sd = -1, nu = 2))), "NaNs produced")
  expect_warning(expect_true(is.nan(qged(p = 1.1, mean = 0, sd = 1, nu = 2))), "NaNs produced")
  expect_warning(expect_true(is.nan(rged(n = 1, mean = 0, sd = -1, nu = 2))), "NaNs produced")
  set.seed(81); first <- rged(n = 100, mean = 0, sd = 1, nu = 2)
  set.seed(81); expect_identical(rged(n = 100, mean = 0, sd = 1, nu = 2), first)
  expect_length(rged(n = 1:7, mean = 0, sd = 1, nu = 2), 7)
  expect_length(rged(n = 0, mean = 0, sd = 1, nu = 2), 0)
  expect_length(rged(n = 3, mean = numeric(), sd = 1, nu = 2), 0)
  expect_identical(rged(n = 1, mean = NA_real_, sd = 1, nu = 2), NA_real_)
  set.seed(712)
  draws <- rged(n = 5000, mean = 0, sd = 1, nu = 2)
  for (prob in c(.1, .5, .9))
    expect_lt(abs(mean(draws <= qged(p = prob, mean = 0, sd = 1, nu = 2)) - prob), .025)
  for (bad in c(-1, 0, Inf)) {
    expect_warning(expect_true(is.nan(dged(x = 0, mean = 0, sd = bad, nu = 2))), "NaNs produced")
    expect_warning(expect_true(is.nan(pged(q = 0, mean = 0, sd = bad, nu = 2))), "NaNs produced")
    expect_warning(expect_true(is.nan(qged(p = 0.5, mean = 0, sd = bad, nu = 2))), "NaNs produced")
  }
  fit <- suppressWarnings(mlged(small_data), classes = "univariateML_restricted")
  expect_equal(as.numeric(logLik(fit)), sum(dml(small_data, fit, log = TRUE)),
               tolerance = 1e-10)
  expect_equal(pml(0, fit), do.call(pged, c(list(0), as.list(coef(fit)))))
  expect_equal(qml(.2, fit), do.call(qged, c(list(.2), as.list(coef(fit)))))
  set.seed(93); expected <- do.call(rged, c(list(10), as.list(coef(fit))))
  set.seed(93); expect_identical(rml(10, fit), expected)
})

test_that("ged matches frozen fGarch values (reference.json)", {
  expect_reference_values("ged")
})

test_that("ged parameter domain is enforced", {
  for (bad in c(-1, 0, Inf))
    expect_warning(expect_true(is.nan(dged(x = 0, mean = 0, sd = 1, nu = bad))), "NaNs produced")
})

test_that("ged log tails exceed the backend exponent range", {
  for (lower in c(TRUE, FALSE)) {
    x <- qged(p = -20000, mean = 0, sd = 1, nu = 2, lower.tail = lower, log.p = TRUE)
    expect_true(is.finite(x))
    expect_equal(pged(q = x, mean = 0, sd = 1, nu = 2, lower.tail = lower, log.p = TRUE), -20000,
                 tolerance = 1e-10)
    x <- qged(p = -1e-30, mean = 0, sd = 1, nu = 2, lower.tail = lower, log.p = TRUE)
    expect_equal(pged(q = x, mean = 0, sd = 1, nu = 2, lower.tail = lower, log.p = TRUE)/(-1e-30), 1,
                 tolerance = 1e-10)
  }
})


test_that("ged new holds agree with independent multistart likelihoods", {
  skip_unless_tier("full")
  set.seed(8100+6806)
  held_values <- c(mean=0.2,sd=1.3,nu=1.5)
  samples <- list(rlogis(20),rlogis(100),c(-2,-1,0,0,1,3),c(-1,2),
                  qnorm((1:20-.5)/20),seq(-1,1,length.out=20))
  ld <- function(x,p) {v<-p[3]; b<-p[2]*exp(.5*(-2*log(2)/v+lgamma(1/v)-lgamma(3/v))); -log(p[2])-abs((x-p[1])/b)^v/2+log(v)-1.5*lgamma(1/v)+.5*lgamma(3/v)-log(2)}
  for(x in samples)for(mask in 1:7) {
    held <- names(held_values)[as.logical(intToBits(mask)[seq_along(held_values)])]
    args <- as.list(held_values[held])
    f <- tryCatch(suppressWarnings(do.call(mlged,c(list(x=x),args))),error=identity)
    if(inherits(f,"error")) {
      expect_match(conditionMessage(f),"[Uu]se `ml|unbounded|collapse|support cannot contain|does not exist|positive|did not converge|not unique|unresolved|work budget")
      next
    }
    expect_identical(unname(as.numeric(f[held])),unname(held_values[held]))
    expect_setequal(attr(f,"fixed"),held)
    expect_equal(attr(logLik(f),"df"),length(held_values)-length(held))
    expect_equal(as.numeric(logLik(f)),sum(ld(x,as.numeric(f))),tolerance=1e-7)
    free <- setdiff(names(held_values),held)
    if(!length(free))next
    encode <- function(p) {z<-c(p[1],log(p[2]));z<-c(z,log(max(p[3]-1,1e-6)));names(z)<-names(held_values);z[free]}
    decode <- function(z) {
      p<-held_values
      for(j in seq_along(z)) {
        k<-match(free[j],names(p))
        p[k]<-if(k==1) z[j] else if(k==2) exp(z[j]) else 1+exp(z[j])
      }
      p
    }
    objective <- function(z) {
      if(any(!is.finite(z))||any(abs(z)>30))return(1e100)
      v<-suppressWarnings(sum(ld(x,decode(z))))
      if(is.finite(v)) -v else 1e100
    }
    # Starts depend on the sample and held values, not on the fitted result.
    values <- c()
    for(shift in c(-1,0,1)) {
      start<-held_values;start[1]<-median(x)+shift;start[2]<-max(.2,sd(x))*exp(shift/2)
      start[held]<-held_values[held]
      o<-suppressWarnings(optim(encode(start),objective,method="Nelder-Mead",
           control=list(maxit=1800,reltol=1e-11)))
      values<-c(values,o$value)
    }
    expect_gte(as.numeric(logLik(f)),-min(values)-2e-6*length(x))
  }
})


test_that("ged conditional collapse, feasibility and invalid holds", {
  expect_error(suppressWarnings(mlged(rep(2,4),mean=2,nu=2), classes = "univariateML_restricted"),"unbounded")
  expect_true(is.finite(logLik(suppressWarnings(mlged(rep(2,4),mean=0,nu=2), classes = "univariateML_restricted"))))
  expect_equal(unname(suppressWarnings(mlged(rep(2,4),sd=1,nu=2), classes = "univariateML_restricted")["mean"]),2)
  expect_error(suppressWarnings(mlged(seq(-1,1,length.out=40),mean=0), classes = "univariateML_restricted"),"mlunif")
  expect_error(suppressWarnings(mlged(1:5,mean=Inf), classes = "univariateML_restricted"),"mean")
  expect_error(suppressWarnings(mlged(1:5,sd=0), classes = "univariateML_restricted"),"sd")
  expect_s3_class(suppressWarnings(mlged(1:5,nu=.5), classes = "univariateML_restricted"),"univariateML")
})


test_that("GED held variance uniform endpoints have the conditional limiting penalty", {
  x<-rep(c(-sqrt(3),sqrt(3)),10)
  expect_error(suppressWarnings(mlged(x,mean=0,sd=1), classes = "univariateML_restricted"),"unresolved")
  expect_error(suppressWarnings(mlged(x,sd=1), classes = "univariateML_restricted"),"unresolved")
  # From log Gamma(epsilon)=-log(epsilon)-gamma*epsilon+O(epsilon^2):
  # lambda=sqrt(3)*exp((gamma-log(2))/nu+O(nu^-2)), hence W -> exp(-gamma).
  nu<-1e6
  log_lambda<--log(2)/nu+.5*(lgamma(1/nu)-lgamma(3/nu))
  reference<-log(nu)-1.5*lgamma(1/nu)+.5*lgamma(3/nu)-log(2)-
    exp(nu*(log(sqrt(3))-log_lambda)-log(2))
  limit<--log(2*sqrt(3))-exp(-0.5772156649015328606)
  expect_equal(reference,limit,tolerance=2e-6)
  expect_equal(dged(x = sqrt(3), mean = 0, sd = 1, nu = nu, log = TRUE),reference,tolerance=1e-8)
})


test_that("ged held profiles preserve tiny and large units", {
  x<-c(-.02,-.005,0,.007,.03)
  args<-list(nu=10000)
  base<-do.call(mlged,c(list(x=x),args))
  for(unit in c(1e-300,1e300)) {
    fit<-do.call(mlged,c(list(x=x*unit),args))
    expect_equal(unname(fit["sd"])/unit,unname(base["sd"]),tolerance=1e-8)
    expect_equal(as.numeric(logLik(fit))+length(x)*log(unit),as.numeric(logLik(base)),tolerance=1e-8)
  }
})


test_that("ged constant held-sd free-shape submodels attain Laplace", {
  for (args in list(list(sd=1), list(mean=2,sd=1))) {
    f <- suppressWarnings(do.call(mlged,c(list(x=rep(2,3),iterlim=1L),args)),
                          classes="univariateML_restricted")
    expect_equal(unname(as.numeric(f)), c(2,1,1))
    expect_equal(as.numeric(logLik(f)), -1.5*log(2))
    expect_equal(attr(f,"restricted")[["nu"]], "lower")
  }
})

test_that("ged a completed low-budget certificate can prove the Laplace face", {
  x <- c(-3,-1,0,1,4)
  f <- suppressWarnings(mlged(x,mean=0,sd=1,iterlim=1L),classes="univariateML_restricted")
  expect_equal(unname(f["nu"]),1)
  expect_equal(as.numeric(logLik(f)),sum(dged(x=x,mean=0,sd=1,nu=1,log=TRUE)))
})


test_that("ged small held sd returns the proved Laplace maximum or a median tie", {
  f <- suppressWarnings(mlged(c(-3,-1,0,1,4),sd=.1,iterlim=1L),
                        classes="univariateML_restricted")
  expect_equal(unname(f["mean"]),0)
  expect_equal(unname(f["nu"]),1)
  e <- tryCatch(mlged(c(-1,1),sd=.1,iterlim=1L),error=identity)
  expect_s3_class(e,"univariateML_tie")
  expect_match(conditionMessage(e),"median interval")
  expect_equal(e$candidates$mean,c(-1,1))
  expect_true(is.finite(e$loglik_upper))
  expect_lte(e$loglik_lower,e$loglik_upper)
  h <- suppressWarnings(mlged(c(-1,1),mean=.3,sd=.1,iterlim=1L),
                        classes="univariateML_restricted")
  expect_equal(unname(h["mean"]),.3)
  expect_equal(unname(h["nu"]),1)
})


test_that("ged rounded held-sd endpoints produce numerical conditions", {
  x <- c(-sqrt(3),sqrt(3))
  for (args in list(list(sd=1),list(mean=0,sd=1))) {
    e <- tryCatch(do.call(mlged,c(list(x=x),args)),error=identity)
    expect_s3_class(e,"univariateML_numerical")
    expect_length(e$estimates,0)
    expect_true(nrow(e$candidates)>0)
    expect_gte(e$loglik_upper,-2*log(2*sqrt(3)))
  }
})


test_that("ged constant data away from a held mean have a certified uniform escape", {
  e <- tryCatch(mlged(rep(2,3),mean=0,iterlim=1L),error=identity)
  expect_s3_class(e,"univariateML_limit")
  expect_identical(e$model,"unif")
  expect_equal(unname(e$estimates),c(-2,2))
  expect_equal(e$limit_loglik,-3*log(4))
})


test_that("ged free-scale uniform stops have certified global bounds", {
  x <- seq(-1,1,length.out=40)
  for (args in list(list(),list(mean=0))) {
    e <- tryCatch(do.call(mlged,c(list(x=x),args)),error=identity)
    expect_s3_class(e,"univariateML_limit")
    expect_true(is.finite(e$loglik_upper))
    expect_lte(e$loglik_lower,-40*log(2))
    expect_gte(e$loglik_upper,-40*log(2))
  }
})


test_that("ged score certificate accepts every free-shape golden subset", {
  skip_if_not_installed("jsonlite")
  root <- Sys.getenv("UMVL_DISTRIBUTIONS_DIR")
  if(!nzchar(root)) root <- testthat::test_path("fixtures")
  g <- jsonlite::fromJSON(file.path(root,"ged","golden.json"))
  for(args in list(list(),list(mean=g$estimates$mean),list(sd=g$estimates$sd),
                   list(mean=g$estimates$mean,sd=g$estimates$sd))) {
    f <- suppressWarnings(do.call(mlged,c(list(x=g$data),args)),classes="univariateML_restricted")
    expect_equal(unname(f["nu"]),g$estimates$nu,tolerance=1e-5)
    expect_equal(as.numeric(logLik(f)),g$logLik,tolerance=1e-8)
  }
})


test_that("ged globally selected Laplace faces expose their whole median interval", {
  x <- c(seq(-.02,.02,length.out=18),-10,10)
  e <- tryCatch(mlged(x),error=identity)
  expect_s3_class(e,"univariateML_tie")
  expect_equal(e$candidates$nu,c(1,1))
  expect_true(diff(e$candidates$mean)!=0)
})


test_that("ged separated finite shape maxima carry both root witnesses", {
  x <- c(rep(0,8),rep(c(-.32998660809120006,.32998660809120006),23),
         rep(c(-1.4239025460520294,1.4239025460520294),12))
  for(args in list(list(mean=0,sd=1),list(sd=1))) {
    e <- tryCatch(do.call(mlged,c(list(x=x),args)),error=identity)
    expect_s3_class(e,"univariateML_tie")
    expect_equal(sort(e$candidates$nu),c(1.108145553391854,25.39783214175462),tolerance=1e-5)
  }
})


test_that("ged inverse normalization does not return an unchecked rounded location", {
  skip_if_not_installed("jsonlite")
  root <- Sys.getenv("UMVL_DISTRIBUTIONS_DIR")
  if(!nzchar(root)) root <- testthat::test_path("fixtures")
  g <- jsonlite::fromJSON(file.path(root,"ged","golden.json"))
  x <- 1e16+10*g$data
  for(args in list(list(),list(nu=2))) {
    e <- tryCatch(do.call(mlged,c(list(x=x),args)),error=identity)
    expect_s3_class(e,"univariateML_numerical")
    expect_match(conditionMessage(e),"normalization")
    expect_true(nrow(e$candidates)>0)
  }
})



test_that("ged uniform witnesses contain the original observations", {
  u <- 2^-53
  e <- tryCatch(mlged(c(-1+u,1+2*u),mean=u),error=identity)
  expect_s3_class(e,"univariateML_numerical")
  expect_match(conditionMessage(e),"normalization")
  expect_length(e$estimates,0)
})


test_that("ged Laplace median intervals retain original observations", {
  e <- tryCatch(mlged(c(1:19,1e308)),error=identity)
  expect_s3_class(e,"univariateML_tie")
  expect_equal(e$candidates$mean,c(10,11))
})

test_that("held GED shape arithmetic losses stop numerically", {
  expect_error(mlged(c(0, 0, 1e-320), nu = .5), "accuracy")
  expect_error(mlged(c(0, 0, 1e-200), nu = .02), "accuracy")
  expect_error(mlged(c(0, 1e-100), sd = 1e300, nu = .02), "accuracy")
})

test_that("ged fully held evaluation retains underflowed residual powers", {
  x <- c(-1e-100, 1e-100)
  f <- mlged(x, mean=0, sd=1e300, nu=.02)
  target <- -612.224579676587918
  expect_equal(dml(x,f,log=TRUE), rep(target,2), tolerance=2e-12)
  expect_equal(dml(x,f)/exp(target), c(1,1), tolerance=2e-12)
  expect_equal(as.numeric(logLik(f)), 2*target, tolerance=2e-12)
  expect_setequal(attr(f,"fixed"), c("mean","sd","nu"))
  f <- mlged(x, mean=0, sd=1e300, nu=.0005)
  for (lower in c(TRUE,FALSE)) {
    lp <- pml(x,f,lower.tail=lower,log.p=TRUE)
    expect_equal(lp, if(lower) c(-41.209468768459971,-1.2675208492151555e-18)
                 else c(-1.2675208492151555e-18,-41.209468768459971), tolerance=2e-10)
    expect_equal(qml(lp,f,lower.tail=lower,log.p=TRUE)/x,c(1,1),tolerance=2e-10)
  }
})

test_that("extreme GED shapes retain quantiles and sampling spread", {
  for (nu in c(1e8, 1e100, .Machine$double.xmax)) {
    for (f in list(mlged(mean=0, sd=1, nu=nu), mlsged(mean=0, sd=1, nu=nu, xi=2))) {
      p <- c(.01,.25,.5,.75,.99)
      expect_equal(pml(qml(p,f),f),p,tolerance=1e-10)
      set.seed(163)
      x <- rml(10000,f)
      expect_true(all(is.finite(x)))
      expect_lt(abs(mean(x)),.04)
      expect_lt(abs(mean(x*x)-1),.05)
    }
  }
})

test_that("GED log-scale overflow produces NaN rather than a false endpoint", {
  expect_warning(expect_true(is.nan(pged(1,0,1,4e-306,lower.tail=FALSE,log.p=TRUE))),
                 "NaNs produced")
  expect_true(is.finite(pged(1,0,1,1e-300,lower.tail=FALSE,log.p=TRUE)))
})

test_that("GED uniform edges retain the side of adjacent doubles", {
  # Independent 360-digit references from tail-extreme-data/edge-oracle.py.
  nu <- c(1e8,1e100,.Machine$double.xmax)
  ld <- c(-1.803912823679201,-1.2424533248940002,-1.2424533248940002)
  lp <- c(-19.824051541098797,-38.080312530773824,-38.080312530773824)
  for (i in seq_along(nu)) {
    expect_equal(dged(sqrt(3),0,1,nu[i],log=TRUE),ld[i],tolerance=1e-12)
    expect_equal(pged(sqrt(3),0,1,nu[i],lower.tail=FALSE,log.p=TRUE),lp[i],tolerance=1e-12)
    expect_equal(pged(sqrt(3),0,1,nu[i],lower.tail=FALSE)/exp(lp[i]),1,tolerance=1e-10)
  }
  expect_warning(expect_true(is.nan(pged(1+sqrt(3),1,1,.Machine$double.xmax))),"NaNs produced")
  expect_warning(expect_true(is.nan(psged(sqrt(3),0,1,.Machine$double.xmax,2))),"NaNs produced")
})
