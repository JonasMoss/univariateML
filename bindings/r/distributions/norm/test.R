context("mlnorm")

## Data generation.
set.seed(313)
small_data <- rnorm(100, 2, 3)
tiny_data <- rnorm(10, 0, 1)

## Finds errors with na and data out of bounds.
expect_error(mlnorm(c(tiny_data, NA)))

## Checks that na.rm works as intended.
expect_equal(
  coef(mlnorm(small_data)),
  coef(mlnorm(c(small_data, NA), na.rm = TRUE))
)

## Is the log-likelihood correct?
est <- mlnorm(small_data, na.rm = TRUE)
expect_equal(
  sum(dnorm(small_data, est[1], est[2], log = TRUE)),
  attr(est, "logLik")
)

## Check class.
expect_equal(attr(est, "model"), "Normal")
expect_equal(class(est), "univariateML")


## Check support.
expect_equal(class(attr(est, "support"))[[1]], "numeric")


test_that("norm handles a seeded sample with a large outlier", {
  set.seed(7)
  x <- c(stats::rnorm(199), 1e3)
  objective <- function(p) -sum(stats::dnorm(x, p[1], exp(p[2]), log = TRUE))
  oracle <- optim(c(median(x), log(stats::mad(x))), objective,
    method = "Nelder-Mead", control = list(reltol = 1e-14, maxit = 10000))
  expected <- c(oracle$par[1], exp(oracle$par[2]))
  fit <- mlnorm(x)
  expect_true(all(is.finite(fit)))
  expect_true(all(abs(as.numeric(fit) - expected) <= 1e-4 * abs(expected)))
  expect_gte(attr(fit, "logLik"), -oracle$value - 1e-6)
})

test_that("constant data have no attained norm MLE", {
  expect_error(mlnorm(rep(2, 5)), "does not exist")
  expect_error(mlnorm(2), "does not exist")
})

test_that("normal moments retain tiny and huge scales and near ties", {
  for (s in c(1e-300, 1e300)) {
    x <- c(-2, -1, 0, 1, 2) * s
    f <- mlnorm(x)
    expect_equal(f[["sd"]] / s, sqrt(2))
    expect_equal(attr(f, "logLik"), sum(dnorm(x, f[[1]], f[[2]], log = TRUE)))
  }
  x <- 1 + (0:4)*.Machine$double.eps
  expect_equal(mlnorm(x)[["sd"]] / .Machine$double.eps, sqrt(2))
})


test_that("norm new holds agree with independent multistart likelihoods", {
  skip_unless_tier("full")
  set.seed(8100+1790)
  held_values <- c(mean=0.2,sd=1.3)
  samples <- list(rlogis(20),rlogis(100),c(-2,-1,0,0,1,3),c(-1,2),
                  qnorm((1:20-.5)/20),seq(-1,1,length.out=20))
  ld <- function(x,p) dnorm(x,p[1],p[2],log=TRUE)
  for(x in samples)for(mask in 1:3) {
    held <- names(held_values)[as.logical(intToBits(mask)[seq_along(held_values)])]
    args <- as.list(held_values[held])
    f <- tryCatch(suppressWarnings(do.call(mlnorm,c(list(x=x),args))),error=identity)
    if(inherits(f,"error")) {
      expect_match(conditionMessage(f),"[Uu]se `ml|unbounded|collapse|support cannot contain|does not exist|positive")
      next
    }
    expect_identical(unname(as.numeric(f[held])),unname(held_values[held]))
    expect_setequal(attr(f,"fixed"),held)
    expect_equal(attr(logLik(f),"df"),length(held_values)-length(held))
    expect_equal(as.numeric(logLik(f)),sum(ld(x,as.numeric(f))),tolerance=1e-7)
    free <- setdiff(names(held_values),held)
    if(!length(free))next
    encode <- function(p) {z<-c(p[1],log(p[2]));names(z)<-names(held_values);z[free]}
    decode <- function(z) {
      p<-held_values
      for(j in seq_along(z)) {
        k<-match(free[j],names(p))
        p[k]<-if(k==1) z[j] else if(k==2) exp(z[j]) else exp(z[j])
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


test_that("norm conditional collapse, feasibility and invalid holds", {
  expect_equal(as.numeric(mlnorm(rep(2,3),sd=1)),c(2,1))
  expect_equal(unname(mlnorm(rep(2,3),mean=0)["sd"]),2)
  expect_error(mlnorm(rep(2,3),mean=2),"unbounded")
  expect_error(mlnorm(1:4,mean=Inf),"finite")
  expect_error(mlnorm(1:4,sd=0),"positive")
})

test_that("scaled residuals preserve every normal hold at extreme units", {
  for (unit in c(1e-200, 1e200)) for (mask in 0:3) {
    args <- list(x = c(1, 2) * unit)
    m <- if (bitwAnd(mask, 1L)) 0 else 1.5
    s <- if (bitwAnd(mask, 2L)) 1 else if (bitwAnd(mask, 1L)) sqrt(2.5) else .5
    if (bitwAnd(mask, 1L)) args$mean <- 0
    if (bitwAnd(mask, 2L)) args$sd <- unit
    f <- do.call(mlnorm, args)
    expect_equal(unname(f["mean"]) / unit, m, tolerance = 1e-14)
    expect_equal(unname(f["sd"]) / unit, s, tolerance = 1e-14)
    ll <- -2 * (log(unit) + log(s) + log(2*pi)/2) - sum((c(1,2)-m)^2)/(2*s*s)
    expect_equal(attr(f, "logLik"), ll, tolerance = 1e-14)
    t <- do.call(mlnorm, c(args, list(truncate=c(-Inf,Inf))))
    expect_equal(as.numeric(t[c("mean", "sd")]) / unit, c(m,s), tolerance=1e-14)
    expect_equal(attr(t, "logLik"), ll, tolerance=1e-14)
  }
  expect_error(mlnorm(c(0,0), mean=0), "unbounded")
  expect_true(is.finite(attr(mlnorm(c(0,0), mean=0, sd=1e-200), "logLik")))
})

test_that("transformed normal and normalized scale families keep extreme units", {
  for (unit in c(1e-200, 1e200)) {
    for (held in c(FALSE, TRUE)) {
      a <- list(x=c(1,1), meanlog=unit)
      b <- list(x=c(.5,.5), mu=unit)
      if (held) { a$sdlog <- unit; b$sigma <- unit }
      l <- do.call(mllnorm,a); g <- do.call(mllogitnorm,b)
      expect_equal(unname(l["sdlog"])/unit, 1, tolerance=1e-14)
      expect_equal(unname(g["sigma"])/unit, 1, tolerance=1e-14)
      ll <- -2*(log(unit)+log(2*pi)/2+.5)
      expect_equal(attr(l,"logLik"), ll, tolerance=1e-14)
      expect_equal(attr(g,"logLik"), ll+2*log(4), tolerance=1e-14)
    }
    h <- mlhnorm(c(1,2)*unit,mu=0)
    f <- mlfoldnorm(c(1,2)*unit,mu=0)
    b <- mlbhatt(c(1,2)*unit,mu=0,a=0)
    for (fit in list(h,f,b)) {
      expect_equal(unname(fit["sigma"])/unit,sqrt(2.5),tolerance=1e-12)
      expect_true(is.finite(attr(fit,"logLik")))
    }
    s <- mlsnorm(c(-1,rep(0,10),1)*unit)
    expect_equal(unname(s["sd"])/unit,sqrt(1/6),tolerance=1e-12)
    expect_true(is.finite(attr(s,"logLik")))
  }
})

test_that("Gaussian limit witnesses retain extreme residual scales", {
  for (unit in c(1e-200,1e200)) {
    h <- tryCatch(mlhuber(c(-1,1)*unit,epsilon=NULL),error=identity)
    e <- tryCatch(mlemg(c(-1,-2)*unit,mu=0),error=identity)
    for (stop in list(h,e)) {
      expect_s3_class(stop,"univariateML_limit")
      expect_equal(stop$model,"norm")
      expect_true(is.finite(stop$limit_loglik))
    }
    expect_equal(unname(h$estimates["sd"])/unit,1,tolerance=1e-12)
    expect_equal(unname(e$estimates["sd"])/unit,sqrt(2.5),tolerance=1e-12)
  }
})
