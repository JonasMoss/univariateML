context("mllaplace")

## Data generation.
set.seed(313)
small_data <- rlaplace(n = 100, mu = 2, sigma = 3)
tiny_data <- rlaplace(n = 10, mu = 0, sigma = 1)

## Finds errors with na and data out of bounds.
expect_error(mllaplace(c(tiny_data, NA)))

## Checks that na.rm works as intended.
expect_equal(
  coef(mllaplace(small_data)),
  coef(mllaplace(c(small_data, NA), na.rm = TRUE))
)

## Is the log-likelihood correct?
est <- mllaplace(small_data, na.rm = TRUE)
expect_equal(
  sum(dlaplace(x = small_data, mu = est[1], sigma = est[2], log = TRUE)),
  attr(est, "logLik")
)

## Check class.
expect_equal(attr(est, "model"), "Laplace")
expect_equal(class(est), "univariateML")


## Check support.
expect_equal(class(attr(est, "support"))[[1]], "numeric")


test_that("laplace handles a seeded sample with a large outlier", {
  set.seed(7)
  x <- c(rlaplace(n = 199, mu = 0, sigma = 1), 1e3)
  objective <- function(p) -sum(dlaplace(x = x, mu = p[1], sigma = exp(p[2]), log = TRUE))
  oracle <- optim(c(median(x), log(stats::mad(x))), objective,
    method = "Nelder-Mead", control = list(reltol = 1e-14, maxit = 10000))
  expected <- c(oracle$par[1], exp(oracle$par[2]))
  # For even n the location MLE is an interval; choose its upper endpoint.
  location <- sort(x)[length(x) / 2 + 1]
  expected <- c(location, mean(abs(x - location)))
  fit <- mllaplace(x)
  expect_true(all(is.finite(fit)))
  expect_true(all(abs(as.numeric(fit) - expected) <= 1e-4 * abs(expected)))
  expect_gte(attr(fit, "logLik"), -oracle$value - 1e-6)
})

test_that("constant Laplace data have no attained MLE", {
  expect_error(mllaplace(rep(1,5)),"does not exist")
})


test_that("laplace new holds agree with independent multistart likelihoods", {
  set.seed(8100+1615)
  held_values <- c(mu=0.2,sigma=1.3)
  samples <- list(rlogis(20),rlogis(100),c(-2,-1,0,0,1,3),c(-1,2),
                  qnorm((1:20-.5)/20),seq(-1,1,length.out=20))
  ld <- function(x,p) dlaplace(x = x, mu = p[1], sigma = p[2], log = TRUE)
  for(x in samples)for(mask in 1:3) {
    held <- names(held_values)[as.logical(intToBits(mask)[seq_along(held_values)])]
    args <- as.list(held_values[held])
    f <- tryCatch(suppressWarnings(do.call(mllaplace,c(list(x=x),args))),error=identity)
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


test_that("laplace conditional collapse, feasibility and invalid holds", {
  expect_equal(as.numeric(mllaplace(rep(2,3),sigma=1)),c(2,1))
  expect_equal(unname(mllaplace(rep(2,3),mu=0)["sigma"]),2)
  expect_error(mllaplace(rep(2,3),mu=2),"does not exist")
  expect_error(mllaplace(1:4,mu=Inf),"finite")
  expect_error(mllaplace(1:4,sigma=0),"positive")
})
