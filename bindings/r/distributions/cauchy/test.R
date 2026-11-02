context("mlcauchy")

## Data generation.
set.seed(313)
tiny_data <- stats::rcauchy(10, 1, 7)
small_data <- stats::rcauchy(100, 10, 3)
medium_data <- stats::rcauchy(1000, 1 / 2, 2)
large_data <- stats::rcauchy(10000, 20, 13)

## Checks if the ML is correct.

mle1 <- suppressWarnings(nlm(function(p) {
  -sum(stats::dcauchy(tiny_data, p[1], p[2], log = TRUE))
}, p = c(1, 1)))

mle2 <- suppressWarnings(nlm(function(p) {
  -sum(stats::dcauchy(small_data, p[1], p[2], log = TRUE))
}, p = c(1, 1)))

mle3 <- suppressWarnings(nlm(function(p) {
  -sum(stats::dcauchy(medium_data, p[1], p[2], log = TRUE))
}, p = c(1, 1)))

mle4 <- suppressWarnings(nlm(function(p) {
  -sum(stats::dcauchy(large_data, p[1], p[2], log = TRUE))
}, p = c(
  stats::median(large_data),
  stats::median(abs(large_data - stats::median(large_data)))
)))

## Checks estimates.
expect_equal(mle1$estimate, as.numeric(mlcauchy(tiny_data)),
  tolerance = 1e-5
)
expect_equal(mle2$estimate, as.numeric(mlcauchy(small_data)),
  tolerance = 1e-5
)
expect_equal(mle3$estimate, as.numeric(mlcauchy(medium_data)),
  tolerance = 1e-5
)
expect_equal(mle4$estimate, as.numeric(mlcauchy(large_data)),
  tolerance = 1e-5
)

## Checks logLiks.
expect_equal(-mle1$minimum, attr(mlcauchy(tiny_data), "logLik"),
  tolerance = 1e-5
)
expect_equal(-mle2$minimum, attr(mlcauchy(small_data), "logLik"),
  tolerance = 1e-5
)
expect_equal(-mle3$minimum, attr(mlcauchy(medium_data), "logLik"),
  tolerance = 1e-5
)
expect_equal(-mle4$minimum, attr(mlcauchy(large_data), "logLik"),
  tolerance = 1e-5
)

## Finds errors with na and data out of bounds.
expect_error(mlcauchy(c(tiny_data, NA)))

## Checks that na.rm works as intended.
expect_equal(
  coef(mlcauchy(small_data)),
  coef(mlcauchy(c(small_data, NA), na.rm = TRUE))
)

## Check class.
est <- mlcauchy(small_data, na.rm = TRUE)
expect_equal(attr(est, "model"), "Cauchy")
expect_equal(class(est), "univariateML")


## Check support.
expect_equal(class(attr(est, "support"))[[1]], "numeric")


test_that("cauchy handles a seeded sample with a large outlier", {
  set.seed(7)
  x <- c(stats::rcauchy(199), 1e3)
  objective <- function(p) -sum(stats::dcauchy(x, p[1], exp(p[2]), log = TRUE))
  oracle <- optim(c(median(x), log(stats::mad(x))), objective,
    method = "Nelder-Mead", control = list(reltol = 1e-14, maxit = 10000))
  expected <- c(oracle$par[1], exp(oracle$par[2]))
  fit <- mlcauchy(x)
  expect_true(all(is.finite(fit)))
  expect_true(all(abs(as.numeric(fit) - expected) <= 1e-4 * abs(expected)))
  expect_gte(attr(fit, "logLik"), -oracle$value - 1e-6)
})


test_that("Cauchy fits three observations and diagnoses nonexistent MLEs", {
  x <- c(-2.2425190273413, 0.723253413022922, -2.56072856385821)
  objective <- function(p) -sum(stats::dcauchy(x, p[1], exp(p[2]), log = TRUE))
  oracle <- optim(c(median(x), log(mad(x))), objective,
    control = list(reltol = 1e-14, maxit = 10000))
  expected <- c(oracle$par[1], exp(oracle$par[2]))
  fit <- mlcauchy(x)
  expect_true(all(abs(as.numeric(fit) - expected) <= 1e-4 * abs(expected)))
  expect_gte(attr(fit, "logLik"), -oracle$value - 1e-6)
  for (x in list(1, c(1, 1), c(1, 1, 2), c(1, 1, 2, 3))) {
    expect_error(mlcauchy(x), "MLE does not exist")
  }
})

test_that("balanced two-point Cauchy data have attained nonunique maxima", {
  for (x in list(c(1, 2), c(1, 1, 2, 2))) {
    f <- mlcauchy(x)
    expect_equal(as.numeric(f), c(1.5, .5))
    objective <- function(p) -sum(dcauchy(x, p[1], exp(p[2]), log = TRUE))
    o <- optim(c(1.4, log(.6)), objective, control = list(reltol=1e-12))
    expect_equal(attr(f, "logLik"), -o$value, tolerance=1e-8)
  }
})
test_that("Cauchy fitting respects extreme scale changes", {
  x <- c(-3, -.5, 0, 1, 4)
  f <- mlcauchy(x)
  for (s in c(1e-300, 1e300)) {
    g <- mlcauchy(x*s)
    expect_equal(as.numeric(g)/s, as.numeric(f), tolerance=1e-4)
    expect_equal(attr(g,"logLik") + length(x)*log(s), attr(f,"logLik"), tolerance=1e-6)
  }
})


test_that("cauchy new holds agree with independent multistart likelihoods", {
  skip_unless_tier("full")
  set.seed(8100+3951)
  held_values <- c(location=0.2,scale=1.3)
  samples <- list(rlogis(20),rlogis(100),c(-2,-1,0,0,1,3),c(-1,2),
                  qnorm((1:20-.5)/20),seq(-1,1,length.out=20))
  ld <- function(x,p) dcauchy(x,p[1],p[2],log=TRUE)
  for(x in samples)for(mask in 1:3) {
    held <- names(held_values)[as.logical(intToBits(mask)[seq_along(held_values)])]
    args <- as.list(held_values[held])
    f <- tryCatch(suppressWarnings(do.call(mlcauchy,c(list(x=x),args))),error=identity)
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


test_that("cauchy conditional collapse, feasibility and invalid holds", {
  expect_equal(as.numeric(mlcauchy(rep(2,3),scale=1)),c(2,1))
  expect_true(is.finite(logLik(mlcauchy(rep(2,3),location=0))))
  expect_error(mlcauchy(c(0,1),location=0),"does not exist")
  expect_error(mlcauchy(c(0,0,1),location=0),"does not exist")
  expect_error(mlcauchy(1:4,location=Inf),"finite")
  expect_error(mlcauchy(1:4,scale=0),"positive")
  expect_error(mlcauchy(1:4,location=0,iterlim=1),"converge")
})


test_that("cauchy held profiles preserve tiny and large units", {
  x<-c(-2,-.5,0,.7,3)
  args<-list(location=0)
  base<-do.call(mlcauchy,c(list(x=x),args))
  for(unit in c(1e-300,1e300)) {
    fit<-do.call(mlcauchy,c(list(x=x*unit),args))
    expect_equal(unname(fit["scale"])/unit,unname(base["scale"]),tolerance=1e-8)
    expect_equal(as.numeric(logLik(fit))+length(x)*log(unit),as.numeric(logLik(base)),tolerance=1e-8)
  }
})

test_that("held Cauchy location brackets disparate residuals", {
  for (a in c(1e-310, 1e-300, 1e-200, 1, 1e200)) for (b in c(1, 1e200, 1e300)) {
    if (b < a*1e10) next
    x <- c(0, a, b)
    f <- mlcauchy(x, location=0)
    expect_equal(log(unname(f["scale"])), log(a), tolerance=1e-10)
    expected <- -3*log(pi)-log(a)-2*log(b)-log(2)
    expect_equal(as.numeric(logLik(f)), expected, tolerance=1e-10)
  }
  expect_error(mlcauchy(c(0, .Machine$double.xmin*.Machine$double.eps, 1), location=0), "numerical")
  expect_error(mlcauchy(c(-1e308,-1e308,-1e308), location=1e308), "numerical")
  expect_error(mlcauchy(c(0,1e-300,1), location=0, iterlim=1), "converge")
  f <- mlcauchy(c(0,1e-300,1), scale=1)
  expect_identical(unname(f["scale"]), 1)
})

test_that("held Cauchy location supports overflowing finite differences", {
  f <- mlcauchy(c(-1e308,0,1e308), location=-1e308)
  expect_equal(unname(f["scale"])/1e308, sqrt((sqrt(73)-5)/6), tolerance=1e-10)
  expect_true(is.finite(logLik(f)))
})

test_that("free Cauchy fits pass the geometric total-score gate", {
  samples <- list(c(0,0,0,0,.01,1,2,3,4),
                  c(-1e6,-100,-1,0,1,100,1e6),
                  c(-101,-100,-99,98,99,100,101))
  for (x in samples) {
    f <- mlcauchy(x, abstol=1e-8, iterlim=1000)
    z <- (x-unname(f["location"]))/unname(f["scale"])
    score <- c(sum(2*z/(1+z^2)), sum((z^2-1)/(1+z^2)))
    expect_lt(sqrt(sum(score^2)), 1e-3)
    expect_equal(as.numeric(logLik(f)),
                 sum(dcauchy(x, f[1], f[2], log=TRUE)), tolerance=1e-10)
  }
  expect_error(mlcauchy(c(-8,-2,0,1,3,20,100), abstol=1e6), "converge")
})
