context("mllogis")

## Data generation.
set.seed(313)
tiny_data <- stats::rlogis(10, 1, 7)
small_data <- stats::rlogis(100, 10, 3)
medium_data <- stats::rlogis(1000, 1 / 2, 2)
large_data <- stats::rlogis(10000, 20, 13)

## Checks if the ML is correct.

m <- stats::median(tiny_data)
mad <- stats::median(abs(tiny_data - m))
start <- c(m, log(mad))

mle1 <- suppressWarnings(nlm(function(p) {
  -sum(stats::dlogis(tiny_data, p[1], exp(p[2]), log = TRUE))
}, p = start))


m <- stats::median(small_data)
mad <- stats::median(abs(small_data - m))
start <- c(m, log(mad))

mle2 <- suppressWarnings(nlm(function(p) {
  -sum(stats::dlogis(small_data, p[1], exp(p[2]), log = TRUE))
}, p = start))


m <- stats::median(medium_data)
mad <- stats::median(abs(medium_data - m))
start <- c(m, log(mad))

mle3 <- suppressWarnings(nlm(function(p) {
  -sum(stats::dlogis(medium_data, p[1], exp(p[2]), log = TRUE))
}, p = start))

m <- stats::median(large_data)
mad <- stats::median(abs(large_data - m))
start <- c(m, log(mad))

mle4 <- suppressWarnings(nlm(function(p) {
  -sum(stats::dlogis(large_data, p[1], exp(p[2]), log = TRUE))
}, p = start))

## Checks estimates.
expect_equal(c(mle1$estimate[1], exp(mle1$estimate[2])),
  as.numeric(mllogis(tiny_data)),
  tolerance = 1e-4
)
expect_equal(c(mle2$estimate[1], exp(mle2$estimate[2])),
  as.numeric(mllogis(small_data)),
  tolerance = 1e-4
)
expect_equal(c(mle3$estimate[1], exp(mle3$estimate[2])),
  as.numeric(mllogis(medium_data)),
  tolerance = 1e-4
)
expect_equal(c(mle4$estimate[1], exp(mle4$estimate[2])),
  as.numeric(mllogis(large_data)),
  tolerance = 1e-4
)

## Checks logLiks.
expect_equal(-mle1$minimum, attr(mllogis(tiny_data), "logLik"),
  tolerance = 1e-4
)
expect_equal(-mle2$minimum, attr(mllogis(small_data), "logLik"),
  tolerance = 1e-4
)
expect_equal(-mle3$minimum, attr(mllogis(medium_data), "logLik"),
  tolerance = 1e-4
)
expect_equal(-mle4$minimum, attr(mllogis(large_data), "logLik"),
  tolerance = 1e-4
)

## Finds errors with na and data out of bounds.
expect_error(mllogis(c(tiny_data, NA)))

## Checks that na.rm works as intended.
expect_equal(
  coef(mllogis(small_data)),
  coef(mllogis(c(small_data, NA), na.rm = TRUE))
)

## Check class.
est <- mllogis(small_data, na.rm = TRUE)
expect_equal(attr(est, "model"), "Logistic")
expect_equal(class(est), "univariateML")


## Check support.
expect_equal(class(attr(est, "support"))[[1]], "numeric")


test_that("logis handles a seeded sample with a large outlier", {
  set.seed(7)
  x <- c(stats::rlogis(199), 1e3)
  objective <- function(p) -sum(stats::dlogis(x, p[1], exp(p[2]), log = TRUE))
  oracle <- optim(c(median(x), log(stats::mad(x))), objective,
    method = "Nelder-Mead", control = list(reltol = 1e-14, maxit = 10000))
  expected <- c(oracle$par[1], exp(oracle$par[2]))
  fit <- mllogis(x)
  expect_true(all(is.finite(fit)))
  expect_true(all(abs(as.numeric(fit) - expected) <= 1e-4 * abs(expected)))
  expect_gte(attr(fit, "logLik"), -oracle$value - 1e-6)
})

test_that("logis fitting respects extreme scale changes", {
  set.seed(1)
  x <- simulate_default("mllogis", 50)
  f <- mllogis(x)
  for (s in c(1e-300,1e300)) {
    g <- mllogis(x*s)
    expect_equal(attr(g,"logLik")+length(x)*log(s),attr(f,"logLik"),tolerance=1e-6)
  }
})


test_that("logis new holds agree with independent multistart likelihoods", {
  set.seed(8100+3268)
  held_values <- c(location=0.2,scale=1.3)
  samples <- list(rlogis(20),rlogis(100),c(-2,-1,0,0,1,3),c(-1,2),
                  qnorm((1:20-.5)/20),seq(-1,1,length.out=20))
  ld <- function(x,p) dlogis(x,p[1],p[2],log=TRUE)
  for(x in samples)for(mask in 1:3) {
    held <- names(held_values)[as.logical(intToBits(mask)[seq_along(held_values)])]
    args <- as.list(held_values[held])
    f <- tryCatch(suppressWarnings(do.call(mllogis,c(list(x=x),args))),error=identity)
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


test_that("logis conditional collapse, feasibility and invalid holds", {
  expect_equal(as.numeric(mllogis(rep(2,3),scale=1)),c(2,1))
  expect_true(is.finite(logLik(mllogis(rep(2,3),location=0))))
  expect_error(mllogis(rep(2,3),location=2),"unbounded")
  expect_error(mllogis(1:4,location=Inf),"finite")
  expect_error(mllogis(1:4,scale=0),"positive")
  expect_error(mllogis(1:4,location=0,iterlim=1),"converge")
})


test_that("logis held profiles preserve tiny and large units", {
  x<-c(-2,-.5,0,.7,3)
  args<-list(location=0)
  base<-do.call(mllogis,c(list(x=x),args))
  for(unit in c(1e-300,1e300)) {
    fit<-do.call(mllogis,c(list(x=x*unit),args))
    expect_equal(unname(fit["scale"])/unit,unname(base["scale"]),tolerance=1e-8)
    expect_equal(as.numeric(logLik(fit))+length(x)*log(unit),as.numeric(logLik(base)),tolerance=1e-8)
  }
})
