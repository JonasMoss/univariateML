context("mlkumar")

## Data generation.
set.seed(313)
small_data <- extraDistr::rkumar(100, 1, 1)
tiny_data <- extraDistr::rkumar(10, 3, 7)
medium_data <- extraDistr::rkumar(1000, 9, 11)

## Checks if the ML is correct.
mle1 <- suppressWarnings(nlm(function(p) {
  -sum(extraDistr::dkumar(small_data, p[1], p[2], log = TRUE))
}, p = c(1, 1)))

mle2 <- nlm(function(p) {
  -sum(extraDistr::dkumar(tiny_data, p[1], p[2], log = TRUE))
}, p = c(3, 7))

mle3 <- nlm(function(p) {
  -sum(extraDistr::dkumar(medium_data, p[1], p[2], log = TRUE))
}, p = c(9, 11))

expect_equal(mle1$estimate, as.numeric(mlkumar(small_data)),
  tolerance = 1e-5
)
expect_equal(mle2$estimate, as.numeric(mlkumar(tiny_data)),
  tolerance = 1e-5
)
expect_equal(mle3$estimate, as.numeric(mlkumar(medium_data)),
  tolerance = 1e-5
)

expect_equal(-mle1$minimum, attr(mlkumar(small_data), "logLik"),
  tolerance = 1e-5
)
expect_equal(-mle2$minimum, attr(mlkumar(tiny_data), "logLik"),
  tolerance = 1e-5
)
expect_equal(-mle3$minimum, attr(mlkumar(medium_data), "logLik"),
  tolerance = 1e-5
)

## Finds errors with na and data out of bounds.
expect_error(mlkumar(c(tiny_data, NA)))
expect_error(mlkumar(c(tiny_data, 0)))
expect_error(mlkumar(c(tiny_data, 1)))

## Checks that na.rm works as intended.
expect_equal(
  coef(mlkumar(small_data)),
  coef(mlkumar(c(small_data, NA), na.rm = TRUE))
)

est <- mlkumar(tiny_data)
## Check class.
expect_equal(attr(est, "model"), "Kumaraswamy")
expect_equal(class(est), "univariateML")


## Check support.
expect_equal(class(attr(est, "support"))[[1]], "numeric")

fit_default <- mlkumar(medium_data, reltol = 1e-12)
fit_start <- mlkumar(medium_data, a0 = 1, reltol = 1e-12)
expect_equal(coef(fit_start), coef(fit_default), tolerance = 1e-8)
expect_error(mlkumar(medium_data, a0 = 0), "`a0` must be a positive finite number")
expect_error(mlkumar(medium_data, a0 = Inf), "`a0` must be a positive finite number")

test_that("clipped endpoint samples have finite profile maxima", {
  for (seed in c(300019, 300023, 1, 2, 3)) {
    set.seed(seed)
    x <- pmin(pmax(rbeta(200, .05, .05), 1e-12), 1 - 1e-12)
    fit <- mlkumar(x)
    stable <- function(p) {
      a <- exp(p[1]); b <- exp(p[2])
      -sum(log(a) + log(b) + (a - 1)*log(x) +
             (b - 1)*log(-expm1(a*log(x))))
    }
    starts <- expand.grid(a = c(.01, .1, 1), b = c(.05, .5))
    reference <- function(p) {
      v <- -sum(extraDistr::dkumar(x, exp(p[1]), exp(p[2]), log = TRUE))
      if (is.finite(v)) v else 1e100
    }
    oracles <- apply(starts, 1, function(p) optim(log(p), reference,
      control = list(maxit = 2000, reltol = 1e-12)))
    values <- vapply(oracles, `[[`, 0, "value")
    best <- oracles[[which.min(values)]]
    # extraDistr loses precision in 1-x^a at the clipped upper endpoint.
    expect_lt(abs(attr(fit, "logLik") + min(values)), .1)
    expect_lt(abs(attr(fit, "logLik") + stable(best$par)), .002)
    oracle <- optim(log(as.numeric(fit)), stable, method = "BFGS",
                    control = list(reltol = 1e-13))
    profile <- function(p) {
      a <- exp(p); b <- -length(x)/sum(log(-expm1(a*log(x))))
      stable(log(c(a, b)))
    }
    prof <- optimize(profile, c(-15, 5), tol = 1e-10)
    expect_equal(attr(fit, "logLik"), -oracle$value, tolerance = 1e-11)
    expect_equal(attr(fit, "logLik"), -prof$objective, tolerance = 1e-11)
    expect_equal(unname(log(fit[1])), prof$minimum, tolerance = 1e-6)
  }
})

test_that("constant Kumaraswamy samples have unbounded likelihood", {
  for (v in c(.01, .4, .99)) expect_error(mlkumar(rep(v, 5)), "unbounded")
})

test_that("P2 every held subset agrees with independent multistart density fits", {
  set.seed(8129)
  samples <- list(extraDistr::rkumar(20,2,3), extraDistr::rkumar(100,2,3),
                  runif(20,.01,.99), runif(100,.01,.99),
                  c(.2,.2,.6,.8), c(.2,.8), rep(.4,5), .4,
                  c(1e-5, .01, .5, .99, 1-1e-5))
  values <- c(a=2, b=3)
  for (values in list(c(a=2,b=3),c(a=.2,b=.3),c(a=1,b=1),c(a=20,b=30))) {
  for (x in samples) for (mask in 1:3) {
    held <- names(values)[as.logical(intToBits(mask)[1:2])]
    free <- setdiff(names(values),held)
    fit <- do.call(mlkumar,c(list(x=x),as.list(values[held])))
    expect_identical(attr(fit,"fixed"),held)
    expect_identical(unname(coef(fit)[held]),unname(values[held]))
    expect_equal(attr(logLik(fit),"df"),2L-length(held))
    ll <- function(p) suppressWarnings(sum(extraDistr::dkumar(x,p[1],p[2],log=TRUE)))
    expect_equal(as.numeric(logLik(fit)),ll(coef(fit)),tolerance=1e-9)
    if (length(free)) {
      objective <- function(z) {
        p <- values
        p[free] <- exp(z)
        v <- -ll(p)
        if (is.finite(v)) v else 1e100
      }
      refs <- vapply(c(-3,0,3), function(start)
        nlminb(start,objective,control=list(iter.max=1000,rel.tol=1e-13))$objective,0.0)
      expect_lt(abs(as.numeric(logLik(fit))+min(refs)),1e-6*length(x))
    }
  }
  }
  for (p in names(values)) for (bad in c(NA_real_,Inf,-Inf,0,-1))
    expect_error(do.call(mlkumar,c(list(x=c(.2,.8)),setNames(list(bad),p))))
  for (edge in c(0,1)) for (mask in 1:3) {
    held <- names(values)[as.logical(intToBits(mask)[1:2])]
    expect_error(do.call(mlkumar,c(list(x=c(edge,.4)),as.list(values[held]))))
  }
})

test_that("P2 held shape solves stop on exhausted brackets", {
  expect_error(mlkumar(c(.2,.8),b=2,iterlim=1),"failed")
  expect_error(mlkumar(c(.2,.8),b=3,iterlim=1),"failed")
})
