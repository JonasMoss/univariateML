context("mlged")

## Data generation.
set.seed(313)
small_data <- rged(100, 2, 3, 4)
tiny_data <- rged(100, 2, 3, 4)

## Finds errors with na and data out of bounds.
expect_error(mlged(c(tiny_data, NA)))

## Checks that na.rm works as intended.
expect_equal(
  coef(mlged(small_data)),
  coef(mlged(c(small_data, NA), na.rm = TRUE))
)

## Is the log-likelihood correct?
est <- mlged(small_data, na.rm = TRUE)
expect_equal(
  sum(dged(small_data, est[1], est[2], est[3], log = TRUE)),
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
expect_error(mlged(seq(-1, 1, length.out = 40)), "Use `mlunif`")
x <- c(seq(-.02, .02, length.out = 18), -10, 10)
f <- mlged(x)
expect_equal(unname(f["nu"]), 1)
expect_equal(attr(f, "logLik"), sum(dged(x, mean=f[1], sd=f[2], nu=1, log=TRUE)))
a <- mean(abs(x-median(x)))
expect_equal(attr(f, "logLik"), -length(x)*(log(2*a)+1))

set.seed(812)
for (shape in c(1.5, 3)) {
  x <- rged(500, nu=shape)
  f <- mlged(x, gradtol=1e-6, iterlim=1000)
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
  x <- simulate_default("mlged",50)
  base <- tryCatch(mlged(x),error=function(e) conditionMessage(e))
  for(s in c(1e-300,1e300)) {
    f <- tryCatch(mlged(x*s),error=function(e) conditionMessage(e))
    if(is.character(base)) expect_identical(f,base) else {
      expect_false(is.character(f))
      if(!is.character(f)) expect_equal(attr(f,"logLik")+length(x)*log(s),attr(base,"logLik"),tolerance=1e-4)
    }
  }
})

test_that("ged large-sample fitting preserves likelihood under replication", {
  p<-((1:1000)-.5)/1000
  x<-qged(p,nu=3)
  base<-mlged(x)
  f<-mlged(rep(x,100))
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
  expect_length(dged(numeric()), 0)
  expect_equal(dged(c(0, 1, 2), mean = c(0, 1)),
    c(dged(0), dged(1, mean = 1), dged(2)))
  expect_identical(dged(NA_real_), NA_real_)
  expect_true(is.nan(pged(NaN)))
  expect_warning(expect_true(is.nan(dged(0, sd = -1))), "NaNs produced")
  expect_warning(expect_true(is.nan(qged(1.1))), "NaNs produced")
  expect_warning(expect_true(is.nan(rged(1, sd = -1))), "NaNs produced")
  set.seed(81); first <- rged(100)
  set.seed(81); expect_identical(rged(100), first)
  expect_length(rged(1:7), 7)
  expect_length(rged(0), 0)
  expect_length(rged(3, mean = numeric()), 0)
  expect_identical(rged(1, mean = NA_real_), NA_real_)
  set.seed(712)
  draws <- rged(5000)
  for (prob in c(.1, .5, .9))
    expect_lt(abs(mean(draws <= qged(prob)) - prob), .025)
  for (bad in c(-1, 0, Inf)) {
    expect_warning(expect_true(is.nan(dged(0, sd = bad))), "NaNs produced")
    expect_warning(expect_true(is.nan(pged(0, sd = bad))), "NaNs produced")
    expect_warning(expect_true(is.nan(qged(.5, sd = bad))), "NaNs produced")
  }
  fit <- mlged(small_data)
  expect_equal(as.numeric(logLik(fit)), sum(dml(small_data, fit, log = TRUE)),
               tolerance = 1e-10)
  expect_equal(pml(0, fit), do.call(pged, c(list(0), as.list(coef(fit)))))
  expect_equal(qml(.2, fit), do.call(qged, c(list(.2), as.list(coef(fit)))))
  set.seed(93); expected <- do.call(rged, c(list(10), as.list(coef(fit))))
  set.seed(93); expect_identical(rml(10, fit), expected)
})

test_that("ged dpqr agree with accurate fGarch ordinary values", {
  skip_if_not_installed("fGarch")
  pars <- list(mean = .3, sd = 1.7, nu = 1.4)
  for (op in c("d", "p", "q")) {
    x <- if (op == "q") c(.001, .1, .5, .8, .999) else c(-3, -1, 0, 1, 4)
    args <- c(list(x), pars)
    actual <- do.call(get(paste0(op, "ged")), args)
    expected <- do.call(getExportedValue("fGarch", paste0(op, "ged")), args)
    expect_lt(max(abs(actual/expected - 1)), 1e-12)
    if (op == "d") {
      args$log <- TRUE
      expect_equal(do.call(dged, args), do.call(fGarch::dged, args), tolerance = 1e-12)
    }
  }
})

test_that("ged parameter domain is enforced", {
  for (bad in c(-1, 0, Inf))
    expect_warning(expect_true(is.nan(dged(0, nu = bad))), "NaNs produced")
})

test_that("ged log tails exceed the backend exponent range", {
  for (lower in c(TRUE, FALSE)) {
    x <- qged(-20000, lower.tail = lower, log.p = TRUE)
    expect_true(is.finite(x))
    expect_equal(pged(x, lower.tail = lower, log.p = TRUE), -20000,
                 tolerance = 1e-10)
    x <- qged(-1e-30, lower.tail = lower, log.p = TRUE)
    expect_equal(pged(x, lower.tail = lower, log.p = TRUE)/(-1e-30), 1,
                 tolerance = 1e-10)
  }
})

test_that("ged agrees with fGarch across ordinary shape/skew parameters", {
  skip_if_not_installed("fGarch")
  for (shape in c(.5, 1, 2, 5)) for (skew in 1) {
    pars <- list(mean = .3, sd = 1.7)
    pars$nu <- shape
    # Central probability positions keep comparisons in the reference's
    # accurate range even when the skew compresses one side of the density.
    observations <- do.call(qged, c(list(c(.1, .3, .5, .7, .9)), pars))
    for (op in c("d", "p", "q")) {
      args <- c(list(if (op == "q") c(.01, .1, .5, .9, .99) else observations), pars)
      actual <- do.call(get(paste0(op, "ged")), args)
      expected <- do.call(getExportedValue("fGarch", paste0(op, "ged")), args)
      expect_lt(max(abs(actual/expected - 1)), 1e-12)
    }
  }
})
