context("mlstd")

## Data generation.
set.seed(313)
small_data <- rstd(500, 2, 3, 4)
tiny_data <- rstd(100, 2, 3, 4)

## Finds errors with na and data out of bounds.
expect_error(mlstd(c(tiny_data, NA)))

## Checks that na.rm works as intended.
expect_equal(
  coef(mlstd(small_data)),
  coef(mlstd(c(small_data, NA), na.rm = TRUE))
)

## Is the log-likelihood correct?
est <- mlstd(small_data, na.rm = TRUE)
expect_equal(
  sum(dstd(small_data, est[1], est[2], est[3], log = TRUE)),
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
expect_equal(attr(est, "model"), "Student-t")
expect_equal(class(est), "univariateML")


## Check support.
expect_equal(class(attr(est, "support"))[[1]], "numeric")


test_that("Student-t escapes name the limiting model", {
  set.seed(1)
  expect_error(mlstd(rnorm(200)), "Use `mlnorm`", fixed = TRUE)
  set.seed(313)
  expect_error(mlstd(rstd(100, 2, 3, 4)),
               "nu -> 2 (infinite variance)", fixed = TRUE)
  x <- c(seq(-1e-4, 1e-4, length = 30),
         seq(0.9999, 1.0001, length = 10))
  expect_error(mlstd(x), "nu -> 2 (infinite variance)", fixed = TRUE)
})

test_that("Student-t interior fit and large-nu log likelihood agree with independent dt", {
  set.seed(313)
  x <- rstd(500, nu = 4)
  fit <- mlstd(x)
  expect_equal(attr(fit, "logLik"),
               sum(dstd(x, fit[1], fit[2], fit[3], log = TRUE)),
               tolerance = 1e-10)
  for (nu in c(10, 100, 343, 344, 1000, 9999, 1e4, 1e5, 1e6, 1e7)) {
    expect_equal(univariateML:::student_t_loglik_cpp(x, 0.3, 1.2, nu),
                 sum(dt((x - 0.3) / (1.2 * sqrt((nu-2)/nu)), nu, log = TRUE) -
                       log(1.2 * sqrt((nu-2)/nu))),
                 tolerance = 1e-10)
  }
})

test_that("large-nu interior fit matches dt and a slow oracle", {
  x <- qt(((1:4000) - .5)/4000, df = 300)
  fit <- mlstd(x, gradtol = 1e-7, iterlim = 500)
  expect_gt(unname(fit[3]), 343)
  objective <- function(p) {
    nu <- 2 + exp(p[3]); s <- exp(p[2])*sqrt((nu-2)/nu)
    -sum(dt((x-p[1])/s, nu, log = TRUE) - log(s))
  }
  p <- c(fit[1], log(fit[2]), log(fit[3]-2))
  expect_equal(attr(fit, "logLik"), -objective(p), tolerance = 1e-11)
  oracles <- lapply(c(100, 500, 2000), function(nu)
    optim(c(0, 0, log(nu-2)), objective, method = "Nelder-Mead",
          control = list(maxit = 3000, reltol = 1e-13)))
  best <- oracles[[which.min(vapply(oracles, `[[`, 0, "value"))]]
  expect_lt(abs(attr(fit, "logLik") + best$value), 1e-5)
  expect_equal(unname(fit[3]), 2 + exp(best$par[3]), tolerance = .02)
})

test_that("fixed nu agrees with density oracle and records two free parameters", {
  x <- c(-5,-2,-1,0,0,1,2,9)
  for (nu in c(2.1,4,30)) {
    fit <- mlstd(x, nu = nu, gradtol = 1e-8, iterlim = 5000)
    objective <- function(p) -sum(dstd(x, mean = p[1],
      sd = exp(p[2]), nu = nu, log = TRUE))
    oracle <- optim(c(0,1), objective, method = "BFGS",
                    control = list(reltol = 1e-13, maxit = 3000))
    expect_equal(attr(fit, "logLik"), -oracle$value, tolerance = 1e-9)
    expect_equal(as.numeric(fit[1:2]), c(oracle$par[1],exp(oracle$par[2])), tolerance = 1e-4)
    expect_identical(attr(fit, "fixed"), "nu")
    expect_equal(attr(logLik(fit), "df"), 2)
    expect_identical(unname(fit[3]), nu)
  }
  for (nu in c(-1,0,2,Inf,NaN)) expect_error(mlstd(x, nu = nu), "greater than 2")
  expect_error(mlstd(c(0,0,0,1), nu = 2.5), "unbounded")
  expect_error(mlstd(c(0,0,0,1), nu = 3), "equality")
  expect_error(mlstd(x, nu = 4, iterlim = 0), "score tolerance")
  expect_s3_class(mlstd(qnorm((1:100-.5)/100), nu = 4), "univariateML")
})

test_that("std scale extremes preserve fit or declared escape", {
  set.seed(10)
  x <- simulate_default("mlstd",50)
  base <- tryCatch(mlstd(x),error=function(e) conditionMessage(e))
  for(s in c(1e-300,1e300)) {
    f <- tryCatch(mlstd(x*s),error=function(e) conditionMessage(e))
    if(is.character(base)) expect_identical(f,base) else {
      expect_false(is.character(f))
      if(!is.character(f)) expect_equal(attr(f,"logLik")+length(x)*log(s),attr(base,"logLik"),tolerance=1e-4)
    }
  }
})

test_that("std large-sample fitting preserves likelihood under replication", {
  x<-qt(((1:1000)-.5)/1000,df=4)/sqrt(2)
  base<-mlstd(x)
  y<-rep(x,100)
  f<-mlstd(y)
  expect_equal(attr(f,"logLik")/100,attr(base,"logLik"),tolerance=1e-5)
})

test_that("free Student t detects strict tie collapse and separates equality", {
  expect_error(mlstd(c(rep(0,8),1,2,3)),"unbounded.*multiplicity")
  for (k in c(5L,6L,7L)) {
    x <- c(rep(0,k),1,2,3)
    if(k==7L) expect_error(mlstd(x),"unbounded")
    if(k==6L) expect_error(mlstd(x),"equality.*not certified")
    if(k==5L) {
      f <- tryCatch(mlstd(x),error=identity)
      if(inherits(f,"error")) expect_false(grepl("unbounded|equality",conditionMessage(f)))
      else expect_true(all(is.finite(f)))
    }
  }
  expect_error(mlstd(c(rep(0,8),1,2,3),nu=2.5),"unbounded")
  expect_error(mlstd(c(rep(0,8),1,2,3),nu=8/3),"equality")
})

# Package-owned dpqr contract, independent of the optional reference package.
test_that("std dpqr integrate, invert and preserve extreme tails", {
  pars <- list(mean = .3, sd = 1.7, nu = 5)
  d <- function(x, ...) do.call(dstd, c(list(x), pars, list(...)))
  p <- function(q, ...) do.call(pstd, c(list(q), pars, list(...)))
  q <- function(p, ...) do.call(qstd, c(list(p), pars, list(...)))
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

test_that("std dpqr recycle, propagate missing values and use R RNG", {
  expect_length(dstd(numeric()), 0)
  expect_equal(dstd(c(0, 1, 2), mean = c(0, 1)),
    c(dstd(0), dstd(1, mean = 1), dstd(2)))
  expect_identical(dstd(NA_real_), NA_real_)
  expect_true(is.nan(pstd(NaN)))
  expect_warning(expect_true(is.nan(dstd(0, sd = -1))), "NaNs produced")
  expect_warning(expect_true(is.nan(qstd(1.1))), "NaNs produced")
  expect_warning(expect_true(is.nan(rstd(1, sd = -1))), "NaNs produced")
  set.seed(81); first <- rstd(100)
  set.seed(81); expect_identical(rstd(100), first)
  expect_length(rstd(1:7), 7)
  expect_length(rstd(0), 0)
  expect_length(rstd(3, mean = numeric()), 0)
  expect_identical(rstd(1, mean = NA_real_), NA_real_)
  set.seed(712)
  draws <- rstd(5000)
  for (prob in c(.1, .5, .9))
    expect_lt(abs(mean(draws <= qstd(prob)) - prob), .025)
  for (bad in c(-1, 0, Inf)) {
    expect_warning(expect_true(is.nan(dstd(0, sd = bad))), "NaNs produced")
    expect_warning(expect_true(is.nan(pstd(0, sd = bad))), "NaNs produced")
    expect_warning(expect_true(is.nan(qstd(.5, sd = bad))), "NaNs produced")
  }
  fit <- mlstd(small_data)
  expect_equal(as.numeric(logLik(fit)), sum(dml(small_data, fit, log = TRUE)),
               tolerance = 1e-10)
  expect_equal(pml(0, fit), do.call(pstd, c(list(0), as.list(coef(fit)))))
  expect_equal(qml(.2, fit), do.call(qstd, c(list(.2), as.list(coef(fit)))))
  set.seed(93); expected <- do.call(rstd, c(list(10), as.list(coef(fit))))
  set.seed(93); expect_identical(rml(10, fit), expected)
})

test_that("std dpqr agree with accurate fGarch ordinary values", {
  skip_if_not_installed("fGarch")
  pars <- list(mean = .3, sd = 1.7, nu = 5)
  for (op in c("d", "p", "q")) {
    x <- if (op == "q") c(.001, .1, .5, .8, .999) else c(-3, -1, 0, 1, 4)
    args <- c(list(x), pars)
    actual <- do.call(get(paste0(op, "std")), args)
    expected <- do.call(getExportedValue("fGarch", paste0(op, "std")), args)
    expect_lt(max(abs(actual/expected - 1)), 1e-12)
    if (op == "d") {
      args$log <- TRUE
      expect_equal(do.call(dstd, args), do.call(fGarch::dstd, args), tolerance = 1e-12)
    }
  }
})

test_that("std log density survives the fGarch underflow defect", {
  expected <- dt(1e6*sqrt(1e6/(1e6-2)), df = 1e6, log = TRUE) +
    log(sqrt(1e6/(1e6-2)))
  expect_true(is.finite(dstd(1e6, nu = 1e6, log = TRUE)))
  expect_equal(dstd(1e6, nu = 1e6, log = TRUE), expected, tolerance = 1e-13)
})

test_that("std parameter domain is enforced", {
  for (bad in c(-1, 0, 2, Inf))
    expect_warning(expect_true(is.nan(dstd(0, nu = bad))), "NaNs produced")
})

test_that("std log tails exceed the backend exponent range", {
  for (lower in c(TRUE, FALSE)) {
    x <- qstd(-20000, nu = 1000, lower.tail = lower, log.p = TRUE)
    expect_true(is.finite(x))
    expect_equal(pstd(x, nu = 1000, lower.tail = lower, log.p = TRUE), -20000,
                 tolerance = 1e-10)
    x <- qstd(-1e-30, nu = 1000, lower.tail = lower, log.p = TRUE)
    expect_equal(pstd(x, nu = 1000, lower.tail = lower, log.p = TRUE)/(-1e-30), 1,
                 tolerance = 1e-10)
  }
})

test_that("std agrees with fGarch across ordinary shape/skew parameters", {
  skip_if_not_installed("fGarch")
  for (shape in c(2.1, 3, 10, 100)) for (skew in 1) {
    pars <- list(mean = .3, sd = 1.7)
    pars$nu <- shape
    # Central probability positions keep comparisons in the reference's
    # accurate range even when the skew compresses one side of the density.
    observations <- do.call(qstd, c(list(c(.1, .3, .5, .7, .9)), pars))
    for (op in c("d", "p", "q")) {
      args <- c(list(if (op == "q") c(.01, .1, .5, .9, .99) else observations), pars)
      actual <- do.call(get(paste0(op, "std")), args)
      expected <- do.call(getExportedValue("fGarch", paste0(op, "std")), args)
      expect_lt(max(abs(actual/expected - 1)), 1e-12)
    }
  }
})
