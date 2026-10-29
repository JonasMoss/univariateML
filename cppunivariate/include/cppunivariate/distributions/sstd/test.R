context("mlsstd")

## Deterministic interior data for successful-fit contracts.
set.seed(313)
small_data <- qsstd(((1:200) - .5)/200)
tiny_data <- qsstd(((1:100) - .5)/100)

## Finds errors with na and data out of bounds.
expect_error(mlsstd(c(tiny_data, NA)))

## Checks that na.rm works as intended.
expect_equal(
  coef(mlsstd(small_data)),
  coef(mlsstd(c(small_data, NA), na.rm = TRUE))
)

## Is the log-likelihood correct?
est <- mlsstd(small_data, na.rm = TRUE)
expect_equal(
  sum(dsstd(small_data, est[1], est[2], est[3], est[4], log = TRUE)),
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
expect_equal(attr(est, "model"), "Skew Student-t")
expect_equal(class(est), "univariateML")


## Check support.
expect_equal(class(attr(est, "support"))[[1]], "numeric")

## Boundary faces (decision D1; notes/ml-behaviour.tex).
test_that("mlsstd stops on the skew-normal face for normal data", {
  set.seed(1)
  expect_error(mlsstd(rnorm(200)), "skew-normal distribution. Use `mlsnorm`")
})

test_that("mlsstd stops on the translated half-normal and half-t faces", {
  x <- qnorm(0.5 + 0.5 * ppoints(200))
  expect_error(mlsstd(x), "endpoint at the sample minimum. Use `mlhnorm` to fit")
  expect_error(mlsstd(-x), "Use `mlhnorm` on `-x`")
  set.seed(3)
  y <- rexp(300)
  expect_error(mlsstd(y), "xi -> Inf, i.e. by a translated half-t with [0-9.]+ degrees of freedom and endpoint at the sample minimum")
  expect_error(mlsstd(-y), "xi -> 0, i.e. by a translated half-t with [0-9.]+ degrees of freedom and endpoint at the sample maximum")
})

test_that("mlsstd stops on the df-2 face for skewed Cauchy data", {
  z <- qcauchy(ppoints(200))
  expect_error(mlsstd(ifelse(z > 0, 2 * z, z)), "nu -> 2, i.e. by a two-piece t with 2 degrees of freedom")
})

## Slow oracle: the interior over dsstd, and each face fitted directly.
sstd_oracle <- function(x) {
  ll <- function(p) {
    v <- suppressWarnings(dsstd(x, p[1], exp(p[2]), 2 + exp(p[3]), exp(p[4]), log = TRUE))
    if (any(!is.finite(v))) -1e300 else sum(v)
  }
  interior <- -Inf
  for (nu in c(2.3, 4, 10, 100)) for (xi in c(0.5, 1, 2)) {
    o <- optim(c(mean(x), log(sd(x)), log(nu - 2), log(xi)), ll,
               control = list(fnscale = -1, maxit = 4000, reltol = 1e-14))
    o <- optim(o$par, ll, method = "BFGS", control = list(fnscale = -1, reltol = 1e-15))
    interior <- max(interior, o$value)
  }
  half_t <- -Inf
  for (y in list(x - min(x), max(x) - x)) for (nu in c(2.1, 4, 20, 1e3)) {
    f <- function(p) sum(log(2) - p[1] + dt(y / exp(p[1]), 2 + exp(p[2]), log = TRUE))
    o <- optim(c(log(sqrt(mean(y^2))), log(nu - 2)), f, control = list(fnscale = -1, reltol = 1e-14, maxit = 4000))
    half_t <- max(half_t, o$value)
  }
  t2 <- -Inf
  for (q in quantile(x, c(.25, .5, .75))) for (k in c(-1, 0, 1)) {
    f <- function(p) { d <- x - p[1]
      sum(log(2) - log(exp(p[2]) + exp(p[3])) + dt(ifelse(d < 0, d / exp(p[2]), d / exp(p[3])), 2, log = TRUE)) }
    s <- log(IQR(x) / 1.6)
    t2 <- max(t2, optim(c(q, s - k, s + k), f, control = list(fnscale = -1, reltol = 1e-14, maxit = 4000))$value)
  }
  hn <- function(y) length(y) * (0.5 * log(2 / pi) - log(sqrt(mean(y^2))) - 0.5)
  snorm <- tryCatch(attr(mlsnorm(x), "logLik"),
                    error = function(e) max(hn(x - min(x)), hn(max(x) - x)))
  list(interior = interior, faces = c(snorm = snorm, half_t = half_t, t2 = t2))
}

test_that("mlsstd agrees with the slow oracle on interior and boundary samples", {
  face_of <- function(msg) {
    if (grepl("skew-normal|half-normal", msg)) "snorm"
    else if (grepl("half-t", msg)) "half_t" else if (grepl("nu -> 2", msg)) "t2" else msg
  }
  # n = 20 family samples whose supremum is interior or on each face.
  cases <- list(c(5, 1.5, 5), c(5, 1.5, 7), c(5, 1.5, 1), c(5, 1.5, 2), c(10, 1, 8))
  for (case in cases) {
    set.seed(1000 * case[1] + case[3])
    x <- rsstd(20, nu = case[1], xi = case[2])
    o <- sstd_oracle(x)
    fit <- tryCatch(mlsstd(x), error = function(e) conditionMessage(e))
    if (is.character(fit)) {
      face <- face_of(fit)
      expect_true(face %in% names(o$faces), info = fit)
      expect_gte(o$faces[[face]], max(o$faces) - 1e-6)
      expect_lte(o$interior, max(o$faces) + 1e-6)
    } else {
      expect_gte(attr(fit, "logLik"), max(o$interior, o$faces) - 1e-6)
    }
  }
  set.seed(313)
  x <- rsstd(500)
  fit <- mlsstd(x)
  o <- sstd_oracle(x)
  expect_equal(attr(fit, "logLik"), o$interior, tolerance = 1e-8)
  expect_gt(attr(fit, "logLik"), max(o$faces))
})

test_that("sstd scale extremes preserve fit or declared escape", {
  set.seed(10)
  x <- simulate_default("mlsstd",50)
  base <- tryCatch(mlsstd(x),error=function(e) conditionMessage(e))
  for(s in c(1e-300,1e300)) {
    f <- tryCatch(mlsstd(x*s),error=function(e) conditionMessage(e))
    if(is.character(base)) expect_identical(f,base) else {
      expect_false(is.character(f))
      if(!is.character(f)) expect_equal(attr(f,"logLik")+length(x)*log(s),attr(base,"logLik"),tolerance=1e-4)
    }
  }
})

test_that("sstd large-sample fitting preserves likelihood under replication", {
  x<-qt(((1:1000)-.5)/1000,df=4)/sqrt(2)
  base<-mlsstd(x)
  y<-rep(x,100)
  f<-mlsstd(y)
  expect_equal(attr(f,"logLik")/100,attr(base,"logLik"),tolerance=1e-5)
})

test_that("skew t detects tie collapse before free df and face fits", {
  expect_error(mlsstd(c(rep(0,8),-1,1,2)),"unbounded.*multiplicity")
  for(k in c(5L,6L,7L)) {
    x <- c(rep(0,k),1,2,3)
    if(k==7L) expect_error(mlsstd(x),"unbounded")
    if(k==6L) expect_error(mlsstd(x),"equality.*not certified")
    if(k==5L) {
      f <- tryCatch(mlsstd(x),error=identity)
      if(inherits(f,"error")) expect_false(grepl("unbounded|equality",conditionMessage(f)))
      else expect_true(all(is.finite(f)))
    }
  }
})

# Package-owned dpqr contract, independent of the optional reference package.
test_that("sstd dpqr integrate, invert and preserve extreme tails", {
  pars <- list(mean = .3, sd = 1.7, nu = 5, xi = 1.8)
  d <- function(x, ...) do.call(dsstd, c(list(x), pars, list(...)))
  p <- function(q, ...) do.call(psstd, c(list(q), pars, list(...)))
  q <- function(p, ...) do.call(qsstd, c(list(p), pars, list(...)))
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

test_that("sstd dpqr recycle, propagate missing values and use R RNG", {
  expect_length(dsstd(numeric()), 0)
  expect_equal(dsstd(c(0, 1, 2), mean = c(0, 1)),
    c(dsstd(0), dsstd(1, mean = 1), dsstd(2)))
  expect_identical(dsstd(NA_real_), NA_real_)
  expect_true(is.nan(psstd(NaN)))
  expect_warning(expect_true(is.nan(dsstd(0, sd = -1))), "NaNs produced")
  expect_warning(expect_true(is.nan(qsstd(1.1))), "NaNs produced")
  expect_warning(expect_true(is.nan(rsstd(1, sd = -1))), "NaNs produced")
  set.seed(81); first <- rsstd(100)
  set.seed(81); expect_identical(rsstd(100), first)
  expect_length(rsstd(1:7), 7)
  expect_length(rsstd(0), 0)
  expect_length(rsstd(3, mean = numeric()), 0)
  expect_identical(rsstd(1, mean = NA_real_), NA_real_)
  set.seed(712)
  draws <- rsstd(5000)
  for (prob in c(.1, .5, .9))
    expect_lt(abs(mean(draws <= qsstd(prob)) - prob), .025)
  for (bad in c(-1, 0, Inf)) {
    expect_warning(expect_true(is.nan(dsstd(0, sd = bad))), "NaNs produced")
    expect_warning(expect_true(is.nan(psstd(0, sd = bad))), "NaNs produced")
    expect_warning(expect_true(is.nan(qsstd(.5, sd = bad))), "NaNs produced")
  }
  fit <- mlsstd(small_data)
  expect_equal(as.numeric(logLik(fit)), sum(dml(small_data, fit, log = TRUE)),
               tolerance = 1e-10)
  expect_equal(pml(0, fit), do.call(psstd, c(list(0), as.list(coef(fit)))))
  expect_equal(qml(.2, fit), do.call(qsstd, c(list(.2), as.list(coef(fit)))))
  set.seed(93); expected <- do.call(rsstd, c(list(10), as.list(coef(fit))))
  set.seed(93); expect_identical(rml(10, fit), expected)
})

test_that("sstd dpqr agree with accurate fGarch ordinary values", {
  skip_if_not_installed("fGarch")
  pars <- list(mean = .3, sd = 1.7, nu = 5, xi = 1.8)
  for (op in c("d", "p", "q")) {
    x <- if (op == "q") c(.001, .1, .5, .8, .999) else c(-3, -1, 0, 1, 4)
    args <- c(list(x), pars)
    actual <- do.call(get(paste0(op, "sstd")), args)
    expected <- do.call(getExportedValue("fGarch", paste0(op, "sstd")), args)
    expect_lt(max(abs(actual/expected - 1)), 1e-12)
    if (op == "d") {
      args$log <- TRUE
      expect_equal(do.call(dsstd, args), do.call(fGarch::dsstd, args), tolerance = 1e-12)
    }
  }
})

test_that("sstd parameter domain is enforced", {
  for (bad in c(-1, 0, 2, Inf))
    expect_warning(expect_true(is.nan(dsstd(0, nu = bad))), "NaNs produced")
})

test_that("sstd skew parameter domain is enforced", {
  for (bad in c(-1, 0, Inf))
    expect_warning(expect_true(is.nan(dsstd(0, xi = bad))), "NaNs produced")
})

test_that("sstd log tails exceed the backend exponent range", {
  for (lower in c(TRUE, FALSE)) {
    x <- qsstd(-20000, nu = 1000, lower.tail = lower, log.p = TRUE)
    expect_true(is.finite(x))
    expect_equal(psstd(x, nu = 1000, lower.tail = lower, log.p = TRUE), -20000,
                 tolerance = 1e-10)
    x <- qsstd(-1e-30, nu = 1000, lower.tail = lower, log.p = TRUE)
    expect_equal(psstd(x, nu = 1000, lower.tail = lower, log.p = TRUE)/(-1e-30), 1,
                 tolerance = 1e-10)
  }
})

test_that("sstd agrees with fGarch across ordinary shape/skew parameters", {
  skip_if_not_installed("fGarch")
  for (shape in c(2.1, 3, 10, 100)) for (skew in c(.25, 1, 4)) {
    pars <- list(mean = .3, sd = 1.7)
    pars$nu <- shape
    pars$xi <- skew
    # Central probability positions keep comparisons in the reference's
    # accurate range even when the skew compresses one side of the density.
    observations <- do.call(qsstd, c(list(c(.1, .3, .5, .7, .9)), pars))
    for (op in c("d", "p", "q")) {
      args <- c(list(if (op == "q") c(.01, .1, .5, .9, .99) else observations), pars)
      actual <- do.call(get(paste0(op, "sstd")), args)
      expected <- do.call(getExportedValue("fGarch", paste0(op, "sstd")), args)
      expect_lt(max(abs(actual/expected - 1)), 1e-12)
    }
  }
})
