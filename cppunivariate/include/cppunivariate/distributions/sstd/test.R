context("mlsstd")

## Data generation.
set.seed(313)
small_data <- fGarch::rsstd(100)
tiny_data <- fGarch::rsstd(100)

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
  sum(fGarch::dsstd(small_data, est[1], est[2], est[3], est[4], log = TRUE)),
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

## Slow oracle: the interior over fGarch::dsstd, and each face fitted directly.
sstd_oracle <- function(x) {
  ll <- function(p) {
    v <- suppressWarnings(fGarch::dsstd(x, p[1], exp(p[2]), 2 + exp(p[3]), exp(p[4]), log = TRUE))
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
    x <- fGarch::rsstd(20, nu = case[1], xi = case[2])
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
  x <- fGarch::rsstd(500)
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
