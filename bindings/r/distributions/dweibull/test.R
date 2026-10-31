context("mldweibull")

# Slow oracle: internal density on moderate counts; multistart
# BFGS for the free fit, optimize() for each held subset.
dweibull_oracle <- function(x, shape1 = NULL, shape2 = NULL) {
  ll <- function(q, b) {
    if (!is.finite(q) || q <= 0 || q >= 1 || !is.finite(b) || b <= 0) return(-1e100)
    v <- sum(ddweibull(x = x, shape1 = q, shape2 = b, log = TRUE))
    if (is.finite(v)) v else -1e100
  }
  if (!is.null(shape1)) {
    return(optimize(function(u) ll(shape1, exp(u)), c(-6, 4), maximum = TRUE, tol = 1e-12)$objective)
  }
  if (!is.null(shape2)) {
    return(optimize(function(z) ll(plogis(z), shape2), c(-15, 15), maximum = TRUE, tol = 1e-12)$objective)
  }
  starts <- expand.grid(q = c(.3, .7, .95), b = c(.5, 1, 2))
  max(vapply(seq_len(nrow(starts)), function(i) {
    p <- c(qlogis(starts$q[i]), log(starts$b[i]))
    -optim(p, function(p) -ll(plogis(p[1]), exp(p[2])), method = "BFGS",
           control = list(reltol = 1e-14, maxit = 2000))$value
  }, numeric(1)))
}

test_that("mldweibull and its held subsets agree with the density oracle", {
  skip_unless_tier("full")
  set.seed(20261028)
  for (par in list(c(.8, 1.5), c(.5, .7), c(.95, 2.5), c(.9, .5))) {
    x <- rdweibull(n = 300, shape1 = par[1], shape2 = par[2])
    fit <- mldweibull(x)
    expect_equal(attr(fit, "logLik"), dweibull_oracle(x), tolerance = 1e-9)
    expect_equal(attr(fit, "logLik"), sum(dml(x, fit, log = TRUE)), tolerance = 1e-12)
    expect_equal(attr(fit, "fixed"), character(0))
    expect_equal(attr(logLik(fit), "df"), 2L)

    held2 <- mldweibull(x, shape2 = 1.2)
    expect_equal(unname(held2[["shape2"]]), 1.2)
    expect_equal(attr(held2, "fixed"), "shape2")
    expect_equal(attr(logLik(held2), "df"), 1L)
    expect_equal(attr(held2, "logLik"), dweibull_oracle(x, shape2 = 1.2), tolerance = 1e-10)

    held1 <- mldweibull(x, shape1 = par[1])
    expect_equal(unname(held1[["shape1"]]), par[1])
    expect_equal(attr(held1, "fixed"), "shape1")
    expect_equal(attr(logLik(held1), "df"), 1L)
    expect_equal(attr(held1, "logLik"), dweibull_oracle(x, shape1 = par[1]), tolerance = 1e-10)

    both <- mldweibull(x, shape1 = par[1], shape2 = par[2])
    expect_equal(attr(both, "fixed"), c("shape1", "shape2"))
    expect_equal(attr(logLik(both), "df"), 0L)
    expect_equal(attr(both, "logLik"), sum(ddweibull(x = x, shape1 = par[1], shape2 = par[2], log = TRUE)))
  }
})

test_that("shape2 = 1 is the attained geometric member", {
  set.seed(3)
  x <- rgeom(200, .3)
  fit <- mldweibull(x, shape2 = 1)
  geom <- mlgeom(x)
  expect_equal(unname(fit[["shape1"]]), 1 - unname(geom[["prob"]]))
  expect_equal(attr(fit, "logLik"), attr(geom, "logLik"))
})

test_that("each escape is reported from data of its limit law", {
  expect_error(mldweibull(rep(3, 20)), "point mass at 3")
  expect_error(mldweibull(rep(0, 20)), "point mass at 0")
  expect_error(mldweibull(rep(0, 20), shape2 = 2), "with `shape2` held.*point mass at 0")
  set.seed(4)
  bern <- rbinom(200, 1, .3)
  expect_error(mldweibull(bern), "Use `mlbinom\\(x, size = 1\\)`")
  expect_equal(unname(mlbinom(bern, size = 1)[["prob"]]), mean(bern))
  expect_error(mldweibull(5 + rbinom(200, 1, .6)), "adjacent values 5 and 6")
  expect_error(mldweibull(bern, shape1 = .4), "success probability `shape1`")
  expect_error(mldweibull(rep(1, 5), shape1 = .4), "success probability `shape1`")
  expect_warning(flat <- mldweibull(rep(0, 10), shape1 = .4), "not unique")
  expect_equal(unname(flat[["shape2"]]), 1)
  expect_equal(attr(flat, "logLik"), 10 * log(.6))
  # Held subsets that have no such face fit these samples.
  expect_s3_class(mldweibull(rep(3, 20), shape1 = .5), "univariateML")
  expect_s3_class(mldweibull(rep(3, 20), shape2 = 2), "univariateML")
  expect_s3_class(mldweibull(5 + rbinom(200, 1, .6), shape2 = 2), "univariateML")
  expect_s3_class(mldweibull(c(0, 2)), "univariateML")
})

test_that("invalid held values and shape1 rounding to 1 stop", {
  for (bad in c(0, 1, -1, NaN)) expect_error(mldweibull(0:4, shape1 = bad), "strictly between")
  for (bad in c(0, -1, Inf, NaN)) expect_error(mldweibull(0:4, shape2 = bad), "finite and positive")
  set.seed(5)
  peaked <- floor(40 * rexp(2000)^(1 / 12))
  expect_error(mldweibull(peaked), "rounds to 1")
})

test_that("large samples and many distinct counts", {
  set.seed(6)
  x <- rdweibull(n = 1e+05, shape1 = 0.8, shape2 = 1.5)
  fit <- mldweibull(x)
  expect_equal(unname(fit[["shape2"]]), 1.5, tolerance = .02)
  # Heavy tail: thousands of distinct counts.
  heavy <- pmax(rdweibull(n = 5000, shape1 = 0.9, shape2 = 0.3), 0)
  expect_gt(length(unique(heavy)), 1000)
  fit <- mldweibull(heavy)
  expect_equal(unname(fit[["shape2"]]), .3, tolerance = .1)
  expect_equal(attr(fit, "logLik"), sum(dml(heavy, fit, log = TRUE)), tolerance = 1e-9)
})

test_that("pml floors non-integer arguments", {
  set.seed(7)
  fit <- mldweibull(rdweibull(n = 200, shape1 = 0.8, shape2 = 1.5))
  q <- c(-0.5, 0, 0.5, 2.7, 5, Inf)
  for (lower in c(TRUE, FALSE)) for (logp in c(TRUE, FALSE)) {
    log_upper <- log(fit[["shape1"]]) * (pmax(0, floor(q) + 1)^fit[["shape2"]])
    expected <- if (lower) -expm1(log_upper) else exp(log_upper)
    if (logp) expected <- if (lower) log(-expm1(log_upper)) else log_upper
    expect_equal(pml(q, fit, lower.tail = lower, log.p = logp), expected)
  }
  expect_equal(pml(0.5, fit), pml(0, fit))

})

test_that("bootstrapml refits with the held shape2", {
  set.seed(8)
  fit <- mldweibull(rdweibull(n = 100, shape1 = 0.8, shape2 = 1.5), shape2 = 1.5)
  boot <- bootstrapml(fit, reps = 5, reducer = identity)
  expect_true(all(boot["shape2", ] == 1.5))
})


test_that("dweibull: P3 unfinished optimizer stops", {
  g <- jsonlite::fromJSON(file.path(Sys.getenv("UMVL_DISTRIBUTIONS_DIR"), "dweibull", "golden.json"))
  x <- g$data
  expect_error(mldweibull(x, iterlim=1, reltol=1e-10),
               "[Ff]ail|converge|unresolved|exhausted|score check")
})
