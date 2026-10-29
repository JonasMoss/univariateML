test_that("location-scale t agrees with independent extraDistr likelihood optimization", {
  for (df in c(1.5, 3, 10)) for (n in c(200, 1000)) {
    set.seed(100 + n + df * 10)
    x <- extraDistr::rlst(n, df = df, mu = 2, sigma = 1.3)
    fit <- mllst(x, gradtol = 1e-7, iterlim = 500)
    objective <- function(p) -sum(extraDistr::dlst(
      x, df = 1 + exp(p[3]), mu = p[1], sigma = exp(p[2]), log = TRUE))
    candidates <- lapply(c(1.1, 2, 5, 20, 100), function(d)
      optim(c(median(x), log(IQR(x)), log(d - 1)), objective,
            method = "BFGS", control = list(maxit = 2000, reltol = 1e-12)))
    oracle <- candidates[[which.min(vapply(candidates, `[[`, 0.0, "value"))]]
    expect_equal(attr(fit, "logLik"), -oracle$value, tolerance = 1e-8)
    expect_equal(as.numeric(fit), c(oracle$par[1], exp(oracle$par[2]),
                                  1 + exp(oracle$par[3])), tolerance = 2e-3)
    expect_equal(names(fit), c("mu", "sigma", "df"))
    expect_equal(attr(fit, "logLik"), sum(extraDistr::dlst(
      x, df = fit[3], mu = fit[1], sigma = fit[2], log = TRUE)), tolerance = 1e-10)
    expect_equal(dml(x[1:3], fit), extraDistr::dlst(
      x[1:3], df = fit[3], mu = fit[1], sigma = fit[2]))
  }
})

test_that("Cauchy is attained and normal escapes name mlnorm", {
  set.seed(1)
  x <- rcauchy(200, location = 2, scale = 1.3)
  fit <- mllst(x)
  expect_equal(unname(fit[3]), 1)
  expect_equal(as.numeric(fit[1:2]), as.numeric(mlcauchy(x)), tolerance = 1e-3)
  expect_error(mllst(qnorm(((1:200) - 0.5) / 200)), "Use `mlnorm`", fixed = TRUE)
  set.seed(1)
  expect_error(mllst(rnorm(200)), "Use `mlnorm`", fixed = TRUE)
})

test_that("collapse configurations are rejected and allowed ties fit", {
  for (x in list(c(1, 2), rep(1, 5), c(0, 0, 1, 2), c(0, 0, 0, 1, 2)))
    expect_error(mllst(x), "no value repeated n/2", fixed = TRUE)
  expect_error(mllst(c(1:5, NA)))
  expect_s3_class(mllst(c(0, 0, 1, 2, 10)), "univariateML")
})

test_that("std two-cluster escape has a location-scale fit", {
  x <- c(seq(-1e-4, 1e-4, length = 30), seq(0.9999, 1.0001, length = 10))
  expect_error(mlstd(x), "Use `mllst`", fixed = TRUE)
  fit <- mllst(x)
  expect_true(is.finite(attr(fit, "logLik")))
  # Free df prefers Cauchy here: the std face at df=2 is not the lst MLE.
  expect_equal(unname(fit[3]), 1)
  t2 <- optim(c(0, log(1e-4)), function(p) -sum(extraDistr::dlst(
    x, df = 2, mu = p[1], sigma = exp(p[2]), log = TRUE)),
    method = "BFGS", control = list(maxit = 2000, reltol = 1e-12))
  expect_gt(attr(fit, "logLik"), -t2$value + 70)
})

test_that("fixed df agrees with oracle and Cauchy, with correct metadata", {
  x <- c(-5,-2,-1,0,0,1,2,9)
  for (df in c(1, 2.5, 10)) {
    fit <- mllst(x, df = df, gradtol = 1e-8, iterlim = 5000)
    objective <- function(p) -sum(extraDistr::dlst(x, df = df,
      mu = p[1], sigma = exp(p[2]), log = TRUE))
    oracle <- optim(c(0,0), objective, method = "BFGS",
                    control = list(reltol = 1e-13, maxit = 3000))
    expect_equal(attr(fit, "logLik"), -oracle$value, tolerance = 1e-9)
    expect_equal(as.numeric(fit[1:2]), c(oracle$par[1],exp(oracle$par[2])), tolerance = 1e-4)
    expect_identical(attr(fit, "fixed"), "df")
    expect_equal(attr(logLik(fit), "df"), 2)
    expect_identical(unname(fit[3]), df)
    if (df == 1) expect_equal(as.numeric(fit[1:2]), as.numeric(mlcauchy(x)), tolerance = 1e-3)
  }
  for (df in c(-1,0,.5,Inf,NaN)) expect_error(mllst(x, df = df), "at least 1")
  expect_error(mllst(c(0,0,0,1), df = 2), "unbounded")
  expect_error(mllst(c(0,0,0,1), df = 3), "equality")
  expect_s3_class(mllst(c(0,0,0,1), df = 4, iterlim = 5000), "univariateML")
  expect_s3_class(mllst(qnorm((1:100-.5)/100), df = 4), "univariateML")
})

test_that("held huge degrees of freedom retain the normal log density", {
  x <- c(-2, -1, 0, 1, 3)
  f <- mllst(x, df = 1e300)
  expect_equal(attr(f, "logLik"), sum(dt((x-f[["mu"]])/f[["sigma"]], df=1e300, log=TRUE)-log(f[["sigma"]])), tolerance=1e-10)
  expect_equal(attr(f, "logLik"), attr(mlnorm(x), "logLik"), tolerance=1e-8)
})

test_that("held Cauchy degrees of freedom allow balanced two-point maxima", {
  f <- mllst(c(1,1,2,2),df=1)
  g <- mlcauchy(c(1,1,2,2))
  expect_equal(f[["mu"]],g[["location"]])
  expect_equal(f[["sigma"]],g[["scale"]])
  expect_equal(attr(f,"logLik"),attr(g,"logLik"))
})
