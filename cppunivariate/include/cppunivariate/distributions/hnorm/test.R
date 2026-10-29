# The translated half-normal is extraDistr's truncated normal with a = mean.
dhn <- function(x, mu, sigma, log = FALSE) ifelse(x >= mu, if (log) log(2) + dnorm(x, mu, sigma, log = TRUE) else 2*dnorm(x, mu, sigma), if (log) -Inf else 0)

test_that("translated half-normal closed form agrees with density oracle", {
  set.seed(73)
  x <- extraDistr::rtnorm(200, -3, 2, a = -3)
  fit <- mlhnorm(x, mu = NULL)
  mu <- unname(fit[1])
  # Closed boundary MLE min(x), represented exactly.
  expect_identical(mu, min(x))
  expect_lte(min(x) - mu, 2 * .Machine$double.eps * abs(min(x)))
  expect_equal(unname(coef(fit)), c(min(x), sqrt(mean((x - min(x))^2))), tolerance = 1e-12)
  oracle <- optimize(function(s) -sum(dhn(x, mu, s, log = TRUE)), c(0.01, 20), tol = 1e-10)
  expect_equal(unname(fit[2]), oracle$minimum, tolerance = 1e-7)
  expect_equal(attr(fit, "logLik"), -oracle$objective, tolerance = 1e-12)
  expect_equal(attr(fit, "logLik"), sum(dml(x, fit, log = TRUE)), tolerance = 1e-12)
  expect_lt(sum(dhn(x, mu - 0.01, fit[2], log = TRUE)), attr(fit, "logLik"))
  expect_equal(coef(mlhnorm(c(x, NA), na.rm = TRUE, mu = NULL)), coef(fit))
  # Location-scale equivariance.
  expect_equal(unname(coef(mlhnorm(5 + 3 * x, mu = NULL))), c(5 + 3 * mu, 3 * unname(fit[2])), tolerance = 1e-12)
  expect_error(mlhnorm(1, mu = NULL), "at least two observations with distinct values")
  expect_error(mlhnorm(rep(2, 4), mu = NULL), "degenerate data")
  expect_error(mlhnorm(c(x, NA), mu = NULL))
  expect_error(mlhnorm(c(x, Inf), mu = NULL))
  expect_error(mlhnorm(x, iterlim = 10, mu = NULL))
})

test_that("dml/pml/qml/rml map mu and sigma onto extraDistr's truncated normal", {
  fit <- mlhnorm(c(-4, -3, 0, 2), mu = NULL)
  mu <- unname(fit[1]); sigma <- unname(fit[2])
  x <- c(-5, -4, -3.5, 0, 3)
  # Translated half-normal density 2 * dnorm(x, mu, sigma) on [mu, Inf).
  expect_equal(dml(x, fit), ifelse(x >= mu, 2 * dnorm(x, mu, sigma), 0))
  expect_equal(dml(x[-1], fit, log = TRUE), log(2) + dnorm(x[-1], mu, sigma, log = TRUE))
  expect_equal(pml(x, fit), pmax(0, 2 * pnorm(x, mu, sigma) - 1))
  u <- c(0.001, 0.1, 0.5, 0.9, 0.999)
  expect_equal(qml(u, fit), mu + sigma * qnorm((1 + u) / 2))
  for (lower in c(TRUE, FALSE)) for (logs in c(TRUE, FALSE)) {
    p <- if (logs) log(u) else u
    expect_equal(pml(qml(p, fit, lower, logs), fit, lower), u, tolerance = 1e-10)
  }
  set.seed(9); actual <- rml(50, fit)
  set.seed(9); expect_equal(actual, extraDistr::rtnorm(50, mu, sigma, a = mu))
  expect_true(all(actual >= mu))
  expect_length(simulate_default("mlhnorm", 10), 10)
  expect_true(all(simulate_default("mlhnorm", 100) >= 0))
})

test_that("bootstrap refits with mlhnorm", {
  set.seed(5)
  fit <- mlhnorm(extraDistr::rtnorm(30, 1, 2, a = 1), mu = NULL)
  boot <- bootstrapml(fit, reps = 20, reducer = identity)
  expect_equal(dim(boot), c(2L, 20L))
  expect_true(all(boot["sigma", ] > 0))
})

test_that("active truncated-normal repairs retain the half-normal map and endpoints", {
  expect_identical(univariateML:::univariateML_args$mlhnorm,
                   c(mean = "mu", sd = "sigma", a = "mu"))
  expect_true(univariateML:::univariateML_reference_fixes[["extraDistr::dtnorm"]]$broken())
  fit <- mlhnorm(c(-4,-3,0,2), mu = NULL)
  mu <- unname(fit[1]); sigma <- unname(fit[2])
  x <- mu + sigma*c(-1,0,.01,.5,2)
  expect_equal(dml(x, fit), ifelse(x >= mu, 2*dnorm(x, mu, sigma), 0), tolerance = 1e-12)
  expect_equal(dml(mu, fit, log = TRUE), log(2) + dnorm(mu, mu, sigma, log = TRUE))
  for (lower in c(TRUE,FALSE)) for (logs in c(TRUE,FALSE)) {
    upper <- pmin(1, 2*pnorm(x, mu, sigma, lower.tail = FALSE))
    expected <- if (lower) 1-upper else upper
    if (logs) expected <- log(expected)
    expect_equal(pml(x, fit, lower.tail = lower, log.p = logs), expected, tolerance = 1e-12)
    u <- c(0,.1,.5,.9,1)
    expected <- mu + sigma*qnorm(if (lower) (1-u)/2 else u/2, lower.tail = FALSE)
    expect_equal(qml(if (logs) log(u) else u, fit, lower.tail = lower, log.p = logs),
                 expected, tolerance = 1e-12)
  }
  set.seed(12); actual <- rml(30, fit)
  set.seed(12); expect_identical(actual, extraDistr::rtnorm(30, mu, sigma, a = mu))
})

test_that("half-normal preserves exact endpoints at large translations", {
  x <- 1e15 + c(0, 1, 2, 3)
  f <- mlhnorm(x, mu = NULL)
  expect_equal(f[["sigma"]], sqrt(mean((x - f[["mu"]])^2)))
  expect_equal(attr(f, "logLik"), sum(dml(x, f, log = TRUE)))
})
test_that("held half-normal endpoint is exact, closed and counted as fixed", {
  x <- c(0,1,2,5)
  f <- mlhnorm(x,mu=0)
  expect_identical(f[["mu"]],0)
  expect_equal(f[["sigma"]],sqrt(mean(x^2)))
  expect_equal(attr(f,"fixed"),"mu")
  expect_equal(attr(logLik(f),"df"),1L)
  expect_equal(AIC(f),-2*attr(f,"logLik")+2)
  expect_equal(dml(x,f),extraDistr::dhnorm(x,f[["sigma"]]))
  expect_equal(sum(dml(x,f,log=TRUE)),attr(f,"logLik"))
  expect_equal(mlhnorm(3,mu=0)[["sigma"]],3)
  expect_equal(mlhnorm(x+4,mu=4)[["sigma"]],f[["sigma"]])
  expect_error(mlhnorm(c(-1,1),mu=0),"at least the held mu")
  expect_error(mlhnorm(c(0,0),mu=0),"unbounded.*sigma -> 0")
  expect_error(mlhnorm(x,mu=Inf),"finite")
  for(lower in c(TRUE,FALSE)) for(lp in if(lower) -100 else c(-100,-1000))
    expect_equal(pml(qml(lp,f,lower,TRUE),f,lower,TRUE),lp,tolerance=1e-10)
  set.seed(1); a <- rml(20,f)
  set.seed(1); expect_identical(a,extraDistr::rtnorm(20, mean=f[["mu"]], sd=f[["sigma"]], a=f[["mu"]]))
  set.seed(1); boot <- bootstrapml(f,reps=3,reducer=identity)
  expect_true(all(boot["mu",]==0))
})

test_that("held half-normal scale and every combined hold match density oracles", {
  set.seed(8101)
  samples <- list(abs(rnorm(20)),abs(rnorm(100)),c(0,0,1,2),1,rep(2,4))
  for(x in samples) for(mu in list(NULL,min(x)-.5)) {
    f <- mlhnorm(x,mu=mu,sigma=1.7)
    expect_identical(unname(f["sigma"]),1.7)
    expect_setequal(attr(f,"fixed"),c(if(!is.null(mu))"mu","sigma"))
    ll <- function(m) sum(log(2)+dnorm(x,m,1.7,log=TRUE))
    if(is.null(mu)) {
      # Independent constrained scalar optimization, plus its exact endpoint.
      best <- min(vapply(c(.1,1,10),function(gap)
        nlminb(min(x)-gap,function(m)-ll(m),upper=min(x),
          control=list(rel.tol=1e-12))$objective,numeric(1)))
      expect_equal(attr(f,"logLik"),-best,tolerance=1e-10)
      expect_equal(f[["mu"]],min(x),tolerance=1e-12)
    } else expect_identical(unname(f["mu"]),mu)
    expect_equal(attr(f,"logLik"),ll(f[["mu"]]),tolerance=1e-12)
    expect_equal(sum(dml(x,f,log=TRUE)),attr(f,"logLik"),tolerance=1e-12)
  }
  expect_s3_class(mlhnorm(c(0,0),mu=0,sigma=1),"univariateML")
  for(bad in c(0,-1,Inf,NaN)) expect_error(mlhnorm(1:3,mu=NULL,sigma=bad),"sigma.*positive finite")
  expect_error(mlhnorm(1:3,mu=2,sigma=1),"at least")
  expect_equal(as.numeric(logLik(mlhnorm(1e300,mu=NULL,sigma=1e-300))), log(2/pi)/2-log(1e-300))
})
