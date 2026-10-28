context("mlstd")

## Data generation.
set.seed(313)
small_data <- fGarch::rstd(500, 2, 3, 4)
tiny_data <- fGarch::rstd(100, 2, 3, 4)

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
  sum(fGarch::dstd(small_data, est[1], est[2], est[3], log = TRUE)),
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
  expect_error(mlstd(fGarch::rstd(100, 2, 3, 4)),
               "nu -> 2 (infinite variance)", fixed = TRUE)
  x <- c(seq(-1e-4, 1e-4, length = 30),
         seq(0.9999, 1.0001, length = 10))
  expect_error(mlstd(x), "nu -> 2 (infinite variance)", fixed = TRUE)
})

test_that("Student-t interior fit and large-nu log likelihood agree with independent dt", {
  set.seed(313)
  x <- fGarch::rstd(500, nu = 4)
  fit <- mlstd(x)
  expect_equal(attr(fit, "logLik"),
               sum(fGarch::dstd(x, fit[1], fit[2], fit[3], log = TRUE)),
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
    objective <- function(p) -sum(fGarch::dstd(x, mean = p[1],
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
