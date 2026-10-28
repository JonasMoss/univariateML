context("mlparalogis")

## Data generation (actuar's shape/scale parameterisation).
set.seed(313)
small_data <- actuar::rparalogis(100, shape = 2, scale = 3)
medium_data <- actuar::rparalogis(1000, shape = 0.8, scale = 0.5)

oracle <- function(x) {
  nll <- function(p) -sum(actuar::dparalogis(x, shape = exp(p[1]), scale = exp(p[2]), log = TRUE))
  o <- optim(c(0, log(median(x))), nll, method = "BFGS", control = list(reltol = 1e-14, maxit = 1000))
  list(estimates = exp(o$par), logLik = -o$value)
}

## The fit matches a slow optimiser on the reference density.
for (x in list(small_data, medium_data)) {
  fit <- mlparalogis(x)
  o <- oracle(x)
  expect_gte(attr(fit, "logLik"), o$logLik - 1e-6)
  expect_equal(unname(coef(fit)), o$estimates, tolerance = 1e-3)
  expect_equal(attr(fit, "logLik"),
               sum(actuar::dparalogis(x, shape = coef(fit)[["shape"]], scale = coef(fit)[["scale"]], log = TRUE)))
}

## Both parameters are estimated (univariateML 1.x held the scale at 1).
expect_equal(names(coef(mlparalogis(small_data))), c("shape", "scale"))

## Control arguments: shape0, gradtol, iterlim.
fit_start <- mlparalogis(medium_data, shape0 = 2, gradtol = 1e-8)
expect_equal(attr(fit_start, "logLik"), attr(mlparalogis(medium_data, gradtol = 1e-8), "logLik"), tolerance = 1e-9)
expect_error(mlparalogis(medium_data, shape0 = 0), "`shape0` must be a positive finite number")

## Input checks.
expect_error(mlparalogis(c(small_data, NA)))
expect_error(mlparalogis(c(-1, small_data)))
expect_equal(attr(mlparalogis(small_data), "model"), "Paralogistic")
