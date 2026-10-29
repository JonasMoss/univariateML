context("mlgumbel")

## Data generation.
set.seed(313)
small_data <- extraDistr::rgumbel(100, 1, 1)
tiny_data <- extraDistr::rgumbel(10, 3, 7)
medium_data <- extraDistr::rgumbel(1000, 9, 11)

## Checks if the ML is correct.
mle1 <- suppressWarnings(nlm(function(p) {
  -sum(extraDistr::dgumbel(small_data, p[1], p[2], log = TRUE))
}, p = c(1, 1)))

mle2 <- nlm(function(p) {
  -sum(extraDistr::dgumbel(tiny_data, p[1], p[2], log = TRUE))
}, p = c(3, 7))

mle3 <- nlm(function(p) {
  -sum(extraDistr::dgumbel(medium_data, p[1], p[2], log = TRUE))
}, p = c(9, 11))

expect_equal(mle1$estimate, as.numeric(mlgumbel(small_data)),
  tolerance = 1e-5
)
expect_equal(mle2$estimate, as.numeric(mlgumbel(tiny_data)),
  tolerance = 1e-5
)
expect_equal(mle3$estimate, as.numeric(mlgumbel(medium_data)),
  tolerance = 1e-5
)

expect_equal(-mle1$minimum, attr(mlgumbel(small_data), "logLik"),
  tolerance = 1e-5
)
expect_equal(-mle2$minimum, attr(mlgumbel(tiny_data), "logLik"),
  tolerance = 1e-5
)
expect_equal(-mle3$minimum, attr(mlgumbel(medium_data), "logLik"),
  tolerance = 1e-5
)

## Checks warning with small iterlim.
expect_warning(mlgumbel(tiny_data, iterlim = 1))

## Finds errors with na and data out of bounds.
expect_error(mlgumbel(c(tiny_data, NA)))

## Checks that na.rm works as intended.
expect_equal(
  coef(mlgumbel(small_data)),
  coef(mlgumbel(c(small_data, NA), na.rm = TRUE))
)

est <- mlgumbel(tiny_data)
## Check class.
expect_equal(attr(est, "model"), "Gumbel")
expect_equal(class(est), "univariateML")


## Check support.
expect_equal(class(attr(est, "support"))[[1]], "numeric")


test_that("gumbel handles a seeded sample with a large outlier", {
  set.seed(7)
  x <- c(extraDistr::rgumbel(199), 1e3)
  objective <- function(p) -sum(extraDistr::dgumbel(x, p[1], exp(p[2]), log = TRUE))
  oracle <- optim(c(median(x), log(stats::mad(x))), objective,
    method = "Nelder-Mead", control = list(reltol = 1e-14, maxit = 10000))
  expected <- c(oracle$par[1], exp(oracle$par[2]))
  fit <- mlgumbel(x)
  expect_true(all(is.finite(fit)))
  expect_true(all(abs(as.numeric(fit) - expected) <= 1e-4 * abs(expected)))
  expect_gte(attr(fit, "logLik"), -oracle$value - 1e-6)
})

test_that("constant data have no attained gumbel MLE", {
  expect_error(mlgumbel(rep(2, 5)), "does not exist")
  expect_error(mlgumbel(2), "does not exist")
})

test_that("Gumbel variance survives a large offset", {
  x <- c(-2, -1, 0, 1, 3)
  f <- mlgumbel(x); g <- mlgumbel(x + 1e8)
  expect_equal(g[["sigma"]], f[["sigma"]], tolerance = 1e-6)
  expect_equal(attr(g, "logLik"), attr(f, "logLik"), tolerance = 1e-6)
})

test_that("Gumbel fitting respects extreme scale changes", {
  x <- c(-2, -1, 0, 1, 3)
  f <- mlgumbel(x)
  for (s in c(1e-300, 1e300)) {
    g <- mlgumbel(x*s)
    expect_equal(as.numeric(g)/s, as.numeric(f), tolerance=1e-4)
    expect_equal(attr(g,"logLik")+length(x)*log(s), attr(f,"logLik"), tolerance=1e-7)
  }
})
