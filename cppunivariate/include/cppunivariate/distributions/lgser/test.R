context("mllgser")

## Finds error with only 1s.
expect_error(
  mllgser(c(1, 1, 1)),
  "All observations equals 1; the maximum likelihood estimator does not exist\\."
)

test_that("theta stays in (0, 1) when nearly every count is one", {
  # The Lambert-W closed form lost all precision near mean(x) = 1 and
  # returned theta = -1.1e-16 for c(1, 1, 1, 2) and theta > 1 for
  # c(rep(1, 9), 2). The score equation mean(x) = -theta/((1-theta)log(1-theta))
  # must hold at the returned theta.
  for (x in list(c(1, 1, 1, 2), c(rep(1, 9), 2), c(rep(1, 1e5), 2), c(1, 2), c(1, 1e6))) {
    fit <- mllgser(x)
    theta <- fit[["theta"]]
    expect_gt(theta, 0)
    expect_lt(theta, 1)
    expect_equal(-theta / ((1 - theta) * log1p(-theta)), mean(x), tolerance = 1e-8)
    expect_equal(attr(fit, "logLik"), sum(extraDistr::dlgser(x, theta, log = TRUE)), tolerance = 1e-10)
  }
})
