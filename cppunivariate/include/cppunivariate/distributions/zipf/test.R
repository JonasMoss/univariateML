context("mlzipf")

## Warning when all observations are 1 (MLE not unique)
expect_warning(
  mlzipf(c(1, 1, 1)),
  "All observations are equal to 1\\. The maximum likelihood estimator is not unique\\."
)

## qml inverts pml: F(q - 1) < p <= F(q). sads::qzipf 0.6.5 is off by one;
## see univariateML_reference_fixes and KNOWN_ISSUES.md.
test_that("qml for zipf is the discrete inverse of pml", {
  fit <- mlzipf(corbet)
  p <- c(0, 0.1, 0.4, 0.5, 0.9, 0.995, 1)
  q <- qml(p, fit)
  cdf <- c(0, pml(seq_len(fit[["N"]]), fit))
  expect_true(all(cdf[q] < p | q == 1))
  expect_true(all(p <= cdf[q + 1]))
  expect_equal(qml(log(0.4), fit, log.p = TRUE), qml(0.4, fit))
  expect_equal(qml(0.6, fit, lower.tail = FALSE), qml(0.4, fit))
  expect_true(all(is.nan(qml(c(-0.1, 1.1), fit))))
})

test_that("the qzipf replacement is the discrete inverse of the zipf CDF", {
  qfun <- univariateML:::univariateML_reference_fixes[["sads::qzipf"]]$replacement
  cdf <- cumsum((1:10)^-1) / sum((1:10)^-1)
  p <- c(0.1, 0.4, 0.5, 0.6, 0.99)
  expect_equal(qfun(p, N = 10, s = 1), vapply(p, function(pp) min(which(cdf >= pp)), 0))
})
