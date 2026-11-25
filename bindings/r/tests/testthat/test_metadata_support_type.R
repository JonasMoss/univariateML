test_that("metadata support keeps the 1.x type attribute alongside $type", {
  # svines and rvinecopulib read attr(univariateML_metadata[[f]]$support, "type"), as written
  # against the intervals::Intervals support of univariateML 1.x.
  for (name in names(univariateML_metadata)) {
    s <- univariateML_metadata[[name]]$support
    expect_true(s$type %in% c("R", "Z"), info = name)
    expect_identical(attr(s, "type"), s$type, info = name)
  }
  expect_identical(attr(univariateML_metadata$mlpois$support, "type"), "Z")
  expect_identical(attr(univariateML_metadata$mlnorm$support, "type"), "R")
})
