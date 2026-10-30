# Developer-side tests: the README/P1 loader is not shipped in the R package.
source(file.path("scripts", "load_distributions.R"))
library(testthat)

test_that("r.export validates declarative export metadata", {
  parse_export <- function(yaml_text) .umvl_export(
    yaml::yaml.load(yaml_text), c("location", "scale"), "fixture")
  expect_null(parse_export("false"))
  expect_false(is.null(parse_export("{}")))
  expect_identical(parse_export("{defaults: {location: 0, scale: 1}}")$defaults,
                   list(location=0, scale=1))
  expect_identical(parse_export("{defaults: {scale: .inf}}")$defaults$scale, Inf)
  expect_identical(parse_export("{title: Example, params: {scale: Positive}, details: Text}")$title,
                   "Example")
  expect_false(is.null(parse_export("{defaults: {}, params: {}}")))
  for (value in c("true", "null", "0", "text", "[]", "[a, b]"))
    expect_error(parse_export(value), "fixture: r.export.*mapping")
  expect_error(parse_export("{typo: 1}"), "r.export has unknown key\\(s\\): typo")
  for (key in c("defaults", "params")) {
    expect_error(parse_export(paste0("{", key, ": {unknown: 1}}")), "names non-coefficient\\(s\\): unknown")
    for (value in c("null", "1", "text", "[]", "[1, 2]"))
      expect_error(parse_export(paste0("{", key, ": ", value, "}")), paste0("r.export.", key, ".*mapping"))
  }
  for (key in c("title", "details"))
    for (value in c("null", "1", "true", "[]", "{}", "''", "'  '"))
      expect_error(parse_export(paste0("{", key, ": ", value, "}")), paste0("r.export.", key, ".*scalar text"))
  expect_error(parse_export('title: "two\\nlines"'), "title must be a single line")
  for (value in c("null", "true", "'0'", ".nan", "[0, 1]", "{}"))
    expect_error(parse_export(paste0("{defaults: {location: ", value, "}}")), "defaults.location must be a nonmissing real numeric scalar")
  for (value in c("null", "1", "true", "[]", "{}", "''", "'  '"))
    expect_error(parse_export(paste0("{params: {scale: ", value, "}}")), "params.scale must be nonempty scalar text")
})
