# Frozen values from the packages that used to supply the d/p/q/r of
# families the package now defines itself (fGarch, logitnorm, nakagami,
# sads). They live in distributions/<id>/reference.json, written by
# scripts/reference_values.R. Test-only copies ship with the source package;
# UMVL_DISTRIBUTIONS_DIR optionally selects canonical dossiers.
expect_reference_values <- function(id) {
  root <- Sys.getenv("UMVL_DISTRIBUTIONS_DIR")
  path <- file.path(root, id, "reference.json")
  testthat::skip_if_not(nzchar(root) && file.exists(path), "no reference values")
  ref <- jsonlite::fromJSON(path, simplifyDataFrame = FALSE)
  for (case in ref$cases) {
    actual <- do.call(get(case$fun), c(list(case$at), case$args))
    label <- sprintf("%s vs %s at %s", case$fun, case$ref,
                     paste(names(case$args), unlist(case$args), sep = "=", collapse = ", "))
    if (identical(case$compare, "max_relative")) {
      testthat::expect_lt(max(abs(actual / case$value - 1)), case$tolerance, label = label)
    } else {
      testthat::expect_equal(actual, case$value, tolerance = case$tolerance, label = label)
    }
  }
}
