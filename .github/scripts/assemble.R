# Portable equivalent of `just regen`: R file operations avoid rsync on Windows.
root <- normalizePath(".", winslash = "/", mustWork = TRUE)
pkg <- file.path(root, "univariateML-r")
Sys.setenv(UMVL_DOSSIER_DIR = file.path(root, "univariateML-cpp/include/cppunivariate/distributions"),
           UMVL_PKG_DIR = pkg, UMVL_PKG_R_DIR = file.path(pkg, "R"))
setwd(file.path(root, "bindings/r"))
run_script <- function(script) {
  status <- system2(file.path(R.home("bin"), "Rscript"), shQuote(script))
  if (status != 0L) stop("generation failed: ", script)
}
run_script("scripts/assemble_package.R")
headers <- file.path(root, "univariateML-cpp/include")
files <- list.files(headers, pattern = "\\.hpp$", recursive = TRUE)
for (f in files) {
  dest <- file.path(pkg, "inst/include", f)
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
  stopifnot(file.copy(file.path(headers, f), dest, overwrite = TRUE))
}
Rcpp::compileAttributes(pkg)
for (s in c("metadata", "wrappers", "links")) {
  run_script(paste0("scripts/codegen_", s, ".R"))
}
options(pkg.build_extra_flags = FALSE)
roxygen2::roxygenise(pkg)
