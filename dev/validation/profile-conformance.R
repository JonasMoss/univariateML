# From repository root, after `just regen`, under /tmp/umvl-cpu/run:
# PKG_BUILD_EXTRA_FLAGS=false Rscript dev/validation/profile-conformance.R bare
# PKG_BUILD_EXTRA_FLAGS=false Rscript dev/validation/profile-conformance.R checked
# Debug control: run `just gen`, then in univariateML-r run
# PKG_BUILD_EXTRA_FLAGS=true Rscript -e 'options(pkg.build_extra_flags=TRUE); roxygen2::roxygenise()'
# (under run4); profile with PKG_BUILD_EXTRA_FLAGS=true. This rebuilds the DLL.
# Restore `just regen` before optimized controls; never reuse a stale DLL.
mode <- match.arg(commandArgs(trailingOnly = TRUE)[1], c("bare", "checked"))
options(pkg.build_extra_flags = identical(Sys.getenv("PKG_BUILD_EXTRA_FLAGS"), "true"))
Sys.setenv(UMVL_TEST_TIER = "full", UMVL_DISTRIBUTIONS_DIR = normalizePath(
  "univariateML-cpp/include/cppunivariate/distributions"))
pkgload::load_all("univariateML-r", quiet = TRUE, helpers = FALSE)
library(testthat)
env <- new.env(parent = as.environment("package:univariateML"))
for (file in c("helper-01-tiers.R", "helper-parameter-rules.R", "setup-legacy-edition.R"))
  sys.source(file.path("bindings/r/tests/testthat", file), env)
expressions <- parse("bindings/r/tests/testthat/test_conformance.R")
eval(expressions[[1]], env) # subset generator
sweep <- Filter(function(x) is.call(x) && identical(x[[1]], as.name("test_that")) &&
  grepl("every holdable subset", as.character(x[[2]]), fixed = TRUE), expressions)[[1]][[3]]
if (mode == "bare") for (name in c("expect_true", "expect_identical", "expect_gte"))
  env[[name]] <- function(...) invisible(NULL)
dir.create("build/scratch", recursive = TRUE, showWarnings = FALSE)
Rprof(paste0("build/scratch/conformance-", mode, ".Rprof"), interval = .01)
elapsed <- system.time(if (mode == "bare") eval(sweep, env) else
  testthat::with_reporter(testthat::SilentReporter$new(),
    testthat::test_that("conformance profile", eval(sweep, env))))
Rprof(NULL)
print(elapsed)
