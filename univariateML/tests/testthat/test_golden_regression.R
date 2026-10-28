# Regression check against tests/golden/*.json.
#
# Goldens are produced by scripts/gen_goldens.R: same fixed-seed dataset
# and same default-parameter sampler per distribution. This test re-runs
# the sample+fit pipeline and asserts that estimates and logLik reproduce
# the recorded values bit-for-bit (1e-12 tolerance for FP noise).
#
# This is the canary for the C++ refactor (de-Armadillo, file split,
# vendoring): a structural change that does not change algorithms must
# leave these untouched. To intentionally accept a new baseline, re-run
# scripts/gen_goldens.R and commit the diff.

# Goldens live at <repo_root>/quality/golden/, outside the R-package
# tarball. The dispatcher Justfile sets UMVL_GOLDEN_DIR; when run from
# inside an installed package or a tarball-based check, the env var is
# unset and this entire test file becomes a no-op (zero tests emitted),
# which is the desired CRAN behaviour.
GOLDEN_DIR <- Sys.getenv("UMVL_GOLDEN_DIR", "")

resolve_callable <- function(spec) {
  parts <- strsplit(spec, "::", fixed = TRUE)[[1]]
  if (length(parts) == 2L) {
    getExportedValue(parts[1], parts[2])
  } else {
    get(parts[1], mode = "function")
  }
}

set_golden_seed <- function(seed) {
  RNGkind("Mersenne-Twister", "Inversion", "Rejection")
  set.seed(seed)
}

# Finite-difference BFGS fitters drift by ~1e-6 across machines; see
# cppunivariate/tests/test_estimators.cpp (kFdTol).
FD_BFGS_IDS <- c("std", "ged", "sstd", "sged", "paralogis")

if (dir.exists(GOLDEN_DIR)) {
  golden_files <- list.files(GOLDEN_DIR, pattern = "\\.json$", full.names = TRUE)

  for (f in golden_files) {
    g <- jsonlite::fromJSON(f, simplifyDataFrame = FALSE)
    test_that(paste0("golden: ", g$id), {
      set_golden_seed(g$seed)
      data <- do.call(resolve_callable(g$sampler), c(list(n = g$n), g$sampler_args))

      fn <- getExportedValue("univariateML", paste0("ml", g$id))
      fit <- fn(data)

      tol <- if (g$id %in% FD_BFGS_IDS) 1e-6 else 1e-12
      expect_equal(unname(attr(fit, "logLik")), g$logLik, tolerance = tol)

      got <- as.numeric(fit)
      want <- unlist(g$estimates, use.names = FALSE)
      expect_equal(got, want, tolerance = tol)
    })
  }
}
