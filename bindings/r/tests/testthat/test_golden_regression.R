# Regression check against per-distribution golden.json fixtures.
#
# Goldens are produced by scripts/goldens.R: same fixed-seed dataset
# and same default-parameter sampler per distribution. This test re-runs
# the fit on the stored dataset and compares estimates and logLik with
# the recorded values using the tolerances below.
#
# This is the canary for the C++ refactor (de-Armadillo, file split,
# vendoring): a structural change that does not change algorithms must
# leave these untouched. To intentionally accept a new baseline, re-run
# scripts/goldens.R and inspect the diff.

# The fixture helper defaults to test-only copies shipped in the source package;
# UMVL_DISTRIBUTIONS_DIR can override them with canonical dossiers.
DISTRIBUTIONS_DIR <- Sys.getenv("UMVL_DISTRIBUTIONS_DIR", "")

# Iterative fit coefficients can move more than the objective/gradient stop:
# its tolerance is not a bound on parameter error without curvature bounds.
# For these golden data use at least four times the largest measured Linux,
# macOS or Windows drift, rounded upward. As a guard against making these canaries too
# loose, require each allowance to be at most ten times the square root of
# its relevant stopping tolerance (the inner scale stop for ht). This is a
# practical rule, not a bound on parameter error. The FD BFGS group retains
# its earlier calibration.
# Closed-form/default families retain 1e-12. See the portability report for
# stopping rules, all three platform observations, and the limits of this empirical rule.
GOLDEN_TOLERANCE <- c(
  std = 3e-5, ged = 3e-5, sstd = 3e-5, sged = 3e-5,
  paralogis = 3e-5, invparalogis = 3e-5, lst = 3e-5,
  betapr = 3e-7, bhatt = 2e-6, bnbinom = 2e-6,
  burr = 1e-6, invburr = 5e-7, dgamma = 1e-7,
  emg = 3e-7, gev = 2e-10, gompertz = 1e-11,
  ht = 5e-7, pareto4 = 1e-7, pig = 1e-7,
  gb2 = 5e-10, invtrgamma = 2e-10, tlambda = 3e-7,
  trgamma = 2e-11, zinb = 4e-9, zmnbinom = 3e-6,
  st = 5e-7
)

# Adapter controls, or the implementation's inner stopping tolerance for ht.
GOLDEN_STOP_TOLERANCE <- c(
  std = 1.220703e-4, ged = 1.220703e-4, sstd = 1.220703e-4,
  sged = 1.220703e-4, paralogis = 1.220703e-4,
  invparalogis = 1.220703e-4, lst = 1.220703e-4,
  betapr = 1e-6, bhatt = 1e-8, bnbinom = 1e-8,
  burr = 1e-8, invburr = 1e-8, dgamma = 1e-8, emg = 1e-8,
  gev = 1e-10, gompertz = 1.220703e-4, ht = 2e-14,
  gb2 = 1e-7, invtrgamma = 1e-10, tlambda = 1e-7,
  pareto4 = 1e-10, pig = 1e-9, trgamma = 1e-10,
  zinb = 1e-10, zmnbinom = 1e-10,
  st = 1e-6
)
stopifnot(identical(sort(names(GOLDEN_TOLERANCE)), sort(names(GOLDEN_STOP_TOLERANCE))),
          all(GOLDEN_TOLERANCE <= 10 * sqrt(GOLDEN_STOP_TOLERANCE[names(GOLDEN_TOLERANCE)])))

if (nzchar(DISTRIBUTIONS_DIR) && dir.exists(DISTRIBUTIONS_DIR)) {
  golden_files <- Sys.glob(file.path(DISTRIBUTIONS_DIR, "*", "golden.json"))

  for (f in golden_files) {
    g <- jsonlite::fromJSON(f, simplifyDataFrame = FALSE)
    test_that(paste0("golden: ", g$id), {
      # Frozen datasets remain valid across sampler changes. RNG reproducibility
      # and distributional accuracy are checked in test-generated-dpqr.R.
      data <- as.numeric(unlist(g$data))

      # fit_args: required design quantities (README fit_args).
      fn <- getExportedValue("univariateML", paste0("ml", g$id))
      fit <- do.call(fn, c(list(data), golden_fit_args(g$id)))

      tol <- if (g$id %in% names(GOLDEN_TOLERANCE)) GOLDEN_TOLERANCE[[g$id]] else 1e-12
      expect_equal(unname(attr(fit, "logLik")), g$logLik, tolerance = tol)

      got <- as.numeric(fit)
      # jsonlite encodes infinite held bounds as "Inf" / "-Inf".
      want <- as.numeric(unlist(g$estimates, use.names = FALSE))
      # Opt-in CI observation only: preserve the ordinary assertions above/below.
      report <- Sys.getenv("UMVL_GOLDEN_DRIFT_FILE")
      if (nzchar(report)) {
        observed <- c(got, unname(attr(fit, "logLik")))
        reference <- c(want, g$logLik)
        # Componentwise relative error; zero uses absolute error. Equal signed
        # infinities have zero drift; all other nonfinite mismatches are Inf.
        drift <- rep(Inf, length(reference))
        same <- !is.na(observed) & !is.na(reference) & observed == reference
        drift[same] <- 0
        finite <- is.finite(observed) & is.finite(reference)
        drift[finite] <- abs(observed[finite] - reference[finite]) /
          ifelse(reference[finite] == 0, 1, abs(reference[finite]))
        write.table(data.frame(family = g$id, max_relative_difference = max(drift),
                               tolerance = tol), report, sep = ",", row.names = FALSE,
                    col.names = !file.exists(report), append = file.exists(report))
      }
      expect_equal(got, want, tolerance = tol)
    })
  }
}
