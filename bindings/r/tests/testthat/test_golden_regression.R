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

# Golden comparisons cannot be tighter than the fitters' declared convergence
# tolerance (the `gradtol`/`reltol`/`abstol` default in each distribution's
# README `r.control`, i.e. the `tol` it was actually given below). But
# cross-machine drift (simbox vs local, 2026-10-29) was much smaller than
# the BFGS tolerance defaults: std's estimates differed by a mean relative of
# 9.4e-6, with nu's change at 2.5e-6 relative. kFdTol = 3e-5 (~3x the largest
# observed drift) keeps golden tests as regression canaries, not noise followers.
# The fitters' declared gradtol is the upper bound; see univariateML-cpp/tests/
# test_support.hpp's kFdTol comment for the same rule applied to C++ checks.
GOLDEN_TOLERANCE <- c(
  # Finite-difference BFGS on (mu, log sigma, log(nu - 2)[, log xi]);
  # golden comparison tolerance 3e-5 (test_support.hpp kFdTol).
  # invparalogis is paralogis_mle run on 1/x, so it shares the same fitter
  # and default.
  std = 3e-5, ged = 3e-5, sstd = 3e-5,
  sged = 3e-5, paralogis = 3e-5, lst = 3e-5,
  invparalogis = 3e-5,
  # Finite-difference BFGS on (shape1, shape2, scale); own default gradtol
  # (distributions/<id>/adapter.cpp).
  burr = 1e-8, invburr = 1e-8,
  # Hand-rolled finite-difference-gradient BFGS on log(size, alpha, beta)
  # over the per-observation-averaged likelihood; own default gradtol
  # (distributions/bnbinom/adapter.cpp).
  bnbinom = 1e-8,
  # Bhattacharjee simplex: its stopping point moves by up to 1.6e-8 relative
  # where long double is double (macOS arm64); TASK-125 report.
  bhatt = 5e-8
)

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
