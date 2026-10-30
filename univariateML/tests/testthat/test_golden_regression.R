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

# Goldens live in distributions/<id>/golden.json, outside the R-package
# tarball. The dispatcher Justfile sets UMVL_DISTRIBUTIONS_DIR; when run from
# inside an installed package or a tarball-based check, the env var is
# unset and this entire test file becomes a no-op (zero tests emitted),
# which is the desired CRAN behaviour.
# Goldens live in distributions/<id>/golden.json, outside the package.
DISTRIBUTIONS_DIR <- Sys.getenv("UMVL_DISTRIBUTIONS_DIR", "")

resolve_callable <- function(spec) {
  parts <- strsplit(spec, "::", fixed = TRUE)[[1]]
  if (length(parts) == 2L) {
    getExportedValue(parts[1], parts[2])
  } else {
    get(parts[1], mode = "function")
  }
}

sampler_is_dependency <- function(spec) {
  pkg <- strsplit(spec, "::", fixed = TRUE)[[1]]
  length(pkg) == 1L || pkg[1] %in% c("stats", "extraDistr", "actuar")
}

set_golden_seed <- function(seed) {
  RNGkind("Mersenne-Twister", "Inversion", "Rejection")
  set.seed(seed)
}

# Golden comparisons cannot be tighter than the fitter's own declared
# convergence tolerance (the `gradtol`/`reltol`/`abstol` default in its
# README `r.control`, i.e. the `tol` it was actually given below). Once a
# multi-D optimiser is stopped by a gradient- or step-norm criterion, its
# estimate is only guaranteed to that criterion's resolution; a different
# platform (compiler, libm, Boost.Math minor version) can take a different
# BFGS/Newton trajectory -- different floating-point summation order in a
# finite-difference gradient, a different number of line-search or
# bisection steps -- and land anywhere within that resolution, concentrated
# in whichever coordinate is least curved (e.g. a Student-t `nu` with a
# near-flat profile likelihood, see cppunivariate's std/test.cpp). Comparing
# any tighter chases that platform noise, not a real regression. Exact
# closed-form fits and well-conditioned 1-D Newton solves on an explicit
# score equation are not in this table and keep the 1e-12 floor below --
# see cppunivariate/tests/test_support.hpp's kTol/kFdTol comment for the
# same rule applied to the C++ golden checks.
GOLDEN_TOLERANCE <- c(
  # Finite-difference BFGS on (mu, log sigma, log(nu - 2)[, log xi]);
  # shared default gradtol (cppunivariate/tests/test_support.hpp kFitTol).
  # invparalogis is paralogis_mle run on 1/x, so it shares the same fitter
  # and default.
  std = 1.220703e-4, ged = 1.220703e-4, sstd = 1.220703e-4,
  sged = 1.220703e-4, paralogis = 1.220703e-4, lst = 1.220703e-4,
  invparalogis = 1.220703e-4,
  # Finite-difference BFGS on (shape1, shape2, scale); own default gradtol
  # (distributions/<id>/adapter.cpp).
  burr = 1e-8, invburr = 1e-8,
  # Hand-rolled finite-difference-gradient BFGS on log(size, alpha, beta)
  # over the per-observation-averaged likelihood; own default gradtol
  # (distributions/bnbinom/adapter.cpp).
  bnbinom = 1e-8
)

if (nzchar(DISTRIBUTIONS_DIR) && dir.exists(DISTRIBUTIONS_DIR)) {
  golden_files <- Sys.glob(file.path(DISTRIBUTIONS_DIR, "*", "golden.json"))

  for (f in golden_files) {
    g <- jsonlite::fromJSON(f, simplifyDataFrame = FALSE)
    test_that(paste0("golden: ", g$id), {
      set_golden_seed(g$seed)
      # Samplers from packages that are not dependencies (fGarch, logitnorm,
      # nakagami, sads: the former references) are not rerun; the stored
      # data are used. Sample size by position: stats::rhyper calls it `nn`.
      # jsonlite reads infinite sampler arguments as strings.
      data <- if (sampler_is_dependency(g$sampler))
        do.call(resolve_callable(g$sampler), c(list(g$n), lapply(g$sampler_args, as.numeric))) else
        as.numeric(unlist(g$data))

      # fit_args: required design quantities (README fit_args).
      fn <- getExportedValue("univariateML", paste0("ml", g$id))
      fit <- do.call(fn, c(list(data), golden_fit_args(g$id)))

      tol <- if (g$id %in% names(GOLDEN_TOLERANCE)) GOLDEN_TOLERANCE[[g$id]] else 1e-12
      expect_equal(unname(attr(fit, "logLik")), g$logLik, tolerance = tol)

      got <- as.numeric(fit)
      # jsonlite encodes infinite held bounds as "Inf" / "-Inf".
      want <- as.numeric(unlist(g$estimates, use.names = FALSE))
      expect_equal(got, want, tolerance = tol)
    })
  }
}
