# Regression check against per-distribution golden.json fixtures.
#
# Goldens are produced by scripts/goldens.R: same fixed-seed dataset
# and same default-parameter sampler per distribution. This test re-runs
# the fit on the stored dataset and compares estimates and logLik with
# the recorded values using the tolerances below.
#
# Samples and fit arguments are frozen. Estimates/logLik are independent
# high-precision references; refresh with just golden-reference --write.

# The fixture helper defaults to test-only copies shipped in the source package;
# UMVL_DISTRIBUTIONS_DIR can override them with canonical dossiers.
DISTRIBUTIONS_DIR <- Sys.getenv("UMVL_DISTRIBUTIONS_DIR", "")

# Measured accuracy against independent references, not drift from our output.
# See dev/research/evaluations/2026-11-09-golden-ref-port-report.md and its captures.
GOLDEN_TOLERANCE <- c(
  bbinom = 1e-15,
  beta = 5e-15,
  betapr = 1e-11,
  binom = 2e-16,
  bnbinom = 1.5e-13, # analytic Newton refinement; acc-allfree measurements
  burr = 2e-14,
  cauchy = 3e-12,
  cmb = 2e-13,
  cmp = 1e-15,
  dgamma = 5e-15, # analytic Newton refinement
  dlaplace = 2e-16,
  dnorm = 1e-12,
  dunif = 3e-16,
  dweibull = 2e-16,
  emg = 5e-14, # analytic Newton refinement
  exp = 5e-16,
  fatigue = 5e-13,
  foldnorm = 1e-14,
  fpareto = 1e-11,
  frechet = 2e-14,
  gamma = 1e-15,
  gb2 = 8e-14, # analytic Newton refinement; acc-allfree measurements
  ged = 3e-11,
  genbeta = 1e-14, # analytic Newton refinement
  gengamma = 5e-14, # 50-digit reference; worst coefficient relative error 8.5e-16
  geom = 5e-15,
  gev = 1e-13,
  gompertz = 5e-13,
  gpd = 2e-15,
  gumbel = 2e-15,
  hcauchy = 5e-15,
  hnorm = 2e-16,
  ht = 1e-14, # analytic Newton refinement
  huber = 1e-12,
  hyper = 2e-16,
  invburr = 1e-15,
  invexp = 1e-15,
  invgamma = 2e-15,
  invgauss = 2e-15,
  invparalogis = 3e-15,
  invpareto = 1e-10,
  invtrgamma = 1e-13,
  invweibull = 1e-14,
  kumar = 5e-16,
  laplace = 1e-15,
  lgamma = 2e-15,
  lgser = 5e-16,
  llogis = 2e-13,
  lnorm = 1e-14,
  logis = 5e-14,
  logitnorm = 2e-15,
  lomax = 2e-15,
  lst = 2e-13,
  naka = 2e-15,
  nbinom = 1e-10,
  nhyper = 2e-16,
  norm = 2e-14,
  nsbeta = 2e-15,
  paralogis = 2e-15,
  pareto = 5e-16,
  pareto2 = 2e-15,
  pareto3 = 1e-15, # analytic Newton refinement
  pareto4 = 5e-15,
  pig = 1e-12,
  pois = 3e-15,
  power = 2e-15,
  rayleigh = 2e-16,
  sged = 3e-14, # analytic Newton refinement; acc-allfree measurements
  sgomp = 1e-15, # analytic Newton refinement
  skellam = 5e-15, # analytic Newton refinement
  slash = 3e-13, # analytic Newton refinement; acc-allfree measurements
  sn = 2e-14, # analytic Newton refinement
  snorm = 3e-13,
  sstd = 4e-13, # analytic Newton refinement; acc-allfree measurements
  st = 1.2e-13, # analytic Newton refinement; acc-allfree measurements
  std = 3e-13,
  tbinom = 5e-15,
  tlambda = 1e-14,
  tnorm = 2e-14,
  tpois = 3e-15,
  trgamma = 3e-14,
  triang = 1e-12,
  unif = 1e-16,
  weibull = 5e-15,
  yulesimon = 5e-11,
  zeta = 1e-11,
  zib = 5e-15,
  zinb = 1e-14, # analytic Newton refinement
  zip = 5e-15,
  zipf = 1e-13,
  zmbinom = 5e-16,
  zmgeom = 2e-15,
  zmlogarithmic = 5e-17,
  zmnbinom = 1e-14, # analytic Newton refinement
  zmpois = 5e-15,
  ztbinom = 1e-15,
  ztgeom = 1e-17,
  ztnbinom = 1e-14, # analytic Newton refinement
  ztpois = 5e-16
  , bhatt = 2e-6 # Deferred to bhatt-allfree
)

# Declared default controls; zero means no exposed scalar control, not zero error.
GOLDEN_STOP_TOLERANCE <- c(
  bbinom = 1e-10,
  beta = 1.490116e-08,
  betapr = 1e-06,
  binom = 0.0001220703,
  bnbinom = 1e-10,
  burr = 1e-08,
  cauchy = 0.0001220703,
  cmb = 1e-10,
  cmp = 1e-10,
  dgamma = 1e-10,
  dlaplace = 0,
  dnorm = 1e-09,
  dunif = 0,
  dweibull = 1e-12,
  emg = 1e-10,
  exp = 0,
  fatigue = 1e-10,
  foldnorm = 1e-10,
  fpareto = 1e-07,
  frechet = 1e-10,
  gamma = 0.0001220703,
  gb2 = 1e-10,
  ged = 0.0001220703,
  genbeta = 1e-10,
  gengamma = 1e-10,
  geom = 0,
  gev = 1e-10,
  gompertz = 0.0001220703,
  gpd = 1e-12,
  gumbel = 1e-10,
  hcauchy = 0,
  hnorm = 0,
  ht = 2e-14,
  huber = 1e-10,
  hyper = 0,
  invburr = 1e-08,
  invexp = 0,
  invgamma = 0.0001220703,
  invgauss = 0,
  invparalogis = 0.0001220703,
  invpareto = 0.0001220703,
  invtrgamma = 1e-10,
  invweibull = 0.0001220703,
  kumar = 1e-10,
  laplace = 0,
  lgamma = 0.0001220703,
  lgser = 0,
  llogis = 0.0001220703,
  lnorm = 0,
  logis = 0.0001220703,
  logitnorm = 0,
  lomax = 1e-10,
  lst = 0.0001220703,
  naka = 0.0001220703,
  nbinom = 0.0001220703,
  nhyper = 0,
  norm = 0,
  nsbeta = 1e-10,
  paralogis = 0.0001220703,
  pareto = 0,
  pareto2 = 1e-10,
  pareto3 = 1e-10,
  pareto4 = 1e-10,
  pig = 1e-10,
  pois = 0,
  power = 0,
  rayleigh = 0,
  sged = 1e-10,
  sgomp = 1e-10,
  skellam = 1e-10,
  slash = 1e-10,
  sn = 1e-10,
  snorm = 0,
  sstd = 1e-10,
  st = 1e-10,
  std = 0.0001220703,
  tbinom = 0,
  tlambda = 1e-07,
  tnorm = 1e-12,
  tpois = 0,
  trgamma = 1e-10,
  triang = 1e-12,
  unif = 0,
  weibull = 1e-10,
  yulesimon = 1e-10,
  zeta = 1e-10,
  zib = 1e-10,
  zinb = 1e-10,
  zip = 0,
  zipf = 0.0001220703,
  zmbinom = 1e-10,
  zmgeom = 0,
  zmlogarithmic = 0,
  zmnbinom = 1e-10,
  zmpois = 0,
  ztbinom = 1e-10,
  ztgeom = 0,
  ztnbinom = 1e-10,
  ztpois = 0
  , bhatt = 1e-8
)
stopifnot(identical(names(GOLDEN_TOLERANCE), names(GOLDEN_STOP_TOLERANCE)))

if (nzchar(DISTRIBUTIONS_DIR) && dir.exists(DISTRIBUTIONS_DIR)) {
  golden_files <- Sys.glob(file.path(DISTRIBUTIONS_DIR, "*", "golden.json"))

  for (f in golden_files) {
    if (!length(umvl_filter_families(basename(dirname(f))))) next
    g <- jsonlite::fromJSON(f, simplifyDataFrame = FALSE)
    test_that(paste0("golden: ", g$id), {
      # Frozen datasets remain valid across sampler changes. RNG reproducibility
      # and distributional accuracy are checked in test-generated-dpqr.R.
      data <- as.numeric(unlist(g$data))

      # fit_args: required design quantities (README fit_args).
      fn <- getExportedValue("univariateML", paste0("ml", g$id))
      fit <- do.call(fn, c(list(data), golden_fit_args(g$id)))

      tol <- if (g$id %in% names(GOLDEN_TOLERANCE)) GOLDEN_TOLERANCE[[g$id]] else stop("Missing measured golden tolerance: ", g$id)
      expect_equal(unname(attr(fit, "logLik")), g$logLik, tolerance = tol)

      got <- as.numeric(fit)
      # jsonlite encodes infinite held bounds as "Inf" / "-Inf".
      want <- as.numeric(unlist(g$estimates, use.names = FALSE))
      # Componentwise relative accuracy; zero uses absolute error. Equal
      # signed infinities have zero error, other nonfinite mismatches fail.
      observed <- c(got, unname(attr(fit, "logLik")))
      reference <- c(want, g$logLik)
      drift <- rep(Inf, length(reference))
      same <- !is.na(observed) & !is.na(reference) & observed == reference
      drift[same] <- 0
      finite <- is.finite(observed) & is.finite(reference)
      drift[finite] <- abs(observed[finite] - reference[finite]) /
        ifelse(reference[finite] == 0, 1, abs(reference[finite]))
      expect_true(all(drift <= tol), info = paste("maximum relative error", max(drift)))
      report <- Sys.getenv("UMVL_GOLDEN_DRIFT_FILE")
      if (nzchar(report)) {
        write.table(data.frame(family = g$id, max_relative_difference = max(drift),
                               tolerance = tol), report, sep = ",", row.names = FALSE,
                    col.names = !file.exists(report), append = file.exists(report))
      }
      expect_equal(got, want, tolerance = tol)
    })
  }
}
