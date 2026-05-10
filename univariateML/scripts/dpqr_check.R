#!/usr/bin/env Rscript
# scripts/dpqr_check.R
#
# Manual cross-validation of univariateML::d/p/q against reference R
# packages (extraDistr, actuar, fGarch, logitnorm, sads, statmod).
#
# Run via: just dpqr-check
#
# This is NOT part of `just test` and the reference packages are NOT in
# DESCRIPTION. We do not want runtime / Imports churn for what is a
# correctness sanity check during development. Each comparison skips
# cleanly if its reference package is unavailable.
#
# Tolerance:
#   d, p:  1e-10  (density / cdf should agree closely with any sane impl)
#   q:     1e-8   (inverse cdf compounds — anything tighter trips on
#                  reference impls that use different root-finders)

suppressPackageStartupMessages({
  devtools::load_all(Sys.getenv("UMVL_PKG_DIR", "package"), quiet = TRUE)
})

TOL_DP <- 1e-10
TOL_Q  <- 1e-8

fmt <- function(v) if (is.finite(v)) sprintf("%.3g", v) else "NA"
report <- function(label, status, diff) {
  cat(sprintf("  %-22s %-6s max|diff| = %s\n", label, status, fmt(diff)))
}

need <- function(pkg) {
  ok <- requireNamespace(pkg, quietly = TRUE)
  if (!ok) cat(sprintf("[skip] %s not installed\n", pkg))
  ok
}

check <- function(label, ours, theirs, tol) {
  d <- tryCatch(max(abs(ours - theirs), na.rm = TRUE),
                error = function(e) NaN, warning = function(w) NaN)
  ok <- is.finite(d) && d <= tol
  report(label, if (ok) "OK" else "FAIL", d)
  if (!ok) 1L else 0L
}

# Compare d/p/q for one distribution against a reference namespace.
# `their_name` defaults to the same name as ours; override for renames.
# `their_params` defaults to `params`; override when arg names diverge
# (e.g. our `dinvgamma(alpha, beta)` with beta=scale vs actuar's
# `dinvgamma(shape, rate, scale=1/rate)`).
check_dpq <- function(name, params, x, q, ref_pkg,
                      their_name = name, their_params = params) {
  if (!need(ref_pkg)) return(0L)
  ref <- asNamespace(ref_pkg)
  ours_d   <- do.call(get(paste0("d", name)),                      c(list(x), params))
  theirs_d <- do.call(get(paste0("d", their_name), envir = ref),   c(list(x), their_params))
  ours_p   <- do.call(get(paste0("p", name)),                      c(list(x), params))
  theirs_p <- do.call(get(paste0("p", their_name), envir = ref),   c(list(x), their_params))
  ours_q   <- do.call(get(paste0("q", name)),                      c(list(q), params))
  theirs_q <- do.call(get(paste0("q", their_name), envir = ref),   c(list(q), their_params))
  fails <- 0L
  fails <- fails + check(paste0("d", name), ours_d, theirs_d, TOL_DP)
  fails <- fails + check(paste0("p", name), ours_p, theirs_p, TOL_DP)
  fails <- fails + check(paste0("q", name), ours_q, theirs_q, TOL_Q)
  fails
}

fails <- 0L

cat("== univariateML dpqr cross-check ==\n")
cat("(reference packages are not in DESCRIPTION; missing ones skip)\n\n")

# --- extraDistr -----------------------------------------------------------
cat("[extraDistr]\n")
xpos <- seq(0.05, 20, length.out = 200)
qmid <- seq(0.01, 0.99, length.out = 50)

fails <- fails + check_dpq("betapr",   list(2, 3),         xpos, qmid, "extraDistr")
fails <- fails + check_dpq("dunif",    list(1, 10),        seq(1, 10, by = 1L), qmid, "extraDistr")
fails <- fails + check_dpq("fatigue",  list(0.5, 2, 0),    xpos, qmid, "extraDistr")
fails <- fails + check_dpq("gompertz", list(1, 2),         xpos, qmid, "extraDistr")
fails <- fails + check_dpq("gumbel",   list(0, 1),         seq(-5, 10, length.out = 200), qmid, "extraDistr")
fails <- fails + check_dpq("kumar",    list(2, 5),         seq(0.01, 0.99, length.out = 200), qmid, "extraDistr")
fails <- fails + check_dpq("laplace",  list(0, 1),         seq(-10, 10, length.out = 200), qmid, "extraDistr")
fails <- fails + check_dpq("lgser",    list(0.5),          1:50, qmid, "extraDistr")
fails <- fails + check_dpq("lomax",    list(2, 4),         xpos, qmid, "extraDistr")
fails <- fails + check_dpq("pareto",   list(2, 1),         xpos + 1, qmid, "extraDistr")
fails <- fails + check_dpq("power",    list(2, 3),         seq(0.01, 1.99, length.out = 200), qmid, "extraDistr")
fails <- fails + check_dpq("rayleigh", list(2),            xpos, qmid, "extraDistr")
fails <- fails + check_dpq("zip",      list(3, 0.2),       0:30, qmid, "extraDistr")

# --- actuar ---------------------------------------------------------------
cat("\n[actuar]\n")
fails <- fails + check_dpq("burr",      list(2, 3),  xpos, qmid, "actuar")
fails <- fails + check_dpq("invburr",   list(2, 3),  xpos, qmid, "actuar")
# actuar's dinvgamma takes (shape, rate, scale=1/rate); our (alpha, beta)
# treats beta as scale. Map to a common parameter set via named args.
fails <- fails + check_dpq("invgamma",  list(alpha = 2, beta = 3), xpos, qmid,
                           "actuar", their_params = list(shape = 2, scale = 3))
fails <- fails + check_dpq("invgauss", list(1, 2),   xpos, qmid, "actuar", their_name = "invgauss")
fails <- fails + check_dpq("invweibull", list(2, 3), xpos, qmid, "actuar")
fails <- fails + check_dpq("llogis",    list(2, 3),  xpos, qmid, "actuar")
# Same scale-vs-rate mismatch as invgamma.
fails <- fails + check_dpq("paralogis", list(shape = 2, scale = 3), xpos, qmid,
                           "actuar", their_params = list(shape = 2, scale = 3))

# log-gamma in actuar is `dlgamma(x, shapelog, ratelog)`; matches ours.
fails <- fails + check_dpq("lgamma",    list(2, 3),  xpos + 1, qmid, "actuar")

# --- fGarch (skew family) -------------------------------------------------
cat("\n[fGarch]\n")
xreal <- seq(-5, 5, length.out = 200)
fails <- fails + check_dpq("ged",   list(0, 1, 2),         xreal, qmid, "fGarch")
fails <- fails + check_dpq("sged",  list(0, 1, 2, 1.5),    xreal, qmid, "fGarch")
fails <- fails + check_dpq("snorm", list(0, 1, 1.5),       xreal, qmid, "fGarch")
fails <- fails + check_dpq("std",   list(0, 1, 5),         xreal, qmid, "fGarch")
fails <- fails + check_dpq("sstd",  list(0, 1, 5, 1.5),    xreal, qmid, "fGarch")

# --- logitnorm ------------------------------------------------------------
cat("\n[logitnorm]\n")
if (need("logitnorm")) {
  ln <- asNamespace("logitnorm")
  x01 <- seq(0.01, 0.99, length.out = 200)
  fails <- fails + check("dlogitnorm", dlogitnorm(x01, 0, 1),
                         ln$dlogitnorm(x01, 0, 1), TOL_DP)
  fails <- fails + check("plogitnorm", plogitnorm(x01, 0, 1),
                         ln$plogitnorm(x01, 0, 1), TOL_DP)
  fails <- fails + check("qlogitnorm", qlogitnorm(qmid, 0, 1),
                         ln$qlogitnorm(qmid, 0, 1), TOL_Q)
}

# --- summary --------------------------------------------------------------
cat("\n")
if (fails == 0L) {
  cat(sprintf("All comparisons within tol (d/p: %.0e, q: %.0e)\n", TOL_DP, TOL_Q))
  invisible(0L)
} else {
  stop(sprintf("\n%d comparisons failed.\n", fails))
}
