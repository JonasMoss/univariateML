#!/usr/bin/env Rscript
# Frozen reference values for the families whose d/p/q/r the package
# defines itself, taken from the packages that used to supply them:
# fGarch (ged, sged, snorm, std, sstd), logitnorm, nakagami (naka) and
# sads (zipf). None of these is a dependency any more. This script writes
# distributions/<id>/reference.json; the tests compare the package's
# functions with the stored values (tests/testthat/helper-reference.R), so
# the reference packages are needed only to rerun it:
#
#   just reference-values
#
# Each case calls the reference `ref` and stores its result. The tests
# call the package's function `fun` with the same arguments.

suppressPackageStartupMessages(
  pkgload::load_all(Sys.getenv("UMVL_PKG_DIR", "package"), quiet = TRUE)
)
dossier_dir <- Sys.getenv(
  "UMVL_DOSSIER_DIR",
  "../cppunivariate/include/cppunivariate/distributions"
)

reference_case <- function(ref, fun, at, args, compare, tolerance) {
  parts <- strsplit(ref, "::", fixed = TRUE)[[1]]
  value <- do.call(getExportedValue(parts[1], parts[2]), c(list(at), args))
  list(ref = ref, fun = fun, at = at, args = args, value = value,
       compare = compare, tolerance = tolerance)
}

# The fGarch families: d, p, q and the log density at one parameter
# point, then d, p, q over a shape and skew grid.
fgarch_cases <- function(id, pars, grid) {
  cases <- list()
  add <- function(op, at, args, compare = "max_relative") {
    cases[[length(cases) + 1L]] <<- reference_case(
      paste0("fGarch::", op, id), paste0(op, id), at, args, compare, 1e-12
    )
  }
  for (op in c("d", "p", "q")) {
    add(op, if (op == "q") c(.001, .1, .5, .8, .999) else c(-3, -1, 0, 1, 4), pars)
  }
  add("d", c(-3, -1, 0, 1, 4), c(pars, log = TRUE), compare = "equal")
  for (k in seq_len(nrow(grid))) {
    pars <- c(list(mean = .3, sd = 1.7), as.list(grid[k, , drop = FALSE]))
    # Central probability positions keep the comparison in the reference's
    # accurate range even when the skew compresses one side of the density.
    observations <- do.call(get(paste0("q", id)), c(list(c(.1, .3, .5, .7, .9)), pars))
    for (op in c("d", "p", "q")) {
      add(op, if (op == "q") c(.01, .1, .5, .9, .99) else observations, pars)
    }
  }
  cases
}

# d and p on a grid, q on probabilities.
dpq_cases <- function(pkg, id, at, p, args, tolerance_q) {
  list(
    reference_case(paste0(pkg, "::d", id), paste0("d", id), at, args, "equal", 1e-12),
    reference_case(paste0(pkg, "::p", id), paste0("p", id), at, args, "equal", 1e-12),
    reference_case(paste0(pkg, "::q", id), paste0("q", id), p, args, "equal", tolerance_q)
  )
}

nu <- c(.5, 1, 2, 5)
xi <- c(.25, 1, 4)
nu_t <- c(2.1, 3, 10, 100)

families <- list(
  ged = list(pkg = "fGarch", cases = fgarch_cases(
    "ged", list(mean = .3, sd = 1.7, nu = 1.4), expand.grid(nu = nu))),
  sged = list(pkg = "fGarch", cases = fgarch_cases(
    "sged", list(mean = .3, sd = 1.7, nu = 1.4, xi = 1.8), expand.grid(nu = nu, xi = xi))),
  snorm = list(pkg = "fGarch", cases = fgarch_cases(
    "snorm", list(mean = .3, sd = 1.7, xi = 1.8), expand.grid(xi = xi))),
  std = list(pkg = "fGarch", cases = fgarch_cases(
    "std", list(mean = .3, sd = 1.7, nu = 5), expand.grid(nu = nu_t))),
  sstd = list(pkg = "fGarch", cases = fgarch_cases(
    "sstd", list(mean = .3, sd = 1.7, nu = 5, xi = 1.8), expand.grid(nu = nu_t, xi = xi))),
  logitnorm = list(pkg = "logitnorm", cases = dpq_cases(
    "logitnorm", "logitnorm", seq(.01, .99, length.out = 25), seq(.01, .99, .07),
    list(mu = 1, sigma = 2), 1e-10)),
  naka = list(pkg = "nakagami", cases = dpq_cases(
    "nakagami", "naka", seq(.01, 5, length.out = 25), seq(.01, .99, .07),
    list(shape = 1, scale = 2), 1e-10)),
  # sads::qzipf is off by one (KNOWN_ISSUES.md), so only d and p.
  zipf = list(pkg = "sads", cases = list(
    reference_case("sads::dzipf", "dzipf", 1:10, list(N = 10, s = 1), "equal", 1e-12),
    reference_case("sads::pzipf", "pzipf", 1:10, list(N = 10, s = 1), "equal", 1e-12)))
)

for (id in names(families)) {
  f <- families[[id]]
  record <- list(
    id = id,
    reference = paste(f$pkg, as.character(utils::packageVersion(f$pkg))),
    generated_by = "univariateML/scripts/reference_values.R",
    cases = f$cases
  )
  jsonlite::write_json(record, file.path(dossier_dir, id, "reference.json"),
                       pretty = TRUE, auto_unbox = TRUE, digits = I(17))
  message(sprintf("[ok] %-10s %3d cases from %s", id, length(f$cases), record$reference))
}
