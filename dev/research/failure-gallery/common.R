# Shared setup for the failure-gallery figure scripts. Run from the
# worktree root, e.g.:
#   /tmp/umvl-cpu/run Rscript dev/research/failure-gallery/fig01-dnormgamma.R
#
# Loads the built package without recompiling (just test must have been run
# once beforehand) and sets a plain, print-friendly PDF device default.

suppressMessages(suppressWarnings({
  ok <- requireNamespace("pkgload", quietly = TRUE)
  if (!ok) stop("pkgload is required; install.packages('pkgload')")
  pkgload::load_all("univariateML-r", compile = FALSE, quiet = TRUE)
}))

fig_dir <- "dev/research/failure-gallery"

open_pdf <- function(name, width = 6, height = 4) {
  pdf(file.path(fig_dir, name), width = width, height = height, pointsize = 10)
}

# A small helper: try() an ml* call and report either the fitted
# "univariateML" object or the error message, as out$value.
try_report <- function(expr) {
  out <- tryCatch(list(ok = TRUE, value = expr),
                   error = function(e) list(ok = FALSE, value = conditionMessage(e)))
  out
}

# ml* returns an S3 "univariateML" object (a coefficient vector with
# attributes), not a list: use coef()/logLik(), not $estimates/$logLik.
ll <- function(fit) as.numeric(logLik(fit))
cf <- function(fit, name) unname(coef(fit)[name])
