# Replay the C++ comparison samples through the R binding.
# Run from the repository root after `just regen`.
pkgload::load_all("univariateML-r", quiet = TRUE)
d <- "dev/research/evaluations/2026-11-02-dgamma-cert-data"
reference <- read.csv(file.path(d, "comparison.csv"), nrows = 600)
inputs <- readLines(file.path(d, "samples.csv"))
stopifnot(nrow(reference) == 600L, length(inputs) == 600L)
losses <- numeric()
for (i in seq_len(600L)) {
  z <- as.numeric(strsplit(inputs[[i]], ",", fixed = TRUE)[[1L]])
  stopifnot(z[[1L]] == i - 1L)
  x <- z[-1L]
  fit <- tryCatch(mldgamma(x), error = identity)
  status <- reference$old_status[[i]]
  if (status == 0L) {
    if (inherits(fit, "error")) stop("case ", i - 1L, ": ", conditionMessage(fit))
    delta <- as.numeric(logLik(fit)) - reference$old_ll[[i]]
    if (!is.finite(delta) || abs(delta) > 1e-8 * length(x))
      stop("case ", i - 1L, ": likelihood difference ", delta)
    losses <- c(losses, delta)
  } else {
    if (!inherits(fit, "error")) stop("case ", i - 1L, ": expected a boundary error")
    pattern <- if (status == 3L) "point mass" else if (status == 4L) "mlbinom" else stop("unexpected baseline status")
    if (!grepl(pattern, conditionMessage(fit))) stop("case ", i - 1L, ": ", conditionMessage(fit))
  }
  if (i %% 100L == 0L) { cat("checked", i, "\n"); flush.console() }
}
cat("R replay passed:", length(inputs), "cases;", length(losses), "finite fits; difference range", range(losses), "\n")
