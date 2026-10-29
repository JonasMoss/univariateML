# Scale-equivariance probe (TASK-77): fit x * 1e-3, x, x * 1e3 for every
# continuous family without required design arguments; for a scale family
# logLik(k x) + n log(k) must not depend on k. Prints the families where it
# does. Run with an installed build: Rscript notes/evaluations/2026-10-29-scale-equivariance.R
library(univariateML)
md <- univariateML:::univariateML_metadata
ids <- sub("^ml", "", names(md))
src <- "cppunivariate/include/cppunivariate/distributions"
fa <- function(id) { y <- yaml::yaml.load(strsplit(paste(readLines(file.path(src, id, "README.md")), collapse = "\n"), "\n---")[[1]][1]); y$fit_args }
for (id in ids) {
  sup <- md[[paste0("ml", id)]]$support
  args <- fa(id)
  set.seed(7)
  x <- tryCatch(simulate_default(paste0("ml", id), 400), error = function(e) NULL)
  if (is.null(x) || all(x == round(x))) next  # skip discrete
  if (length(args)) next  # required design args (endpoints) do not scale
  fit <- function(k) tryCatch({ f <- do.call(paste0("ml", id), list(k * x)); c(attr(f, "logLik") + length(x) * log(k)) }, error = function(e) NA)
  l <- sapply(c(1e-3, 1, 1e3), fit)
  d <- max(abs(l - l[2]))
  if (!is.finite(d) || d > 1e-6) cat(sprintf("%-12s logLik(adj) at x*1e-3, x, x*1e3: %s\n", id, paste(format(l, digits = 10), collapse = "  ")))
}
cat("done\n")
