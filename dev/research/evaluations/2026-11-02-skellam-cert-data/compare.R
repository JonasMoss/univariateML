# Run after compare.cpp and `just test`, from the repository root.
pkgload::load_all("univariateML-r", compile=FALSE, quiet=TRUE)
d <- read.csv("dev/research/evaluations/2026-11-02-skellam-cert-data/samples.csv")
groups <- split(d, d$sample)
stopifnot(length(groups) == 600L)
max_gap <- 0
for (g in groups) {
  fit <- mlskellam(g$x)
  gap <- abs(as.numeric(logLik(fit)) - g$old_logLik[1])
  max_gap <- max(max_gap, gap)
  stopifnot(gap <= 1e-8 * length(g$x),
            abs(unname(fit[1]) - g$old_mu1[1]) <= 1e-4 * (1 + g$old_mu1[1]),
            abs(unname(fit[2]) - g$old_mu2[1]) <= 1e-4 * (1 + g$old_mu2[1]))
}
cat("600 R fits agree with the original C++ fitter; maximum log-likelihood difference:", max_gap, "\n")
