# Entry 2: Bhattacharjee's normal face. a >= 0 is a genuine boundary of the
# parameter space (not an escape outside it, per decision A2): a = 0 is the
# normal itself, attained exactly whenever the one-sided score at a = 0+ is
# non-positive, which happens for roughly half of all samples (whenever the
# sample kurtosis exceeds 3) at every n -- unlike ordinary escapes, this
# probability does not shrink with n. notes/evaluations/2026-10-29-
# slashbhatt-report.md's seeded study finds the normal face selected in
# 21/100, 40/100 and 46/100 replicates at n = 20, 100, 1000.
source("notes/failure-gallery/common.R")

kurt <- function(x) { m <- mean(x); mean((x - m)^4) / mean((x - m)^2)^2 }

x <- NULL
for (seed in 1:200) {
  set.seed(seed)
  xx <- rnorm(80)
  fit <- mlbhatt(xx)
  if (isTRUE(cf(fit, "a") == 0)) { x <- xx; chosen_seed <- seed; break }
}
if (is.null(x)) stop("no a=0 seed found")
fit0 <- mlbhatt(x)
cat(sprintf("n = %d normal sample, seed %d, sample kurtosis = %.4f (> 3)\n",
            length(x), chosen_seed, kurt(x)))
cat("mlbhatt(x): mu =", cf(fit0, "mu"), " sigma =", cf(fit0, "sigma"),
    " a =", cf(fit0, "a"), " logLik =", ll(fit0), "\n")

loglik_normal <- ll(fit0)  # a = 0 is attained, this *is* the normal MLE here

a_grid <- c(0, seq(0.02, 0.5, by = 0.02), seq(0.6, 3, by = 0.2))
prof <- sapply(a_grid, function(a0) {
  if (a0 == 0) return(loglik_normal)
  r <- try_report(mlbhatt(x, a = a0))
  if (r$ok) ll(r$value) else NA_real_  # uniform face dominates at this held a
})

open_pdf("fig02-bhatt.pdf", width = 5.2, height = 4)
par(mar = c(4, 4, 1, 1))
plot(a_grid, prof, type = "l", lwd = 2,
     xlab = "a (held, a >= 0)", ylab = "profile log-likelihood")
points(a_grid, prof, pch = 16, cex = 0.5)
points(0, loglik_normal, pch = 16, col = "firebrick", cex = 1.1)
text(0, loglik_normal, "a = 0 (normal, attained)", pos = 4, col = "firebrick", cex = 0.8)
dev.off()
cat("wrote", file.path(fig_dir, "fig02-bhatt.pdf"), "\n")
