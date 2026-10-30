# Entry 1: two-adjacent-integer escape of the discretized normal.
# x = seven 3s and three 4s: both mlX parameters free, no finite MLE.
# The true profile log-likelihood (location re-optimized at every sigma)
# approaches the saturated two-point bound as sigma -> 0, from below.
source("dev/research/failure-gallery/common.R")

x <- c(rep(3, 7), rep(4, 3))
n <- length(x)
k <- 7                     # multiplicity of the lower cell (3)
p <- k / n                 # lower-cell fraction = 0.7

cell_loglik <- function(mean, sd) {
  sum(log(pnorm((x + 1 - mean) / sd) - pnorm((x - mean) / sd)))
}

# Profile over mean at each sd (1-d optimize; this is the actual profile
# likelihood, not just the one-parameter path used in the existence proof).
sds <- 10^seq(0.5, -4, length.out = 200)
prof <- sapply(sds, function(s) {
  optimize(function(m) cell_loglik(m, s), interval = c(2, 5), maximum = TRUE)$objective
})

bound <- k * log(p) + (n - k) * log(1 - p)  # saturated two-point bound = -6.108643...

cat(sprintf("Two-point bound (7 threes, 3 fours): %.12f\n", bound))

rep1 <- try_report(mldnorm(x))
cat("mldnorm(c(rep(3,7), rep(4,3))):\n")
if (rep1$ok) print(rep1$value) else cat("  error: ", rep1$value, "\n", sep = "")

open_pdf("fig01-dnormgamma.pdf", width = 5.2, height = 4)
par(mar = c(4, 4, 1, 1))
plot(sds, prof, type = "l", log = "x", lwd = 2,
     xlab = expression(sigma), ylab = "profile log-likelihood",
     main = "")
abline(h = bound, lty = 2, col = "firebrick")
text(min(sds) * 1.3, bound, "two-point bound", pos = 3, col = "firebrick", cex = 0.85)
dev.off()
cat("wrote", file.path(fig_dir, "fig01-dnormgamma.pdf"), "\n")
