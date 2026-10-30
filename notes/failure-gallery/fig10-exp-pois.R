# Entry 10: escapes to a smaller-dimensional exponential family.
# (a) Lomax -> exponential (kappa -> infinity, lambda*kappa fixed), exact
#     profile gap from ml-behaviour.tex eq. (lomax-profile) and Theorem 1.
# (b) negative binomial -> Poisson (size -> infinity, mean fixed).
source("notes/failure-gallery/common.R")

## (a) Lomax ---------------------------------------------------------------
lomax_gap <- function(x, q_grid) {
  xbar <- mean(x); y <- x / xbar
  sapply(q_grid, function(q) {
    A <- mean(log1p(q * y))
    length(x) * (log(q) - log(A) - A)
  })
}
x_exist <- c(1, 100)   # c = 0.9802, has an interior MLE (report table)
x_none <- c(1, 10)    # c = 0.8182, no MLE (report table)
q_grid <- 10^seq(-3, 2.1, length.out = 300)
gap_exist <- lomax_gap(x_exist, q_grid)
gap_none <- lomax_gap(x_none, q_grid)
max_gap_exist <- optimize(function(lq) lomax_gap(x_exist, exp(lq)), c(-3, 6), maximum = TRUE)$objective
cat(sprintf("max gap for (1,100): %.4f (report: 0.5901)\n", max_gap_exist))
cat(sprintf("max gap for (1,10): %.6f (report: no MLE, gap <= 0)\n", max(gap_none)))

r_exist <- try_report(mllomax(x_exist))
r_none <- try_report(mllomax(x_none))
cat("mllomax(c(1,100)): ", if (r_exist$ok) sprintf("kappa=%.4f lambda=%.4f logLik=%.4f",
     cf(r_exist$value, "kappa"), cf(r_exist$value, "lambda"), ll(r_exist$value)) else r_exist$value, "\n")
cat("mllomax(c(1,10)): ", if (r_none$ok) "ok" else r_none$value, "\n")

## (b) negative binomial -> Poisson ----------------------------------------
set.seed(3); xb <- rpois(100, lambda = 5)
pois_fit <- mlpois(xb)
size_grid <- c(2^seq(1, 10, by = 0.5), 10^seq(3.5, 7, by = 0.5))
nb_prof <- sapply(size_grid, function(s) {
  r <- try_report(mlnbinom(xb, size = s))
  if (r$ok) ll(r$value) else NA_real_
})
r_nb_free <- try_report(mlnbinom(xb))
cat("mlnbinom(x): ", if (!r_nb_free$ok) r_nb_free$value else sprintf(
  "size=%.3g logLik=%.4f", cf(r_nb_free$value, "size"), ll(r_nb_free$value)), "\n")
cat(sprintf("mlpois(x): lambda=%.4f logLik=%.4f\n", coef(pois_fit), as.numeric(logLik(pois_fit))))

open_pdf("fig10-exp-pois.pdf", width = 8, height = 4)
par(mfrow = c(1, 2), mar = c(4, 4, 2, 1))

plot(q_grid, gap_exist, type = "l", log = "x", lwd = 2, col = "steelblue",
     ylim = range(c(gap_exist, gap_none, 0)),
     xlab = expression(q == lambda * bar(x)), ylab = expression(ell[p] - ell[E]),
     main = "(a) Lomax profile gap")
lines(q_grid, gap_none, lwd = 2, col = "darkorange")
abline(h = 0, lty = 2)
legend("topleft", c("(1, 100): MLE exists", "(1, 10): no MLE"),
       col = c("steelblue", "darkorange"), lwd = 2, bty = "n", cex = 0.75)

plot(size_grid, nb_prof, type = "l", log = "x", lwd = 2,
     xlab = "size (held)", ylab = "logLik",
     main = "(b) NB size profile -> Poisson")
abline(h = as.numeric(logLik(pois_fit)), lty = 2, col = "firebrick")
text(min(size_grid) * 2, as.numeric(logLik(pois_fit)), "Poisson", pos = 3,
     col = "firebrick", cex = 0.8)
dev.off()
cat("wrote", file.path(fig_dir, "fig10-exp-pois.pdf"), "\n")
