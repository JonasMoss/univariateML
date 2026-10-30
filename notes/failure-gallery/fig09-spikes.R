# Entry 9: free-threshold / extreme-shape spikes, three mechanisms side by
# side. Each is unbounded likelihood (not a removable escape): fixed by
# restricting the parameter space (the package's D6/A4 choices), not by a
# numerical tolerance.
source("notes/failure-gallery/common.R")

## (a) Pareto II with the threshold free ---------------------------------
# min = m - eps, scale = eps: L ~ eps^(shape*(n-k)-k); the fitter's own
# shape/scale search adapts and the logLik grows without bound as eps -> 0.
xa <- c(1, 2, 4, 7)
eps_grid <- 10^seq(-4, -11, by = -0.5)
logliks_a <- sapply(eps_grid, function(eps) {
  r <- try_report(mlpareto2(xa, min = min(xa) - eps))
  if (r$ok) ll(r$value) else NA_real_
})
cat("Pareto II, min = min(x) - eps, shape/scale free:\n")
print(data.frame(eps = eps_grid, logLik = logliks_a))
# Larger eps (0.1, 0.01, 0.001) shift the data into a regime where the
# *delegate* Lomax fit itself has no MLE (the exponential-face escape of
# Theorem 1, ml-behaviour.tex); unrelated to the threshold spike studied
# here, so the grid starts below that boundary.

## (b) GPD with xi < -1 ---------------------------------------------------
# Held mu = 0, xi = -2 (outside the fit_domain, allowed here only because
# sigma is also held -- pure evaluation). The upper endpoint is
# mu - sigma/xi = sigma/2; push it down to max(x) from above.
xb <- c(1, 2, 3, 7)
d_grid <- 10^seq(0.5, -6, by = -0.5)
logliks_b <- sapply(d_grid, function(d) {
  U <- max(xb) + d
  r <- try_report(mlgpd(xb, mu = 0, xi = -2, sigma = 2 * U))
  if (r$ok) ll(r$value) else NA_real_
})
cat("GPD, mu=0, xi=-2 held, endpoint -> max(x):\n")
print(data.frame(endpoint_minus_max = d_grid, logLik = logliks_b))

## (c) GED, shape -> 0 -----------------------------------------------------
set.seed(2); xc <- rnorm(40)
mu_c <- xc[1]
ged_profile <- function(p) {
  s <- mean(abs(xc - mu_c)^p)
  beta_hat <- (p * s)^(1 / p)  # beta_hat^p = p * s (s = mean|x-mu|^p, n cancels)
  n <- length(xc)
  n * (log(p) - log(2) - log(beta_hat) - lgamma(1 / p) - 1 / p)
}
repC_free <- try_report(mlged(xc))
repC_held <- try_report(mlged(xc, nu = 0.3))
cat("mlged(x): ", if (repC_free$ok) sprintf("nu=%.4f logLik=%.4f", cf(repC_free$value, "nu"), ll(repC_free$value)) else repC_free$value, "\n")
# A *held* nu = 0.3 is an ordinary, well-posed fit (location/scale solved
# exactly, no search): the instability is specific to the nu -> 0 *limit*
# of the free three-parameter problem, not to any fixed sub-1 shape.
cat("mlged(x, nu = 0.3): ", if (repC_held$ok)
  sprintf("ok, logLik=%.4f (held nu away from 0 is perfectly well-posed)", ll(repC_held$value)) else repC_held$value, "\n")

p_grid <- 10^seq(0.7, -2.3, by = -0.05)
curve_c <- sapply(p_grid, ged_profile)
restricted_c <- if (repC_free$ok) ll(repC_free$value) else NA

open_pdf("fig09-spikes.pdf", width = 10.5, height = 3.6)
par(mfrow = c(1, 3), mar = c(4, 4, 2, 1))

plot(eps_grid, logliks_a, type = "b", log = "x", lwd = 2, pch = 16,
     xlab = expression(epsilon == min(x) - "min"), ylab = "logLik",
     main = "(a) Pareto II, min free")

plot(d_grid, logliks_b, type = "b", log = "x", lwd = 2, pch = 16, col = "steelblue",
     xlab = "endpoint - max(x)", ylab = "logLik",
     main = expression("(b) GPD, " * xi == -2))

plot(p_grid, curve_c, type = "l", log = "x", lwd = 2, col = "darkorange",
     xlab = expression(nu ~ "(shape)"), ylab = "profile logLik",
     main = "(c) GED, shape -> 0")
if (!is.na(restricted_c)) {
  abline(h = restricted_c, lty = 2, col = "firebrick")
  text(1, restricted_c, "restricted fit (nu >= 1)", pos = 3, col = "firebrick", cex = 0.65)
}
dev.off()
cat("wrote", file.path(fig_dir, "fig09-spikes.pdf"), "\n")
