# Entry 7: Azzalini skew-normal. Infinite shape estimate (escape to the
# translated half-normal) and the profile-zero stationary point at alpha =
# 0 for every sample (envelope theorem on the normal profile). Frequencies
# and the bimodal-profile mechanism are in
# dev/research/evaluations/2026-10-29-sn.md: at alpha = 5, n = 20 the escape
# frequency is 77/100.
source("dev/research/failure-gallery/common.R")

rsn_direct <- function(n, xi, omega, alpha) {
  delta <- alpha / sqrt(1 + alpha^2)
  u0 <- rnorm(n); v <- rnorm(n)
  u1 <- delta * u0 + sqrt(1 - delta^2) * v
  z <- ifelse(u0 > 0, u1, -u1)
  xi + omega * z
}
dsn_direct <- function(x, xi, omega, alpha) {
  (2 / omega) * dnorm((x - xi) / omega) * pnorm(alpha * (x - xi) / omega)
}

x <- NULL
for (seed in 1:200) {
  set.seed(seed)
  xx <- rsn_direct(20, 0, 1, 5)
  r <- try_report(mlsn(xx))
  if (!r$ok && grepl("infinite", r$value)) { x <- xx; chosen_seed <- seed; break }
}
if (is.null(x)) stop("no escaping seed found")
cat(sprintf("n = %d skew-normal(alpha=5) sample, seed %d:\n", length(x), chosen_seed))
cat("  mlsn(x): ", try_report(mlsn(x))$value, "\n", sep = "")

# The alpha = 0 stationary point: the normal profile shape-score is zero
# for every nonconstant sample (envelope theorem).
rep0 <- try_report(mlsn(x, alpha = 0))
mu0 <- mean(x); s0 <- sqrt(mean((x - mu0)^2))
score_at_zero <- sqrt(2 / pi) * sum((x - mu0) / s0)
cat(sprintf("mlsn(x, alpha = 0): logLik = %.5f; shape score at alpha=0 (should be 0): %.3e\n",
            ll(rep0$value), score_at_zero))

xi_hat <- min(x)
omega_hat <- sqrt(mean((x - xi_hat)^2))
loglik_rhn <- sum(log(dsn_direct(x, xi_hat - 1e-9, omega_hat, 1e12)))
cat(sprintf("Right half-normal face: xi = %.4f, omega = %.4f, logLik = %.5f\n",
            xi_hat, omega_hat, loglik_rhn))

neg_ll <- function(par, alpha) {
  xi <- par[1]; omega <- exp(par[2])
  -sum(log(dsn_direct(x, xi, omega, alpha)))
}
profile_alpha <- function(alpha, start) {
  o <- optim(start, neg_ll, alpha = alpha, method = "Nelder-Mead",
             control = list(maxit = 500, reltol = 1e-12))
  list(loglik = -o$value, par = o$par)
}

alphas <- c(seq(0, 10, by = 0.5), seq(11, 50, by = 2), seq(55, 500, by = 20),
            10^seq(2.7, 6, by = 0.2))
prof <- numeric(length(alphas))
start <- c(mean(x), log(sd(x)))
for (i in seq_along(alphas)) {
  r <- profile_alpha(alphas[i], start)
  prof[i] <- r$loglik
  start <- r$par
}

open_pdf("fig07-sn.pdf", width = 5.2, height = 4)
par(mar = c(4, 4, 1, 1))
plot(alphas, prof, type = "l", log = "x", lwd = 2,
     xlab = expression(alpha ~ "(location, scale profiled)"),
     ylab = "profile log-likelihood")
points(alphas, prof, pch = 16, cex = 0.4)
abline(h = loglik_rhn, lty = 2, col = "firebrick")
text(2, loglik_rhn, "right half-normal face", pos = 1, col = "firebrick", cex = 0.8)
dev.off()
cat("wrote", file.path(fig_dir, "fig07-sn.pdf"), "\n")
