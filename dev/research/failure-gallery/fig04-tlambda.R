# Entry 4: Tukey lambda, likelihood unbounded as lambda -> infinity.
# Put the location at one observation and sigma = lambda * A, A exceeding
# every other distance to it: log L = k*lambda*log(2) - n*log(lambda) + O(1)
# (k = multiplicity there, here k = 1 suffices). Sample and check values
# from dev/research/evaluations/2026-10-29-tlambda-report.md ("Checked numerically").
source("dev/research/failure-gallery/common.R")

x <- c(-2, -1, 0, 0.3, 1, 3)
n <- length(x)
mu <- 0
A <- 4

Qinv <- function(target, sigma, lambda) {
  Q <- function(u) mu + sigma * (u^lambda - (1 - u)^lambda) / lambda
  uniroot(function(u) Q(u) - target, lower = 1e-14, upper = 1 - 1e-14,
          tol = 1e-13)$root
}

loglik_path <- function(lambda) {
  sigma <- A * lambda
  u <- sapply(x, Qinv, sigma = sigma, lambda = lambda)
  dens <- 1 / (sigma * (u^(lambda - 1) + (1 - u)^(lambda - 1)))
  sum(log(dens))
}

chk <- sapply(c(2, 40, 80), loglik_path)
cat("Check against the report (-12.4766, 2.07364, 25.7199):\n")
print(round(chk, 4))

rep1 <- try_report(mltlambda(x))
cat("mltlambda(x) (restricted to the principal branch [-1, 1]):\n")
if (rep1$ok) print(rep1$value) else cat("  error: ", rep1$value, "\n", sep = "")
restricted_loglik <- if (rep1$ok) ll(rep1$value) else NA

lambdas <- seq(1, 150, length.out = 150)
curve <- sapply(lambdas, loglik_path)

open_pdf("fig04-tlambda.pdf", width = 5.2, height = 4)
par(mar = c(4, 4, 1, 1))
plot(lambdas, curve, type = "l", lwd = 2,
     xlab = expression(lambda ~ "(location at one observation, " * sigma == 4*lambda * ")"),
     ylab = "log-likelihood")
if (!is.na(restricted_loglik)) {
  abline(h = restricted_loglik, lty = 2, col = "firebrick")
  text(20, restricted_loglik, "best fit on [-1, 1]", pos = 3, col = "firebrick", cex = 0.8)
}
dev.off()
cat("wrote", file.path(fig_dir, "fig04-tlambda.pdf"), "\n")
