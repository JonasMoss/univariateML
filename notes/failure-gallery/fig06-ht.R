# Entry 6: half-t, bimodal df profile. Sample from
# cppunivariate/include/cppunivariate/distributions/ht/README.md: two local
# maxima at nu = .41508 (logLik -11.72979, the higher) and nu = 1.17074
# (logLik -11.98253); the half-normal face (nu -> infinity) is at -12.59859.
source("notes/failure-gallery/common.R")

x <- c(.12773, 12.5979, 3.91782, 2.58658, .138212)
n <- length(x)

profile_loglik <- function(nu) {
  optimize(function(sigma) sum(log(2) - log(sigma) + dt(x / sigma, df = nu, log = TRUE)),
           interval = c(1e-3, 100), maximum = TRUE)$objective
}

sigma_hn <- sqrt(mean(x^2))
loglik_hn <- sum(log(sqrt(2 / pi) / sigma_hn) - x^2 / (2 * sigma_hn^2))
cat(sprintf("Half-normal face: sigma = %.5f, logLik = %.5f (report: -12.59859)\n",
            sigma_hn, loglik_hn))

for (nu0 in c(0.41508, 1.17074)) {
  cat(sprintf("profile logLik(nu = %.5f) = %.5f\n", nu0, profile_loglik(nu0)))
}

rep1 <- try_report(mlht(x))
cat("mlht(x):\n")
if (rep1$ok) print(rep1$value) else cat("  error: ", rep1$value, "\n", sep = "")

log_nu <- seq(log(0.05), log(10), length.out = 400)
curve <- sapply(exp(log_nu), profile_loglik)

open_pdf("fig06-ht.pdf", width = 5.2, height = 4)
par(mar = c(4, 4, 1, 1))
plot(exp(log_nu), curve, type = "l", log = "x", lwd = 2,
     xlab = expression(nu ~ "(" * sigma ~ "profiled)"), ylab = "profile log-likelihood")
abline(h = loglik_hn, lty = 2, col = "firebrick")
text(0.08, loglik_hn, "half-normal face", pos = 3, col = "firebrick", cex = 0.8)
points(c(0.41508, 1.17074), c(-11.72979, -11.98253), pch = 16, col = "steelblue")
dev.off()
cat("wrote", file.path(fig_dir, "fig06-ht.pdf"), "\n")
