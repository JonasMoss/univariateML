# Entry 5: slash, the exactly-half-tied case. With multiplicity k at
# location v, free-scale collapse there has likelihood proportional to
# sigma^(n-2k); k = n/2 is the knife edge where that exponent is zero, so
# collapse neither wins outright (k > n/2) nor loses outright (k < n/2) --
# whether it is the global supremum is a variational comparison with every
# interior candidate. Sample from notes/evaluations/2026-10-29-
# slashbhatt-report.md ("exact rational arithmetic"): n = 20, 10 tied at 0,
# one at -1, nine at 9; the report proves an interior point beats the
# collapse bound by a factor > 1.456.
source("notes/failure-gallery/common.R")

x <- c(rep(0, 10), -1, rep(9, 9))
n <- length(x)
v <- 0
k <- sum(x == v)
c0 <- 1 / sqrt(2 * pi)

g <- function(z) {
  out <- numeric(length(z))
  small <- abs(z) < 1e-3
  out[small] <- (0.5 - z[small]^2 / 8 + z[small]^4 / 48) * c0
  zz <- z[!small]
  out[!small] <- (1 - exp(-zz^2 / 2)) / zz^2 * c0
  out
}
slash_loglik <- function(mu, sigma) sum(log(g((x - mu) / sigma)) - log(sigma))

logB <- n * log(c0) - (n / 2) * log(2) + sum(log(abs(x[x != v] - v)^(-2)))
cat(sprintf("Collapse bound at v = 0 (k = n/2 = %d): log B = %.6f\n", k, logB))

chk <- slash_loglik(4, 3)
cat(sprintf("L(mu=4, sigma=3) / B = %.5f (report: > 1.45605)\n", exp(chk - logB)))

rep1 <- try_report(mlslash(x))
cat("mlslash(x):\n")
if (rep1$ok) print(rep1$value) else cat("  error: ", rep1$value, "\n", sep = "")

mu_grid <- seq(-3, 11, by = 0.2)
prof <- sapply(mu_grid, function(m) {
  optimize(function(ls) slash_loglik(m, exp(ls)), interval = log(c(0.05, 20)),
           maximum = TRUE)$objective
})

open_pdf("fig05-slash.pdf", width = 5.2, height = 4)
par(mar = c(4, 4, 1, 1))
plot(mu_grid, prof, type = "l", lwd = 2,
     xlab = expression(mu ~ "(location, " * sigma ~ "profiled)"),
     ylab = "profile log-likelihood")
abline(h = logB, lty = 2, col = "firebrick")
text(-2, logB, "collapse bound at v = 0", pos = 3, col = "firebrick", cex = 0.8)
dev.off()
cat("wrote", file.path(fig_dir, "fig05-slash.pdf"), "\n")
