# Entry 8: Student t. (a) escape to the normal as nu -> infinity for data
# with sample kurtosis K <= 3 (necessary for the normal to be the global
# supremum, ml-behaviour.tex Section "std"). (b) tie-collapse with free df:
# a tie of multiplicity k with k > nu*(n-k) makes the likelihood unbounded
# as the ordinary t scale collapses, even with nu *held*; free nu needs
# only k > 2(n-k) for some held nu in (2, infinity) to hit that window.
source("notes/failure-gallery/common.R")

std_loglik_direct <- function(x, mean, sd, nu) {
  sum(lgamma((nu + 1) / 2) - lgamma(nu / 2) - 0.5 * log((nu - 2) * pi) - log(sd) -
        (nu + 1) / 2 * log1p(((x - mean) / sd)^2 / (nu - 2)))
}

## (a) normal escape -----------------------------------------------------
set.seed(1); xa <- rnorm(200)
repA <- try_report(mlstd(xa))
cat("mlstd(rnorm(200)):\n")
if (repA$ok) print(repA$value) else cat("  error: ", repA$value, "\n", sep = "")

mu_n <- mean(xa); sd_n <- sqrt(mean((xa - mu_n)^2))
loglik_norm <- sum(dnorm(xa, mu_n, sd_n, log = TRUE))

profile_nu <- function(nu, start) {
  o <- optim(start, function(p) -std_loglik_direct(xa, p[1], exp(p[2]), nu),
             method = "Nelder-Mead", control = list(reltol = 1e-12, maxit = 500))
  list(loglik = -o$value, par = o$par)
}
nus <- c(seq(2.01, 20, by = 0.5), seq(21, 200, by = 5), 10^seq(2.3, 6, by = 0.2))
profA <- numeric(length(nus)); start <- c(mu_n, log(sd_n))
for (i in seq_along(nus)) { r <- profile_nu(nus[i], start); profA[i] <- r$loglik; start <- r$par }

## (b) tie-collapse -------------------------------------------------------
xb <- c(0, 0, 0, 0, 10)  # k = 4 at the tie, n - k = 1; k > nu*(n-k) for nu < 4
repB3 <- try_report(mlstd(xb, nu = 3))
repB5 <- try_report(mlstd(xb, nu = 5))
cat("mlstd(c(0,0,0,0,10), nu = 3): ", if (repB3$ok) "ok" else repB3$value, "\n")
cat("mlstd(c(0,0,0,0,10), nu = 5): ", if (repB5$ok) "ok" else repB5$value, "\n")

sds <- 10^seq(1, -6, length.out = 200)
curveB3 <- sapply(sds, function(s) std_loglik_direct(xb, 0, s, 3))
curveB5 <- sapply(sds, function(s) std_loglik_direct(xb, 0, s, 5))

open_pdf("fig08-std.pdf", width = 8, height = 4)
par(mfrow = c(1, 2), mar = c(4, 4, 2, 1))
plot(nus, profA, type = "l", log = "x", lwd = 2,
     xlab = expression(nu), ylab = "profile log-likelihood", main = "(a) escape to normal")
abline(h = loglik_norm, lty = 2, col = "firebrick")
text(5, loglik_norm, "normal", pos = 1, col = "firebrick", cex = 0.8)

plot(sds, curveB3, type = "l", log = "x", lwd = 2, col = "steelblue",
     ylim = range(c(curveB3, curveB5)),
     xlab = "sd (location held at the tie, 0)", ylab = "log-likelihood",
     main = "(b) tie-collapse, k=4, n-k=1")
lines(sds, curveB5, lwd = 2, col = "darkorange")
legend("topright", c("nu = 3 (k > nu(n-k): unbounded)", "nu = 5 (k < nu(n-k): bounded)"),
       col = c("steelblue", "darkorange"), lwd = 2, bty = "n", cex = 0.7)
dev.off()
cat("wrote", file.path(fig_dir, "fig08-std.pdf"), "\n")
