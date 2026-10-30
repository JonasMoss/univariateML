# Entry 3: generalized beta of the first kind, PowerBoundary escape.
# shape1 -> 0, shape3 -> infinity with shape2 ballooning (log(shape2)/shape3
# -> c >= 0) sends Y = -log(X/scale) to c + Exponential(shape1*shape3): a
# power law on (0, scale*exp(-c)), strictly shorter than the held scale.
# Minimal reproducer: X = exp(-Y), Y ~ Gamma(shape = 2, rate = 3); this is
# not itself a genbeta member (not power, not gamma, not beta), and its
# mass concentrates well below scale = 1 (dev/research/evaluations/2026-10-29-
# genbeta-report.md escape catalogue: this generator gives interior 4,
# power 5, unit-gamma 11 of 20 replicates at n = 50).
source("dev/research/failure-gallery/common.R")
dgenbeta <- actuar::dgenbeta  # avoid attaching actuar: it masks several ml* names

x <- NULL
for (seed in 1:200) {
  set.seed(seed)
  xx <- exp(-rgamma(40, shape = 2, rate = 3))
  r <- try_report(mlgenbeta(xx, scale = 1))
  if (!r$ok && grepl("limiting power law", r$value)) { x <- xx; chosen_seed <- seed; break }
}
if (is.null(x)) stop("no PowerBoundary seed found")
cat(sprintf("n = %d, X = exp(-Gamma(2,3)), seed %d:\n", length(x), chosen_seed))
cat("  mlgenbeta(x, scale = 1): ", try_report(mlgenbeta(x, scale = 1))$value, "\n", sep = "")

pow <- mlpower(x)
cat(sprintf("Limiting power law (mlpower): alpha(endpoint) = %.4f, beta(shape) = %.4f, logLik = %.6f\n",
            coef(pow)["alpha"], coef(pow)["beta"], as.numeric(logLik(pow))))
cat(sprintf("max(x) = %.4f versus held scale = 1: the escape endpoint is strictly shorter.\n", max(x)))

neg_loglik <- function(par, beta_fixed) {
  a <- exp(par[1]); t <- exp(par[2])
  -sum(dgenbeta(x, shape1 = a, shape2 = beta_fixed, shape3 = t, scale = 1, log = TRUE))
}
profile_at <- function(beta_fixed, start = c(log(0.4), log(2))) {
  o <- optim(start, neg_loglik, beta_fixed = beta_fixed, method = "Nelder-Mead",
             control = list(maxit = 400, reltol = 1e-10))
  list(loglik = -o$value, par = exp(o$par))
}

beta_grid <- 2^seq(0, 11, by = 0.5)
prof <- numeric(length(beta_grid))
start <- c(log(0.4), log(2))
for (i in seq_along(beta_grid)) {
  r <- profile_at(beta_grid[i], start)
  prof[i] <- r$loglik
  start <- log(r$par)  # warm-start the next point
}

open_pdf("fig03-genbeta.pdf", width = 5.2, height = 4)
par(mar = c(4, 4, 1, 1))
plot(beta_grid, prof, type = "l", log = "x", lwd = 2,
     xlab = expression(shape2 ~ "(" * beta * ", held)"),
     ylab = "profile log-likelihood (shape1, shape3 optimized)")
points(beta_grid, prof, pch = 16, cex = 0.5)
abline(h = as.numeric(logLik(pow)), lty = 2, col = "firebrick")
text(min(beta_grid) * 2, as.numeric(logLik(pow)), "power law, shorter endpoint",
     pos = 3, col = "firebrick", cex = 0.8)
dev.off()
cat("wrote", file.path(fig_dir, "fig03-genbeta.pdf"), "\n")
