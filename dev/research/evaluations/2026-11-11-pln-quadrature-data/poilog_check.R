# Independent exact-likelihood fit with poilog (Engen group), for comparison
# with gauss_hermite_and_fit.py. Run: Rscript poilog_check.R  (2026-11-11)
x <- c(0, 0, 0, 1, 1, 2, 3, 3, 5, 9, 14)
r <- poilog::poilogMLE(x, zTrunc = FALSE)
print(r$par)
cat("logLik", r$logLval, "\n")
