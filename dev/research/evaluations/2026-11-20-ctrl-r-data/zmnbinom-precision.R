pkgload::load_all("univariateML-r", compile = FALSE, quiet = TRUE)
fit <- mlnbinom(c(0L, 1L, 2L), size = 2, prob = 0.5,
                zero = "modified", tol = 1e-20)
cat("Fixed size = 2 and prob = 0.5, sample {0, 1, 2}, tol = 1e-20\n")
cat("p0 =", sprintf("%a", fit[["p0"]]), "\n")
cat("Exact maximizer = 1/3, public coefficient scale = 1\n")
cat("Error of binary64 0x1.5555555555555p-2 =", format(2^-54 / 3), "\n")
cat("Certified =", attr(fit, "certified"), "\n")
stopifnot(identical(fit[["p0"]], 1 / 3))
