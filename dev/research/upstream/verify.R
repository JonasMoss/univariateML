#!/usr/bin/env Rscript
# Verification script: test all extraDistr and actuar defects from KNOWN_ISSUES.md
# Usage: Rscript verify.R > verify.out 2>&1

suppressPackageStartupMessages({
  library(extraDistr)
  library(actuar)
})

cat("=== Package Versions ===\n")
cat("extraDistr:", as.character(packageVersion("extraDistr")), "\n")
cat("actuar:", as.character(packageVersion("actuar")), "\n\n")

format_result <- function(name, actual, expected_desc) {
  cat(sprintf("%-60s", name), " | ")
  if (is.na(actual) || is.null(actual)) {
    cat("ACTUAL: NA", " | ", expected_desc, "\n")
  } else if (length(actual) > 1) {
    cat("ACTUAL:", paste(formatC(actual, digits = 6), collapse = ", "), " | ", expected_desc, "\n")
  } else {
    cat("ACTUAL:", formatC(actual, digits = 10), " | ", expected_desc, "\n")
  }
}

cat("=== EXTRADIST TESTS ===\n\n")

cat("--- Zero-inflated/truncated distributions ---\n")

# dzinb(log = TRUE) at large counts
test_name <- "dzinb(10000, 2, 0.5, 0.1, log = TRUE)"
actual <- tryCatch(extraDistr::dzinb(10000, 2, 0.5, 0.1, log = TRUE), error = function(e) NA)
format_result(test_name, actual, "should be -6923.75")

# pzinb upper-tail log
test_name <- "pzinb(100, 2, 0.5, 0.1, lower.tail = FALSE, log.p = TRUE)"
actual <- tryCatch(extraDistr::pzinb(100, 2, 0.5, 0.1, lower.tail = FALSE, log.p = TRUE), error = function(e) NA)
format_result(test_name, actual, "should be about -66.17")

# qzinb at pi=1
test_name <- "qzinb(1, 2, 0.5, 1)"
actual <- tryCatch(extraDistr::qzinb(1, 2, 0.5, 1), error = function(e) NA)
format_result(test_name, actual, "should be 0")

# dzib log
test_name <- "dzib(9999, 10000, 0.4, 0.3, log = TRUE)"
actual <- tryCatch(extraDistr::dzib(9999, 10000, 0.4, 0.3, log = TRUE), error = function(e) NA)
format_result(test_name, actual, "should be finite (like dzinb)")

# pzib upper-tail log
test_name <- "pzib(100, 10, 0.4, 0.3, lower.tail = FALSE, log.p = TRUE)"
actual <- tryCatch(extraDistr::pzib(100, 10, 0.4, 0.3, lower.tail = FALSE, log.p = TRUE), error = function(e) NA)
format_result(test_name, actual, "should be finite")

# qzib at pi=1
test_name <- "qzib(1, 10, 0.4, 1)"
actual <- tryCatch(extraDistr::qzib(1, 10, 0.4, 1), error = function(e) NA)
format_result(test_name, actual, "should be 0")

cat("\n--- Generalized Pareto ---\n")

# qgpd shape near zero
test_name <- "qgpd(0.5, xi = 1e-16)"
actual <- tryCatch(extraDistr::qgpd(0.5, xi = 1e-16), error = function(e) NA)
format_result(test_name, actual, sprintf("should be log(2) = %.10f", log(2)))

# pgpd underflow
test_name <- "pgpd(1e-20)"
actual <- tryCatch(extraDistr::pgpd(1e-20), error = function(e) NA)
format_result(test_name, actual, "should be about 1e-20")

# pgpd upper-tail log
test_name <- "pgpd(1000, xi = 0, lower.tail = FALSE, log.p = TRUE)"
actual <- tryCatch(extraDistr::pgpd(1000, xi = 0, lower.tail = FALSE, log.p = TRUE), error = function(e) NA)
format_result(test_name, actual, "should be -1000")

cat("\n--- Triangular ---\n")

# ptriang at lower endpoint with mode there
test_name <- "ptriang(0, 0, 1, 0)"
actual <- tryCatch(extraDistr::ptriang(0, 0, 1, 0), error = function(e) NA)
format_result(test_name, actual, "should be 0")

cat("\n--- Huber ---\n")

# phuber overflow
test_name <- "phuber(-60, 0, 1, 40)"
actual <- tryCatch(extraDistr::phuber(-60, 0, 1, 40), error = function(e) NA)
format_result(test_name, actual, "should be finite (not NaN from overflow)")

cat("\n--- Generalized Extreme Value (GEV) ---\n")

# qgev shape near zero
test_name <- "qgev(0.5, xi = 1e-16)"
actual <- tryCatch(extraDistr::qgev(0.5, xi = 1e-16), error = function(e) NA)
format_result(test_name, actual, "should be 0.3665")

# qgev at negative shape, quantile 1
test_name <- "qgev(1, xi = -0.5)"
actual <- tryCatch(extraDistr::qgev(1, xi = -0.5), error = function(e) NA)
format_result(test_name, actual, "should be 2")

# pgev upper-tail log
test_name <- "pgev(1000, lower.tail = FALSE, log.p = TRUE)"
actual <- tryCatch(extraDistr::pgev(1000, lower.tail = FALSE, log.p = TRUE), error = function(e) NA)
format_result(test_name, actual, "should be -1000")

cat("\n--- Truncated normal ---\n")

# dtnorm far in tail
test_name <- "dtnorm(10.05, a = 10, log = TRUE)"
actual <- tryCatch(extraDistr::dtnorm(10.05, a = 10, log = TRUE), error = function(e) NA)
format_result(test_name, actual, "should be about 1.811")

# qtnorm far in tail
test_name <- "qtnorm(0.5, a = 40, b = 41)"
actual <- tryCatch(extraDistr::qtnorm(0.5, a = 40, b = 41), error = function(e) NA)
format_result(test_name, actual, "should be finite (not Inf)")

cat("\n--- Beta-Binomial ---\n")

# dbnbinom log accuracy
test_name <- "dbnbinom(0, 2, 4e14, 1e14, log = TRUE)"
actual <- tryCatch(extraDistr::dbnbinom(0, 2, 4e14, 1e14, log = TRUE), error = function(e) NA)
expected_ref <- dnbinom(0, 2, 0.8, log = TRUE)
format_result(test_name, actual, sprintf("should be about %.3f like dnbinom", expected_ref))

# pbnbinom exceeds one
test_name <- "pbnbinom(0, 2, 4e14, 1e14)"
actual <- tryCatch(extraDistr::pbnbinom(0, 2, 4e14, 1e14), error = function(e) NA)
format_result(test_name, actual, "should be <= 1 (not 2.80)")

cat("\n--- Beta-Binomial (second) ---\n")

# dbbinom log accuracy
test_name <- "dbbinom(5, 10, 1e12, 1e12, log = TRUE)"
actual <- tryCatch(extraDistr::dbbinom(5, 10, 1e12, 1e12, log = TRUE), error = function(e) NA)
format_result(test_name, actual, "should be about -1.40204")

cat("\n--- Half-Cauchy, Half-t, Half-Normal ---\n")

# dhcauchy log overflow
test_name <- "dhcauchy(1e200, log = TRUE)"
actual <- tryCatch(extraDistr::dhcauchy(1e200, log = TRUE), error = function(e) NA)
format_result(test_name, actual, "should be about -921.486")

# dht log overflow
test_name <- "dht(1e200, 3, log = TRUE)"
actual <- tryCatch(extraDistr::dht(1e200, 3, log = TRUE), error = function(e) NA)
format_result(test_name, actual, "should be finite (not -Inf from overflow)")

# phcauchy upper-tail log
test_name <- "phcauchy(1e20, lower.tail = FALSE, log.p = TRUE)"
actual <- tryCatch(extraDistr::phcauchy(1e20, lower.tail = FALSE, log.p = TRUE), error = function(e) NA)
format_result(test_name, actual, "should be about -46.503")

# pht upper-tail log
test_name <- "pht(1e20, 3, lower.tail = FALSE, log.p = TRUE)"
actual <- tryCatch(extraDistr::pht(1e20, 3, lower.tail = FALSE, log.p = TRUE), error = function(e) NA)
format_result(test_name, actual, "should be negative (not -Inf)")

# phnorm upper-tail log
test_name <- "phnorm(1e20, lower.tail = FALSE, log.p = TRUE)"
actual <- tryCatch(extraDistr::phnorm(1e20, lower.tail = FALSE, log.p = TRUE), error = function(e) NA)
format_result(test_name, actual, "should be negative (not -Inf)")

# qhcauchy extreme upper probability
test_name <- "qhcauchy(-100, lower.tail = FALSE, log.p = TRUE)"
actual <- tryCatch(extraDistr::qhcauchy(-100, lower.tail = FALSE, log.p = TRUE), error = function(e) NA)
format_result(test_name, actual, "should be finite (not Inf)")

# qht extreme upper probability
test_name <- "qht(-100, 3, lower.tail = FALSE, log.p = TRUE)"
actual <- tryCatch(extraDistr::qht(-100, 3, lower.tail = FALSE, log.p = TRUE), error = function(e) NA)
format_result(test_name, actual, "should be about 3.90e14")

# qhnorm extreme upper probability
test_name <- "qhnorm(-100, lower.tail = FALSE, log.p = TRUE)"
actual <- tryCatch(extraDistr::qhnorm(-100, lower.tail = FALSE, log.p = TRUE), error = function(e) NA)
format_result(test_name, actual, "should be finite (not Inf)")

cat("\n--- Truncated Poisson and Binomial ---\n")

# dtpois far in tail
test_name <- "dtpois(101, 2, 100, 110, log = TRUE)"
actual <- tryCatch(extraDistr::dtpois(101, 2, 100, 110, log = TRUE), error = function(e) NA)
format_result(test_name, actual, "should be about -0.0198")

# qtbinom under severe truncation
test_name <- "qtbinom(0.5, 100, 0.1, 90, 100)"
actual <- tryCatch(extraDistr::qtbinom(0.5, 100, 0.1, 90, 100), error = function(e) NA)
format_result(test_name, actual, "should be 91 (not 100)")

cat("\n--- Discrete Laplace ---\n")

# pdlaplace branch error
test_name <- "pdlaplace(3, 5, 0.5)"
actual <- tryCatch(extraDistr::pdlaplace(3, 5, 0.5), error = function(e) NA)
format_result(test_name, actual, "should be 1/6 (not -1/3)")

cat("\n--- Negative Hypergeometric ---\n")

# dnhyper log underflow
test_name <- "dnhyper(1003, 1000, 1000, 3, log = TRUE)"
actual <- tryCatch(extraDistr::dnhyper(1003, 1000, 1000, 3, log = TRUE), error = function(e) NA)
format_result(test_name, actual, "should be about -1369.14")

# dnhyper zero urn (crash) - SKIPPED as it causes segfault
test_name <- "dnhyper(0, 0, 0, 0) [segfault check - SKIPPED]"
cat(sprintf("%-60s | ACTUAL: SKIPPED (known segfault)", test_name), " | should not segfault\n")

cat("\n--- Reflected/Discrete Distributions ---\n")

# ddnorm log
test_name <- "ddnorm(-40, log = TRUE)"
actual <- tryCatch(extraDistr::ddnorm(-40, log = TRUE), error = function(e) NA)
format_result(test_name, actual, "should be finite (not -Inf)")

# ddgamma log
test_name <- "ddgamma(10, log = TRUE)"
actual <- tryCatch(extraDistr::ddgamma(10, log = TRUE), error = function(e) NA)
format_result(test_name, actual, "should be finite (not -Inf)")

cat("\n--- Skellam ---\n")

# dskellam log
test_name <- "dskellam(0, 1000, 1000, log = TRUE)"
actual <- tryCatch(extraDistr::dskellam(0, 1000, 1000, log = TRUE), error = function(e) NA)
format_result(test_name, actual, "should be about -4.71933 (not NaN)")

cat("\n--- Frechet ---\n")

# qfrechet upper log tail
test_name <- "qfrechet(-40, 2, lower.tail = FALSE, log.p = TRUE)"
actual <- tryCatch(extraDistr::qfrechet(-40, 2, lower.tail = FALSE, log.p = TRUE), error = function(e) NA)
format_result(test_name, actual, "should be about 4.85e8 (not Inf)")

# pfrechet upper log tail
test_name <- "pfrechet(1e20, 2, lower.tail = FALSE, log.p = TRUE)"
actual <- tryCatch(extraDistr::pfrechet(1e20, 2, lower.tail = FALSE, log.p = TRUE), error = function(e) NA)
format_result(test_name, actual, "should be negative (not -Inf)")

cat("\n--- Slash ---\n")

# pslash slope error
test_name <- "pslash(1e-10) - 0.5"
actual <- tryCatch(extraDistr::pslash(1e-10) - 0.5, error = function(e) NA)
expected_val <- 1.99e-11
format_result(test_name, actual, sprintf("should be about %.2e", expected_val))

cat("\n--- Bhattacharjee ---\n")

# dbhatt log
test_name <- "dbhatt(10, log = TRUE)"
actual <- tryCatch(extraDistr::dbhatt(10, log = TRUE), error = function(e) NA)
format_result(test_name, actual, "should be about -44.3213 (not -Inf)")

cat("\n--- Discrete Weibull ---\n")

# pdweibull flooring
test_name <- "pdweibull(0.5, 0.7, 1)"
actual <- tryCatch(extraDistr::pdweibull(0.5, 0.7, 1), error = function(e) NA)
expected_floor <- extraDistr::pdweibull(0, 0.7, 1)
format_result(test_name, actual, sprintf("should floor to pdweibull(0) = %.3f", expected_floor))

# ddweibull log tail
test_name <- "ddweibull(1000, 0.999, 2, log = TRUE)"
actual <- tryCatch(extraDistr::ddweibull(1000, 0.999, 2, log = TRUE), error = function(e) NA)
format_result(test_name, actual, "should be about -1000.645")

cat("\n\n=== ACTUAR TESTS ===\n\n")

cat("--- Zero-truncated/modified distributions ---\n")

# dztnbinom log
test_name <- "dztnbinom(10000, 2, 0.4, log = TRUE)"
actual <- tryCatch(actuar::dztnbinom(10000, 2, 0.4, log = TRUE), error = function(e) NA)
format_result(test_name, actual, "should be finite (not -Inf from underflow)")

# pztpois upper-tail log
test_name <- "pztpois(10000, 2, lower.tail = FALSE, log.p = TRUE)"
actual <- tryCatch(actuar::pztpois(10000, 2, lower.tail = FALSE, log.p = TRUE), error = function(e) NA)
format_result(test_name, actual, "should be negative (not -Inf)")

# pzmlogarithmic upper-tail log
test_name <- "pzmlogarithmic(10000, 0.4, 0.3, lower.tail = FALSE, log.p = TRUE)"
actual <- tryCatch(actuar::pzmlogarithmic(10000, 0.4, 0.3, lower.tail = FALSE, log.p = TRUE), error = function(e) NA)
format_result(test_name, actual, "should be below -9000 (not about -36.4)")

# qztbinom at 1
test_name <- "qztbinom(1, 10, 0.4)"
actual <- tryCatch(actuar::qztbinom(1, 10, 0.4), error = function(e) NA)
format_result(test_name, actual, "should be 10 (not Inf)")

# qzmbinom at 1
test_name <- "qzmbinom(1, 10, 0.4)"
actual <- tryCatch(actuar::qzmbinom(1, 10, 0.4), error = function(e) NA)
format_result(test_name, actual, "should be 10 (not Inf)")

cat("\n--- Transformed Gamma ---\n")

# dtrgamma extreme shape
test_name <- "dtrgamma(1, 1e12, 1, scale = 1e-12, log = TRUE)"
actual <- tryCatch(actuar::dtrgamma(1, 1e12, 1, scale = 1e-12, log = TRUE), error = function(e) NA)
format_result(test_name, actual, "should be 12.8966 (not 12.8984)")

# ptrgamma extreme shape
test_name <- "ptrgamma(0.9, 1e-6, 1e6)"
actual <- tryCatch(actuar::ptrgamma(0.9, 1e-6, 1e6), error = function(e) NA)
format_result(test_name, actual, "should be 0.9 (not 0)")

cat("\n--- Truncated Beta ---\n")

# ptrbeta saturation
test_name <- "ptrbeta(0.4, 0.003, 1000, 0.002)"
actual <- tryCatch(actuar::ptrbeta(0.4, 0.003, 1000, 0.002), error = function(e) NA)
format_result(test_name, actual, "should be about 0.0960 (not 0)")

# ptrbeta upper-tail log extreme
test_name <- "ptrbeta(1e300, 2, 5, 3, lower.tail = FALSE, log.p = TRUE)"
actual <- tryCatch(actuar::ptrbeta(1e300, 2, 5, 3, lower.tail = FALSE, log.p = TRUE), error = function(e) NA)
format_result(test_name, actual, "should be about -6905.96 (not -Inf)")

# qtrbeta saturation
test_name <- "qtrbeta(0.5, 0.01, 1, 1)"
actual <- tryCatch(actuar::qtrbeta(0.5, 0.01, 1, 1), error = function(e) NA)
format_result(test_name, actual, sprintf("should be expm1(log(2)/0.01) = %.1f (not Inf)", expm1(log(2)/0.01)))

# qtrbeta upper-tail saturation
test_name <- "qtrbeta(0.001, 0.06, 50, 0.04, lower.tail = FALSE)"
actual <- tryCatch(actuar::qtrbeta(0.001, 0.06, 50, 0.04, lower.tail = FALSE), error = function(e) NA)
format_result(test_name, actual, "should be about 7.4 (not Inf)")

cat("\n--- Poisson Inverse Gaussian ---\n")

# dpoisinvgauss near Poisson limit (unscaled recurrence)
test_name <- "dpoisinvgauss(2, mean = 2, shape = 1e20, log = TRUE)"
actual <- tryCatch(actuar::dpoisinvgauss(2, mean = 2, shape = 1e20, log = TRUE), error = function(e) NA)
format_result(test_name, actual, "should be about -1.306852819 (not log(2))")

# dpoisinvgauss large mean/shape underflow
test_name <- "dpoisinvgauss(1000, mean = 1000, shape = 1e12, log = TRUE)"
actual <- tryCatch(actuar::dpoisinvgauss(1000, mean = 1000, shape = 1e12, log = TRUE), error = function(e) NA)
format_result(test_name, actual, "should be about -4.372900006 (not -Inf)")

cat("\n--- Generalized Beta ---\n")

# dgenbeta log precision
test_name <- "dgenbeta(0.5, 0.0001, 2, 100, log = TRUE)"
actual <- tryCatch(actuar::dgenbeta(0.5, 0.0001, 2, 100, log = TRUE), error = function(e) NA)
format_result(test_name, actual, "should be precise at unit-gamma limit")

# pgenbeta log underflow
test_name <- "pgenbeta(1e-10, 0.0001, 2, 100, log.p = TRUE)"
actual <- tryCatch(actuar::pgenbeta(1e-10, 0.0001, 2, 100, log.p = TRUE), error = function(e) NA)
format_result(test_name, actual, "should not underflow to -Inf")

# qgenbeta saturation
test_name <- "qgenbeta(0.5, 0.0001, 2, 100)"
actual <- tryCatch(actuar::qgenbeta(0.5, 0.0001, 2, 100), error = function(e) NA)
format_result(test_name, actual, "should be about 7.81e-31 (not 8.27e-4)")

cat("\n--- Pareto Family ---\n")

# qfpareto upper log tail
test_name <- "qfpareto(-40, 0, 0.3, 0.7, 0.4, lower.tail = FALSE, log.p = TRUE)"
actual <- tryCatch(actuar::qfpareto(-40, 0, 0.3, 0.7, 0.4, lower.tail = FALSE, log.p = TRUE), error = function(e) NA)
format_result(test_name, actual, "should be about 6.89e81 (not Inf)")

cat("\n=== END OF VERIFICATION ===\n")
