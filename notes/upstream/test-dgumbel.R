#!/usr/bin/env Rscript
# Test dgumbel mapping between extraDistr and actuar parameterizations

suppressPackageStartupMessages({
  library(extraDistr)
  library(actuar)
})

cat("=== Testing dgumbel parameter mapping ===\n\n")
cat("extraDistr uses: dgumbel(x, mu, sigma) [location=mu, scale=sigma]\n")
cat("actuar uses: dgumbel(x, alpha, scale) [location=alpha, scale=scale]\n\n")

# Test grid
x_values <- c(-5, -1, 0, 1, 5, 10)
mu_values <- c(0, 1, 5)
sigma_values <- c(0.5, 1, 2)

cat("Testing if extraDistr::dgumbel(x, mu, sigma) == actuar::dgumbel(x, mu, sigma)\n\n")

all_match <- TRUE

for (mu in mu_values) {
  for (sigma in sigma_values) {
    cat(sprintf("mu=%g, sigma=%g:\n", mu, sigma))
    for (x in x_values) {
      ed_val <- tryCatch(extraDistr::dgumbel(x, mu, sigma), error = function(e) NA)
      ac_val <- tryCatch(actuar::dgumbel(x, mu, sigma), error = function(e) NA)

      match <- FALSE
      if (!is.na(ed_val) && !is.na(ac_val)) {
        # Check if values are close (allowing for floating point)
        if (abs(ed_val - ac_val) < 1e-10 * max(abs(ed_val), abs(ac_val), 1)) {
          match <- TRUE
        }
      } else if (is.na(ed_val) && is.na(ac_val)) {
        match <- TRUE
      }

      status <- if(match) "✓" else "✗"
      cat(sprintf("  x=%-3g: extraDistr=%12g, actuar=%12g %s\n", x, ed_val, ac_val, status))
      if (!match) all_match <- FALSE
    }
    cat("\n")
  }
}

cat(sprintf("\nOverall result: %s\n", if(all_match) "PARAMETERS MATCH" else "PARAMETERS DIFFER OR HAVE ISSUES"))

# Also test with log=TRUE
cat("\n=== Testing with log=TRUE ===\n\n")

for (mu in mu_values[1:2]) {
  for (sigma in sigma_values[1:2]) {
    ed_log <- extraDistr::dgumbel(0, mu, sigma, log=TRUE)
    ac_log <- actuar::dgumbel(0, mu, sigma, log=TRUE)
    match <- abs(ed_log - ac_log) < 1e-10
    cat(sprintf("mu=%g, sigma=%g, x=0: extraDistr=%g, actuar=%g %s\n",
                mu, sigma, ed_log, ac_log, if(match) "✓" else "✗"))
  }
}
