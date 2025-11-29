

c <- 2
x <- rburr(1000, 7, c, 3)
mean(x^(c*2)) / mean(x^c)





mean(x^(c*2)) / mean(x^c)





# install.packages(c("Rcpp","microbenchmark","ggplot2","data.table"))
library(Rcpp)
library(microbenchmark)
library(ggplot2)
library(data.table)

# ---------------------------------------------------------

# 2) Parameter grid: 3 mu × 2 sigma × 4 sample sizes = 24 combos
grid <- CJ(
  mu    = 0,
  sigma = c(0.1, 0.5, 1, 2, 10),
  n     = c(50, 1e2, 500, 1e3, 1e4, 1e5)
)

# inverse‐transform sampler for Gumbel(mu, sigma)
rgumbel <- function(n, mu, sigma) {
  u <- runif(n)
  mu - sigma * log(-log(u))
}

# 3) Run benchmarks
res_list <- vector("list", nrow(grid))
set.seed(42)
cat("Running benchmarks...\n")
for (i in seq_len(nrow(grid))) {
  mu0    <- grid$mu[i]
  sig0   <- grid$sigma[i]
  n0     <- grid$n[i]
  cat(sprintf("  Running: mu=%.1f, sigma=%.1f, n=%d ...\n", mu0, sig0, n0))
  x      <- rgumbel(n0, mu0, sig0)

  # Check for non-finite data
  if(any(!is.finite(x))) {
    cat("    Skipped due to non-finite data generated.\n")
    next # Skip this iteration if data is bad
  }

  mb <- microbenchmark(
    newton = ml_gumbel_newt_rcpp(x),
    halley = ml_gumbel_halley_rcpp(x),
    times = 20L,
    unit = "us" # Use microseconds for potentially faster runs
  )

  # Convert summary data.frame to data.table THEN select columns
  dt <- as.data.table(summary(mb))[, .(expr, median)]
  # Add grid parameters using data.table syntax
  dt[, `:=`(mu = mu0, sigma = sig0, n = n0)]
  res_list[[i]] <- dt
}
cat("Benchmarking complete.\n")

# 4) Combine and visualize
# Filter out NULL elements in case some iterations were skipped
results <- rbindlist(res_list[!sapply(res_list, is.null)])

if (nrow(results) > 0) {
  print(results)

  # Convert median time (microseconds) to milliseconds for plot axis
  results[, median_ms := median / 1000]

  p <- ggplot(results, aes(x = factor(n), y = median_ms, fill = expr)) +
    geom_bar(stat="identity", position="dodge") +
    facet_grid(mu ~ sigma, labeller = label_both) +
    scale_y_continuous("Median time (ms)") +
    scale_x_discrete(labels = scales::label_number()) + # Nicer labels for n
    labs(
      title = "Newton vs Halley Gumbel Fit: Median Runtime by μ, σ, n",
      x = "Sample size (n)",
      fill = "Method"
    ) +
    theme_minimal(base_size = 12) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Improve x-axis label readability

  print(p)

} else {
  cat("No valid benchmark results collected.\n")
}