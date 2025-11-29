# benchmark_gumbel_misspecified.R

# Install required packages if needed
# install.packages(c("Rcpp", "RcppArmadillo", "microbenchmark", "dplyr", "tidyr", "data.table", "ggplot2", "scales"))

# --- Load Libraries ---
library(Rcpp)
library(RcppArmadillo)
library(microbenchmark)
library(dplyr)
library(tidyr)
library(data.table)
library(ggplot2)
library(scales) # For plot axis formatting

# --- Define Scenario Functions (Non-Gumbel) ---
# Avoid infinite variance (e.g., Cauchy, t_df<=2)
scenarios_misspecified <- list(
  # Symmetric, Lighter Tails
  Normal_Std = function(n) rnorm(n, mean = 0, sd = 1),
  Normal_ShiftScale = function(n) rnorm(n, mean = 5, sd = 3),
  # Symmetric, Heavier Tails (finite variance)
  T_Dist_3df = function(n) rt(n, df = 3), # Heavy tails
  T_Dist_10df = function(n) rt(n, df = 10), # Moderately heavy tails
  Logistic = function(n) rlogis(n, location = 0, scale = 1), # Similar to T, slightly lighter tails than low df T
  # Skewed (Right)
  LogNormal_Std = function(n) rlnorm(n, meanlog = 0, sdlog = 1),
  Gamma_Shape2 = function(n) rgamma(n, shape = 2, rate = 1),
  Weibull_Shape2 = function(n) rweibull(n, shape = 2, scale = 1), # Moderate skew
  # Skewed (Left)
  Neg_Gamma_Shape2 = function(n) -rgamma(n, shape = 2, rate = 1), # Negated Gamma
  # Mixture (potentially bimodal or skewed)
  Normal_Mixture = function(n) {
    n1 <- floor(n / 2)
    n2 <- n - n1
    c(rnorm(n1, mean = 0, sd = 1), rnorm(n2, mean = 4, sd = 1)) # Mixture of two Normals
  }
)

# --- Benchmark Setup ---
set.seed(999)
sample_sizes <- c(1e2, 1e3, 1e4, 1e5)
n_runs <- 15 # Number of benchmark runs per setting

# --- Run Benchmark ---
results_list <- list()

cat("Starting misspecified model benchmarks...\n")

for (n0 in sample_sizes) {
  cat(sprintf("\n--- Sample Size n = %d ---\n", n0))
  for (scen_name in names(scenarios_misspecified)) {
    cat(sprintf("  Scenario: %s ... ", scen_name))

    # Generate data
    x <- tryCatch(scenarios_misspecified[[scen_name]](n0), error = function(e) {
      cat(" Error generating data: ", e$message, "\n")
      return(NULL)
    })

    if(is.null(x) || any(!is.finite(x))) {
      cat("Skipped (NULL or non-finite data generated).\n")
      next
    }

    # Run microbenchmark
    mb <- NULL
    tryCatch({
      mb <- microbenchmark(
        newton = ml_gumbel_newt_rcpp(x),
        halley = ml_gumbel_halley_rcpp(x),
        times = n_runs,
        unit = "us" # Use microseconds
      )
      cat("Done.\n")
    }, error = function(e){
      cat("Failed during microbenchmark: ", e$message, "\n")
    })

    if (!is.null(mb)) {
      # --- Extract Timing Stats ---
      timing_summary <- tryCatch(summary(mb, unit = "us") %>% select(expr, mean, median),
                                 error = function(e) {
                                   warning("Failed to get timing summary: ", e$message); return(NULL) })
      timing_sd <- tryCatch({ mb %>% group_by(expr) %>% summarise(sd_time = sd(time / 1000), .groups = 'drop') }, # sd in us
                            error = function(e) {
                              warning("Failed to calculate sd_time: ", e$message); return(NULL) })

      # Basic check before proceeding
      if(is.null(timing_summary) || is.null(timing_sd)) {
        cat("Skipped storing results due to timing extraction failure.\n")
        next
      }

      # Ensure all factor levels are present before join
      all_expr_levels <- levels(mb$expr)
      if(nrow(timing_sd) < length(all_expr_levels)) {
        missing_expr_sd <- setdiff(all_expr_levels, as.character(timing_sd$expr))
        if(length(missing_expr_sd) > 0) { timing_sd <- bind_rows(timing_sd, data.frame(expr = factor(missing_expr_sd, levels=all_expr_levels), sd_time = NA_real_)) %>% arrange(factor(expr, levels=all_expr_levels)) }
      }
      if(nrow(timing_summary) < length(all_expr_levels)) {
        missing_expr_ts <- setdiff(all_expr_levels, as.character(timing_summary$expr))
        if(length(missing_expr_ts) > 0) { timing_summary <- bind_rows(timing_summary, data.frame(expr = factor(missing_expr_ts, levels=all_expr_levels), mean = NA_real_, median=NA_real_)) %>% arrange(factor(expr, levels=all_expr_levels)) }
      }

      timing_stats <- left_join(timing_summary, timing_sd, by = "expr")

      # --- Extract Iteration Stats ---
      results_per_run <- try(attr(mb, "values"), silent = TRUE)
      iter_data <- mb$expr
      iterations_vec <- rep(NA_integer_, length(results_per_run))

      if (!inherits(results_per_run, "try-error") && length(results_per_run) == length(iter_data)) {
        for (k in seq_along(results_per_run)) {
          res <- results_per_run[[k]]
          if (is.numeric(res) && !is.null(names(res)) && "iterations" %in% names(res)) {
            iter_val <- res[["iterations"]]
            if(is.finite(iter_val)){ iterations_vec[k] <- as.integer(round(iter_val)) }
          }
        }
        iterations_df <- data.frame(expr = iter_data, iterations = iterations_vec)
      } else {
        warning(paste("Iteration results extraction failed for n=",n0,", scenario=",scen_name,".",sep=""))
        iterations_df <- data.frame(expr = iter_data, iterations = NA_integer_) # Ensure df structure
      }

      iter_summary <- iterations_df %>% filter(!is.na(iterations)) %>% group_by(expr) %>%
        summarise(mean_iter = mean(iterations, na.rm = TRUE), sd_iter = sd(iterations, na.rm = TRUE), n_successful = n(), .groups = 'drop')

      if(nrow(iter_summary) < length(all_expr_levels)) {
        missing_expr_iter <- setdiff(all_expr_levels, as.character(iter_summary$expr))
        if(length(missing_expr_iter) > 0) { iter_summary <- bind_rows(iter_summary, data.frame(expr = factor(missing_expr_iter, levels=all_expr_levels), mean_iter = NA_real_, sd_iter = NA_real_, n_successful = 0)) %>% arrange(factor(expr, levels=all_expr_levels)) }
      }

      # --- Combine and Store Results ---
      if (nrow(timing_stats) == length(all_expr_levels) && nrow(iter_summary) == length(all_expr_levels)) {
        timing_stats$expr <- factor(timing_stats$expr, levels=all_expr_levels) # Ensure factors match
        iter_summary$expr <- factor(iter_summary$expr, levels=all_expr_levels)
        combined_stats <- left_join(timing_stats, iter_summary, by = "expr")
        results_list[[length(results_list) + 1]] <- combined_stats %>% mutate(scenario = scen_name, n = n0) %>% select(n, scenario, expr, mean, median, sd_time, mean_iter, sd_iter, n_successful)
      } else {
        warning(paste("Timing/Iteration summary incomplete for n=",n0,", scenario=",scen_name,". Skipping.",sep=""))
      }

    } else {
      cat("Skipped storing results due to benchmark failure or NULL mb.\n")
    }
  } # End scenario loop
} # End sample size loop

cat("\n--- Misspecified Benchmark Complete ---\n")

# --- Process and Display Results ---
if (length(results_list) > 0) {
  final_results <- rbindlist(results_list) # Use rbindlist for efficiency

  # --- Calculate Ratio ---
  results_wide <- dcast(as.data.table(final_results), # Ensure data.table
                        n + scenario ~ expr,
                        value.var = c("median", "mean_iter")) # Get median time and mean iter

  results_wide[, ratio_halley_newton := ifelse(median_newton > 0, median_halley / median_newton, NA_real_)]

  # --- Print Tables ---
  cat("\n--- Final Combined Results ---\n")
  options(max.print = 2000)
  print.data.frame(final_results, na.print = "NA", digits = 4, row.names = FALSE, max = nrow(final_results))
  cat("\n--- Performance Ratio (Halley/Newton Median Time) & Mean Iterations ---\n")
  print.data.frame(results_wide, na.print = "NA", digits = 4, row.names = FALSE, max = nrow(results_wide))
  cat("\n")
  options(max.print=99999)

  # Optional: Save results
  # saveRDS(final_results, "gumbel_benchmark_misspecified_raw.rds")
  # saveRDS(results_wide, "gumbel_benchmark_misspecified_wide.rds")

  # --- Plotting ---
  plot_data_ratio <- results_wide %>% filter(!is.na(ratio_halley_newton))
  plot_data_iter <- results_wide %>% filter(!is.na(mean_iter_newton) | !is.na(mean_iter_halley)) %>%
    pivot_longer(cols = c(mean_iter_newton, mean_iter_halley), names_to = "expr_iter", values_to = "mean_iter", names_prefix="mean_iter_") %>%
    mutate(expr = factor(ifelse(expr_iter=="newton","newton","halley"))) # Recreate expr column


  if(nrow(plot_data_ratio) > 0) {
    p_ratio <- ggplot(plot_data_ratio, aes(x = n, y = ratio_halley_newton)) +
      geom_hline(yintercept = 1.0, linetype = "dashed", color = "grey50") +
      geom_line(alpha = 0.8) +
      geom_point(alpha = 0.8) +
      facet_wrap(~ scenario, scales = "free_y") +
      scale_x_log10(breaks = sample_sizes, labels = label_number()) +
      scale_y_continuous(breaks = seq(0, 2, by=0.2)) + # Adjust y-axis breaks if needed
      coord_cartesian(ylim = c(0.5, 1.5)) + # Zoom y-axis
      labs(
        title = "Performance Ratio (Halley/Newton) under Model Misspecification",
        subtitle = "Ratio < 1 indicates Halley is faster",
        x = "Sample Size (n)",
        y = "Median Time Ratio (Halley / Newton)"
      ) +
      theme_bw(base_size = 11) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    print(p_ratio)
  } else { cat("\nNo ratio data to plot.\n") }

  if(nrow(plot_data_iter) > 0) {
    p_iter <- ggplot(plot_data_iter, aes(x = n, y = mean_iter, color = expr, group = expr)) +
      geom_line(alpha = 0.8) +
      geom_point(aes(shape=expr), size = 2) +
      facet_wrap(~ scenario, scales = "free_y") +
      scale_x_log10(breaks = sample_sizes, labels = label_number()) +
      scale_y_continuous(limits = c(0, NA)) + # Ensure y-axis starts at 0
      labs(
        title = "Mean Iterations under Model Misspecification",
        x = "Sample Size (n)",
        y = "Mean Iterations",
        color = "Method", shape="Method"
      ) +
      theme_bw(base_size = 11) +
      theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1))
    print(p_iter)
  } else { cat("\nNo iteration data to plot.\n") }

} else {
  cat("\nNo benchmark results were successfully collected for misspecified models.\n")
}