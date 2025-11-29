# --- Load Necessary Libraries ---
library(actuar)
library(univariateML)
library(frechetAEE)
library(dplyr)
library(tibble) # Using tibble for potentially cleaner data frame handling

# --- Simulation Setup ---
n_obs <- 20
n_reps <- 1000

shapes <- c(1, 5, 0.5, 5, 5,  0.5, 2, 10, 5, 1)  # Appended 5 new shapes
thetas <- c(1, 5, 5, 10^-3, 10^-15, 1e-3, 2, 5, 0.1, 1e-10) # Appended 5 new thetas
rates <- (1 / thetas)^(1 / shapes) # True rate (beta)

results_list <- vector("list", length(shapes))
names(results_list) <- paste0("Scenario_", 1:length(shapes))

# --- Run Simulation ---
set.seed(123)

for (i in 1:length(shapes)) {
  cat("Running Scenario", i, "...\n")

  true_shape <- shapes[i]
  true_theta2_p <- thetas[i]
  true_rate <- rates[i]

  # Storage for raw estimates
  ml_shape_est <- numeric(n_reps)
  ml_rate_est <- numeric(n_reps)
  aee_shape_est <- numeric(n_reps)
  aee_theta2_p_est <- numeric(n_reps)

  ml_success <- logical(n_reps)
  aee_success <- logical(n_reps)

  for (j in 1:n_reps) {
    if (j %% 100 == 0) cat("  Rep:", j, "/", n_reps, "\n")

    x <- actuar::rinvweibull(n_obs, shape = true_shape, rate = true_rate)

    # 2. Estimate using mlinvweibull (with stricter checks)
    ml_fit <- try(frechetr::mle(x), silent = TRUE)

    is_ml_success <- TRUE # Assume success initially

    if (inherits(ml_fit, "try-error")) {
      is_ml_success <- FALSE
    } else if (!is.numeric(ml_fit) || length(ml_fit) != 2 || !all(is.finite(ml_fit))) {
      # Check if output is not a finite vector of length 2
      is_ml_success <- FALSE
    } else if (ml_fit[1] <= 0 || ml_fit[2] <= 0) {
      # Check if shape or rate are non-positive (adjust if parameter space allows zero)
      is_ml_success <- FALSE
    }
    # Add any other checks specific to mlinvweibull convergence if possible (e.g., iterlim reached flag?)

    if (is_ml_success) {
      ml_shape_est[j] <- ml_fit[1]
      ml_rate_est[j] <- ml_fit[2]
      ml_success[j] <- TRUE
    } else {
      ml_shape_est[j] <- NA
      ml_rate_est[j] <- NA
      ml_success[j] <- FALSE # Mark as failure
    }
    # Estimate using frechetAEE
    aee_fit <- try(frechetAEE::ce_frechet_cpp(x), silent = TRUE)
    if (!inherits(aee_fit, "try-error") && is.finite(aee_fit$theta1_est) && is.finite(aee_fit$theta2_est) && aee_fit$theta2_est > 0) { # Add check for positive theta2_p
      aee_shape_est[j] <- aee_fit$theta1_est
      aee_theta2_p_est[j] <- aee_fit$theta2_est
      aee_success[j] <- TRUE
    } else {
      aee_shape_est[j] <- NA; aee_theta2_p_est[j] <- NA; aee_success[j] <- FALSE
    }
  }

  # --- Post-processing and RMSE calculation ---
  valid_ml_indices <- which(ml_success)
  valid_aee_indices <- which(aee_success)

  # Derive the "other" parameterization for each method
  # Need > 0 checks for rates/thetas before division/power

  # For ML: derive theta2_p = (1/rate)^shape
  valid_ml_rates <- ml_rate_est[valid_ml_indices]
  valid_ml_shapes <- ml_shape_est[valid_ml_indices]
  ml_theta2p_derived <- rep(NA_real_, n_reps)
  ml_theta2p_derived[valid_ml_indices[valid_ml_rates > 0]] <- (1 / valid_ml_rates[valid_ml_rates > 0])^valid_ml_shapes[valid_ml_rates > 0]

  # For AEE: derive rate = (1/theta2_p)^(1/shape)
  valid_aee_theta2ps <- aee_theta2_p_est[valid_aee_indices]
  valid_aee_shapes <- aee_shape_est[valid_aee_indices]
  aee_rate_derived <- rep(NA_real_, n_reps)
  valid_aee_subset <- valid_aee_indices[valid_aee_theta2ps > 0 & valid_aee_shapes != 0] # Avoid division by zero if shape is 0
  if(length(valid_aee_subset) > 0) {
    aee_rate_derived[valid_aee_subset] <- (1 / aee_theta2_p_est[valid_aee_subset])^(1 / aee_shape_est[valid_aee_subset])
  }


  # Calculate RMSE for all four combinations
  rmse_ml_shape <- sqrt(mean((ml_shape_est[valid_ml_indices] - true_shape)^2, na.rm = TRUE))
  rmse_ml_rate <- sqrt(mean((ml_rate_est[valid_ml_indices] - true_rate)^2, na.rm = TRUE))
  rmse_ml_theta2p <- sqrt(mean((ml_theta2p_derived[valid_ml_indices] - true_theta2_p)^2, na.rm = TRUE)) # Use derived theta2p

  rmse_aee_shape <- sqrt(mean((aee_shape_est[valid_aee_indices] - true_shape)^2, na.rm = TRUE))
  rmse_aee_rate <- sqrt(mean((aee_rate_derived[valid_aee_indices] - true_rate)^2, na.rm = TRUE)) # Use derived rate
  rmse_aee_theta2p <- sqrt(mean((aee_theta2_p_est[valid_aee_indices] - true_theta2_p)^2, na.rm = TRUE)) # Use original theta2p

  # Store results more clearly
  results_list[[i]] <- tibble(
    Scenario = i,
    True_Shape = true_shape,
    True_Rate = true_rate,
    True_Theta2_p = true_theta2_p,
    Method = c("mlinvweibull", "frechetAEE"),
    RMSE_Shape = c(rmse_ml_shape, rmse_aee_shape),
    RMSE_Rate = c(rmse_ml_rate, rmse_aee_rate), # Now compares rate for both
    RMSE_Theta2p = c(rmse_ml_theta2p, rmse_aee_theta2p), # Now compares theta2p for both
    ML_Failures = n_reps - sum(ml_success),
    AEE_Failures = n_reps - sum(aee_success)
  )
  cat("Scenario", i, "Done. ML Failures:", n_reps - sum(ml_success),
      "AEE Failures:", n_reps - sum(aee_success), "\n")
}

# --- Combine and Display Results ---
final_results <- bind_rows(results_list)
print(final_results, width = Inf) # Print full table width