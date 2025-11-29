# --- Function f(k) = (E[logX | alpha=1])^2 / Var(logX | alpha=1) ---
#     (Vectorized and slightly robust version)
f <- function(k) {
  psi_1 <- digamma(1)
  psiP1_1 <- trigamma(1)
  # Ensure k is within valid range (0, 1)
  k <- pmax(1e-15, pmin(1 - 1e-15, k)) # Clamp strictly
  psi_k <- digamma(k)
  psiP1_k <- trigamma(k)

  # Vectorized check for invalid inputs/results
  is_invalid <- is.na(psi_k) | is.na(psiP1_k) | is.na(psiP1_1 + psiP1_k) | (psiP1_1 + psiP1_k) <= 0
  res <- rep(NA_real_, length(k))
  valid_idx <- !is_invalid

  if(any(valid_idx)) {
     res[valid_idx] <- (psi_k[valid_idx] - psi_1)^2 / (psiP1_1 + psiP1_k[valid_idx])
  }
  # Handle edge cases explicitly for spline building if needed
  res[k==1] <- 0 # Analytically f(1)=0
  # Add check for k very near 0 if needed for f(k)=1 limit
  return(res)
}


# --- Create the Spline-based Inverse Function for k < 1 ---

# 1. Create Grid (Logit-spaced k values concentrate points near 0 and 1)
n_points <- 10000 # Number of grid points (increase for more accuracy)
logit_k_grid <- seq(-20, 20, length.out = n_points) # From k~3e-7 to k~1-3e-7
k_grid <- plogis(logit_k_grid)
k_grid <- sort(c(k_grid, seq(0, 1, length.out = n_points)))
# Ensure endpoints are included if desired, but avoid exact 0/1 for f(k) calc
# k_grid <- c(1e-15, k_grid, 1-1e-15) # Optional: Add points very close to edges

# 2. Evaluate f(k) on the grid
cv_grid <- f(k_grid)

# 3. Clean up grid points where f(k) failed (shouldn't happen with clamping)
valid_grid <- !is.na(cv_grid) & is.finite(cv_grid)
k_grid <- k_grid[valid_grid]
cv_grid <- cv_grid[valid_grid]

# Ensure cv_grid is sorted decreasingly (input for splinefun must be increasing/decreasing)
# f(k) decreases as k increases from 0 to 1, so cv should be decreasing
sort_order <- order(cv_grid, decreasing = TRUE)
k_grid_sorted <- k_grid[sort_order]
cv_grid_sorted <- cv_grid[sort_order]

# Add exact endpoints for spline interpolation range
# Input cv range is (0, 1), output k range is (1, 0)
k_grid_final <- c(1, k_grid_sorted, 0) # Corresponds to cv=0 ... cv=1
cv_grid_final <- c(0, cv_grid_sorted, 1) # Strictly increasing input for splinefun

# Remove potential duplicates if grid edges were too close to 0/1
dups <- duplicated(cv_grid_final)
k_grid_final <- k_grid_final[!dups]
cv_grid_final <- cv_grid_final[!dups]

# 4. Build the monotonic spline (cv is input 'x', k is output 'y')
k_from_cv_spline <- splinefun(cv_grid_final, k_grid_final, method = "monoH.FC")


# 5. Define the f_inv function using the spline
f_inv_spline <- function(cv) {
  if (cv == 0) return(c(k = 1))
  if (cv == 1) return(c(k = 0))
  cv_clamped <- pmax(1e-15, pmin(1 - 1e-15, cv))
  cv_clamped <- cv
  k_est <- k_from_cv_spline(cv_clamped)
  c(k = k_est)
}

x <- exp(seq(log(1e-1), log(1-1e-1), length.out = 1000))
y <- f(x)
results <- as.data.frame(cbind(x = x, cv = y, k = sapply(y, f_inv_spline)))
sum((results$x - results$k)^2)

plot(results$x, abs(results$x - results$k) / results$x)


plot(results$x, abs(results$x - results$k))


#
#
# # Apply the spline inverter
# results_spline <- as.data.frame(cbind(x = x, cv = y, t(sapply(y, f_inv_spline))))
#
# # Filter out potential NA results if f(k) failed
# results_valid_spline <- results_spline[!is.na(results_spline$k), ]
#
# sse_spline <- sum((results_valid_spline$x - results_valid_spline$k)^2)
# num_failed_spline <- nrow(results_spline) - nrow(results_valid_spline)
#
# cat("\n--- Results with Spline Inverter (k < 1) ---\n")
# cat("SSE on valid results:", sse_spline, "\n")
# cat("Number of failed inversions:", num_failed_spline, "\n")
#
# # Check points
# print(paste("Check k=0.13:", f_inv_spline(f(0.13))["k"]))
# print(paste("Check k=0.00001:", f_inv_spline(f(1e-5))["k"]))
# print(paste("Check k=0.99999:", f_inv_spline(f(0.99999))["k"]))
