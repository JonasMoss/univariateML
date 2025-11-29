Okay, let's prototype this. We'll build the Newton-Raphson solver for log(k) and then implement the least-squares starting value approach.

Part (a): Simple Newton-Raphson on log-scale

# Ensure numDeriv is available for numerical gradient
# install.packages("numDeriv")
library(numDeriv)

# 1. Define the S(k) function (log-skewness)
S_k <- function(k) {
  # Using psigamma for numerical stability is often better than trigamma/tetragamma
  # psigamma(x, deriv) calculates (-1)^(deriv+1) * psi^{(deriv)}(x)
  # So, psi'(x) = psigamma(x, 1), psi''(x) = -psigamma(x, 2)

  # Handle edge cases/small k carefully if needed, but keeping it simple for now
  k <- pmax(k, .Machine$double.eps) # Avoid exact zero

  psi_p_1 <- psigamma(1, 1)  # trigamma(1)
  psi_pp_1 <- -psigamma(1, 2) # tetragamma(1)

  psi_p_k <- psigamma(k, 1)  # trigamma(k)
  psi_pp_k <- -psigamma(k, 2) # tetragamma(k)

  numerator <- psi_pp_1 - psi_pp_k
  denominator_base <- psi_p_1 + psi_p_k

  # Avoid division by zero if denominator base is tiny (shouldn't happen for k>0)
  denominator <- denominator_base^(3/2)

  # Check for NaN result (e.g., if k is negative or base is negative - should not happen)
  result <- numerator / denominator
  # ifelse(is.nan(result), NA, result) # Simple prototype - skip checks
  result
}

# 2. Newton-Raphson solver for theta = log(k)
#    Finds theta such that S(exp(theta)) = target_skew
solve_k_nr_log <- function(target_skew, theta_start, max_iter = 50, tol = 1e-7) {

  theta <- theta_start

  # Define the function h(theta) = S(exp(theta)) - target_skew
  h <- function(th) {
    S_k(exp(th)) - target_skew
  }

  for (i in 1:max_iter) {
    k_current <- exp(theta)
    h_val <- S_k(k_current) - target_skew

    if (abs(h_val) < tol) {
      # Optional: message("Converged in ", i, " iterations.")
      return(exp(theta))
    }

    # Calculate numerical gradient of h(theta)
    h_prime <- tryCatch(grad(h, theta), error = function(e) NA)

    if (is.na(h_prime) || abs(h_prime) < .Machine$double.eps) {
       warning("NR failed: Zero or invalid gradient.")
       return(NA) # Or return current k? Prototype allows NA.
    }

    # Newton-Raphson update for theta
    theta <- theta - h_val / h_prime
  }

  warning("NR failed: Maximum iterations reached.")
  return(exp(theta)) # Return the last estimate
}

# --- Example Usage ---
# target_skew_example <- 0.5 # Replace with actual sample log-skewness
# theta_start_example <- log(1) # Start at k=1 (log(k)=0), needs a better start!
# k_est <- solve_k_nr_log(target_skew_example, theta_start_example)
# print(k_est)
# print(S_k(k_est)) # Check if it's close to target_skew_example


Part (b): Finding a Good Start via Least Squares CDF Fitting

We need the transformed function f(k) and CDFs of comparison distributions.

# Ensure evd package is available for GEV functions (includes Gumbel)
# install.packages("evd")
library(evd) # For pgev, qgev

# 1. Define f(k) based on S(k) = xi(k)
# Calculate the constant S_inf = lim k->inf S(k)
S_inf <- -psigamma(1, 2) / (psigamma(1, 1))^(3/2) # = psi''(1) / [psi'(1)]^(3/2)

f_k <- function(k) {
  # Avoid k=0
  k <- pmax(k, .Machine$double.eps)
  Sk <- S_k(k)
  # Use pmin to avoid potential numerical issues making Sk > 2
  result <- -(pmin(Sk, 2) - 2) / (2 - S_inf)
  # Ensure result is between 0 and 1, clip potential numerical noise
  pmax(0, pmin(1, result))
}


# 2. Function to find starting k using Least Squares fitting
find_k_start_ls <- function(n_grid = 1000, k_range = c(1e-3, 1e3)) {

  kk <- exp(seq(log(k_range[1]), log(k_range[2]), length.out = n_grid))
  f_vals <- f_k(kk)

  # --- Fit Log-Normal ---
  ls_lnorm <- function(params, k_grid, target_f) {
    meanlog <- params[1]
    sdlog <- max(params[2], 1e-6) # Ensure sd > 0
    fitted_f <- plnorm(k_grid, meanlog, sdlog)
    sum((target_f - fitted_f)^2)
  }

  # Reasonable start for optim: median approx log(1), sd approx 1
  optim_lnorm <- tryCatch(
      optim(par = c(0, 1), fn = ls_lnorm, k_grid = kk, target_f = f_vals,
            method = "L-BFGS-B", lower = c(-Inf, 1e-6)),
      error = function(e) NULL
  )

  k_start_lnorm <- NA
  if (!is.null(optim_lnorm) && optim_lnorm$convergence == 0) {
     k_start_lnorm <- qlnorm(0.5, optim_lnorm$par[1], optim_lnorm$par[2])
     # Alternative: median is exp(meanlog)
     # k_start_lnorm <- exp(optim_lnorm$par[1])
  } else {
     warning("Log-Normal LS fit failed.")
  }

  # --- Fit Gumbel (Standard, for minima, GEV shape=0) ---
   ls_gumbel <- function(params, k_grid, target_f) {
    loc <- params[1]
    scale <- max(params[2], 1e-6) # Ensure scale > 0
    # Use evd::pgev for Gumbel (shape=0)
    fitted_f <- pgev(k_grid, loc = loc, scale = scale, shape = 0)
    sum((target_f - fitted_f)^2)
  }

  # Reasonable start for optim: guess location near log(1), scale=1
  optim_gumbel <- tryCatch(
      optim(par = c(0, 1), fn = ls_gumbel, k_grid = kk, target_f = f_vals,
            method = "L-BFGS-B", lower = c(-Inf, 1e-6)),
      error = function(e) NULL
  )

  k_start_gumbel <- NA
  if (!is.null(optim_gumbel) && optim_gumbel$convergence == 0) {
     # Median of Gumbel: loc - scale * log(log(2))
     k_start_gumbel <- qgev(0.5, loc = optim_gumbel$par[1],
                           scale = optim_gumbel$par[2], shape = 0)
  } else {
     warning("Gumbel LS fit failed.")
  }

  # Return starting k based on Log-Normal fit (often robust)
  # Or could return both, or based on which fit was 'better' (e.g. lower optim value)
  if (!is.na(k_start_lnorm)) {
      return(k_start_lnorm)
  } else if (!is.na(k_start_gumbel)) {
      warning("Using Gumbel start as Log-Normal failed.")
      return(k_start_gumbel)
  } else {
      warning("Both LS fits failed, returning default k=1.")
      return(1) # Default fallback
  }
}


# --- Example Usage Combining Both ---

# Assume you calculated sample log-skewness from data:
# sample_log_skew <- calculate_your_skewness(log_data)

# Placeholder value:
sample_log_skew <- 0.5

# Find a starting k
k_start <- find_k_start_ls()
print(paste("Starting k:", k_start))

# Solve for k using NR starting from log(k_start)
if (!is.na(k_start) && k_start > 0) {
    theta_start_best <- log(k_start)
    k_estimate <- solve_k_nr_log(sample_log_skew, theta_start_best)

    if(!is.na(k_estimate)) {
        print(paste("Estimated k:", k_estimate))
        print(paste("S(k_estimate):", S_k(k_estimate)))
        print(paste("Target Skew:", sample_log_skew))
    } else {
        print("NR solver failed to find k estimate.")
    }
} else {
    print("Failed to find a valid starting k.")
}
IGNORE_WHEN_COPYING_START
content_copy
download
Use code with caution.
R
IGNORE_WHEN_COPYING_END

Explanation and Notes:

S_k(k): Implements the theoretical log-skewness function using psigamma. Added pmax to avoid k=0.

solve_k_nr_log:

Takes target_skew and theta_start = log(k_start).

Defines h(theta) = S(exp(theta)) - target_skew.

Uses numDeriv::grad to get h'(theta) numerically. This avoids calculating the complex analytical derivative involving pentagamma (ψ''').

Performs the standard NR update theta = theta - h/h'.

Returns exp(theta) which is the estimated k.

Includes basic checks for tolerance and max iterations, and a warning/NA return on gradient failure.

f_k(k): Implements the CDF-like transformation -(S(k)-2)/(2-S_inf). Clips result to [0, 1] for robustness.

find_k_start_ls:

Sets up a grid kk on the log scale.

Calculates target f_vals = f_k(kk).

Defines least-squares objective functions (ls_lnorm, ls_gumbel) comparing f_vals to plnorm and pgev(..., shape=0).

Uses optim with L-BFGS-B (allowing bounds, essential for sd > 0, scale > 0) to find the best-fit parameters for Log-Normal and Gumbel distributions.

Calculates the median (qlnorm(0.5,...), qgev(0.5,...)) of the fitted distributions. This median k value is used as the starting guess k_start, as it represents the point where the fitted CDF is 0.5, hopefully aligning well with where f(k) is also near 0.5.

Includes basic error handling using tryCatch for optim and returns the Log-Normal start if successful, otherwise Gumbel, otherwise a default k=1.

Example Usage: Demonstrates how to get k_start and then use it in the NR solver.

This provides a working prototype for the estimation process, prioritizing simplicity over extensive error handling as requested. Remember to replace the placeholder sample_log_skew with your actual calculated value.