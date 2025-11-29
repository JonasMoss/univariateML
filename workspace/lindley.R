rlindley2 <- function(n, alpha, theta) {
  if (!is.numeric(n) || length(n) != 1 || n <= 0 || floor(n) != n) {
    stop("'n' must be a single positive integer.")
  }
  if (!is.numeric(alpha) || length(alpha) != 1 || alpha <= 0) {
    stop("'alpha' must be a single positive number.")
  }
  if (!is.numeric(theta) || length(theta) != 1 || theta <= 0) {
    stop("'theta' must be a single positive number.")
  }
  prob1 <- theta / (1 + theta)
  u <- runif(n)
  samples_comp1 <- rgamma(n, shape = alpha, rate = theta)
  samples_comp2 <- rgamma(n, shape = alpha + 1, rate = theta)
  ifelse(u < prob1, samples_comp1, samples_comp2)
}

mllindley2_exact_ <- function(x, alpha_start = 1.0) {
  TOL <- .Machine$double.eps^0.5
  MAX_ITER <- 100

  n <- length(x)
  Sx <- sum(x)
  Slogx <- sum(log(x)) # Precompute sum of logs

  # Current estimate for alpha
  alpha_k <- alpha_start

  # --- Iteration Loop ---
  for (iter in 1:MAX_ITER) {
    # --- Step a: Profile theta ---
    B <- Sx - n * alpha_k
    C <- -n * (alpha_k + 1)
    discriminant <- B^2 - 4 * Sx * C
    if (discriminant < 0) {
      warning(paste("Negative discriminant encountered at iteration", iter, "alpha_k =", alpha_k, ". Stopping."))
      return(list(alpha = NA, theta = NA, logLik = NA, convergence = FALSE, iterations = iter, message = "Negative discriminant"))
    }
    theta_k <- (-B + sqrt(discriminant)) / (2 * Sx)
    if (theta_k <= 0) {
      warning(paste("Non-positive theta_k encountered at iteration", iter, "alpha_k =", alpha_k, "theta_k =", theta_k, ". Stopping."))
      return(list(alpha = NA, theta = NA, logLik = NA, convergence = FALSE, iterations = iter, message = "Non-positive theta"))
    }

    # --- Step b: Calculate Score U_alpha ---
    log_theta_k <- log(theta_k)
    psi_alpha1_k <- digamma(alpha_k + 1)
    sum_inv_alpha_xi <- sum(1 / (alpha_k + x))
    U_alpha_k <- n * log_theta_k - n * psi_alpha1_k + Slogx + sum_inv_alpha_xi

    # --- Step c: Calculate Components of Exact Hessian ---
    # H_aa = ∂²ℓ/∂α²
    H_aa_k <- -n * trigamma(alpha_k + 1) - sum(1 / (alpha_k + x)^2)

    # H_at = ∂²ℓ/∂α∂θ
    H_at_k <- n / theta_k

    # H_tt = ∂²ℓ/∂θ²
    H_tt_k <- -n * (alpha_k + 1) / theta_k^2 + n / (1 + theta_k)^2

    # Check H_tt (must be negative for maximization w.r.t theta)
    if (H_tt_k >= 0) {
      warning(paste("Non-negative H_tt encountered at iteration", iter, "alpha_k =", alpha_k, ". Stopping."))
      return(list(alpha = NA, theta = NA, logLik = NA, convergence = FALSE, iterations = iter, message = "Non-negative H_tt"))
    }

    # Calculate Exact Hessian H_exact = H_aa - (H_at)^2 / H_tt
    H_exact_k <- H_aa_k - (H_at_k^2) / H_tt_k

    # Check H_exact (must be negative for overall maximization w.r.t alpha)
    if (H_exact_k >= 0) {
      warning(paste("Non-negative Exact Hessian encountered at iteration", iter, "alpha_k =", alpha_k, ". Stopping."))
      return(list(alpha = NA, theta = NA, logLik = NA, convergence = FALSE, iterations = iter, message = "Non-negative Exact Hessian"))
    }

    # --- Step d: Update alpha ---
    delta_alpha <- -U_alpha_k / H_exact_k
    alpha_next <- alpha_k + delta_alpha

    if (alpha_next <= 0) {
      # Simplified handling: Try halving the step. More robust methods exist.
      num_halves <- 0
      max_halves <- 10
      alpha_temp <- alpha_next
      delta_temp <- delta_alpha
      while(alpha_temp <= 0 && num_halves < max_halves){
        delta_temp <- delta_temp / 2
        alpha_temp <- alpha_k + delta_temp
        num_halves <- num_halves + 1
      }
      alpha_next <- alpha_temp
      if (alpha_next <= 0) {
        warning(paste("alpha went non-positive at iteration", iter, "alpha_k =", alpha_k, "even after step halving. Stopping."))
        return(list(alpha = NA, theta = NA, logLik = NA, convergence = FALSE, iterations = iter, message = "Alpha non-positive after step halving"))
      }
    }


    # --- Step e: Check Convergence ---
    if (abs(alpha_next - alpha_k) / abs(alpha_k + TOL) < TOL) {
      alpha_mle <- alpha_next
      # Recalculate final theta_mle based on final alpha_mle
      B_mle <- Sx - n * alpha_mle
      C_mle <- -n * (alpha_mle + 1)
      discriminant_mle <- B_mle^2 - 4 * Sx * C_mle
      if (discriminant_mle < 0) {
        theta_mle <- NA
        warning("Negative discriminant at final step.")
      } else {
        theta_mle <- (-B_mle + sqrt(discriminant_mle)) / (2 * Sx)
      }

      # Calculate final log-likelihood
      logLik_val <- NA
      if (!is.na(theta_mle) && theta_mle > 0 && alpha_mle > 0) {
        logLik_val <- n * (alpha_mle + 1) * log(theta_mle) -
          n * log(1 + theta_mle) -
          n * lgamma(alpha_mle + 1) +
          (alpha_mle - 1) * Slogx +
          sum(log(alpha_mle + x)) -
          theta_mle * Sx
      }

      return(list(alpha = alpha_mle,
                  theta = theta_mle,
                  logLik = logLik_val,
                  convergence = TRUE,
                  iterations = iter,
                  message = "Converged"))
    }

    # --- Prepare for next iteration ---
    alpha_k <- alpha_next

  } # End of iteration loop

  # If loop finished without converging
  warning("Maximum iterations reached without convergence.")
  # Calculate final theta based on last alpha_k
  B_last <- Sx - n * alpha_k
  C_last <- -n * (alpha_k + 1)
  discriminant_last <- B_last^2 - 4 * Sx * C_last
  if (discriminant_last < 0 || alpha_k <= 0) {
    theta_last <- NA
  } else {
    theta_last <- (-B_last + sqrt(discriminant_last)) / (2 * Sx)
    if (theta_last <= 0) theta_last <- NA
  }
  # Calculate final log-likelihood
  logLik_val <- NA
  if (!is.na(theta_last) && !is.na(alpha_k) && theta_last > 0 && alpha_k > 0) {
    logLik_val <- n * (alpha_k + 1) * log(theta_last) -
      n * log(1 + theta_last) -
      n * lgamma(alpha_k + 1) +
      (alpha_k - 1) * Slogx +
      sum(log(alpha_k + x)) -
      theta_last * Sx
  }

  return(list(alpha = alpha_k,
              theta = theta_last,
              logLik = logLik_val,
              convergence = FALSE,
              iterations = MAX_ITER,
              message = "Max iterations reached"))
}

bank_data <- c(0.8, 0.8, 1.3, 1.5, 1.8, 1.9, 1.9, 2.1, 2.6, 2.7, 2.9, 3.1, 3.2, 3.3, 3.5, 3.6, 4.0, 4.1, 4.2, 4.2, 4.3, 4.3, 4.4, 4.4, 4.6, 4.7, 4.7, 4.8, 4.9, 4.9, 5.0, 5.3, 5.5, 5.7, 5.7, 6.1, 6.2, 6.2, 6.2, 6.3, 6.7, 6.9, 7.1, 7.1, 7.1, 7.1, 7.4, 7.6, 7.7, 8.0, 8.2, 8.6, 8.6, 8.6, 8.8, 8.8, 8.9, 8.9, 9.5, 9.6, 9.7, 9.8, 10.7, 10.9, 11.0, 11.0, 11.1, 11.2, 11.2, 11.5, 11.9, 12.4, 12.5, 12.9, 13.0, 13.1, 13.3, 13.6, 13.7, 13.9, 14.1, 15.4, 15.4, 17.3, 17.3, 18.1, 18.2, 18.4, 18.9, 19.0, 19.9, 20.6, 21.3, 21.4, 21.9, 23.0, 27.0, 31.6, 33.1, 38.5)
fit_exact_close_start <- mllindley2_exact_(bank_data, alpha_start = 1.4)
print(fit_exact_close_start)

# Fit the model using the exact Hessian, starting at alpha=2.0
fit_exact_alt_start <- mllindley2_exact_(bank_data, alpha_start = 2.0)
print(fit_exact_alt_start)

# Using the mllindley2_exact_ function and bank_data

# First, get the MLE value with high precision from a previous run
fit_ref <- mllindley2_exact_(bank_data, alpha_start = 1.4) # Run again to ensure we have the value
mle_alpha_precise <- fit_ref$alpha
print(paste("Starting exactly at alpha =", mle_alpha_precise))

# Now run starting exactly at this value
fit_at_mle_precise <- mllindley2_exact_(bank_data, alpha_start = mle_alpha_precise)
print(fit_at_mle_precise)
