# --- 0. Setup ---
# Install extraDistr if you haven't already
# install.packages("extraDistr")

library(extraDistr)

# Function to calculate the theoretical asymptotic covariance matrix
# (Include the function from the previous answer here)
kumaraswamy_log_moment_cov_matrix <- function(a, b) {
  if (!is.numeric(a) || length(a) != 1 || a <= 0) {
    stop("Parameter 'a' must be a single positive number.")
  }
  if (!is.numeric(b) || length(b) != 1 || b <= 0) {
    stop("Parameter 'b' must be a single positive number.")
  }
  delta1 <- psigamma(1, deriv = 1) - psigamma(b + 1, deriv = 1)
  delta2 <- psigamma(1, deriv = 2) - psigamma(b + 1, deriv = 2)
  delta3 <- psigamma(1, deriv = 3) - psigamma(b + 1, deriv = 3)
  sigma_11 <- (1 / a^2) * delta1
  sigma_12 <- (1 / a^3) * delta2
  sigma_21 <- sigma_12
  sigma_22 <- (1 / a^4) * (2 * delta1^2 + delta3)
  cov_matrix <- matrix(c(sigma_11, sigma_21,
                         sigma_12, sigma_22),
                       nrow = 2, ncol = 2, byrow = TRUE)
  dimnames(cov_matrix) <- list(c("m", "s^2"), c("m", "s^2"))
  return(cov_matrix)
}


# --- 1. Simulation Parameters ---
a_param <- 9        # Kumaraswamy parameter a
b_param <- 3.0        # Kumaraswamy parameter b
n_sample_size <- 500  # Sample size for each simulation run (needs to be reasonably large)
M_simulations <- 20000 # Number of Monte Carlo simulations (needs to be large for stability)

set.seed(123)         # for reproducibility

cat("--- Simulation Parameters ---\n")
cat("a =", a_param, "\nb =", b_param, "\n")
cat("Sample size (n) =", n_sample_size, "\n")
cat("Number of simulations (M) =", M_simulations, "\n\n")


# --- 2. Calculate Theoretical Matrix ---
theoretical_cov_mat <- kumaraswamy_log_moment_cov_matrix(a_param, b_param)

cat("--- Theoretical Asymptotic Covariance Matrix (Sigma) ---\n")
print(theoretical_cov_mat)
cat("\n")


# --- 3. Run Monte Carlo Simulation ---
cat("--- Running Monte Carlo Simulation... ---\n")

# Matrix to store results: M rows, 2 columns (for m and s^2)
simulation_results <- matrix(NA, nrow = M_simulations, ncol = 2)
colnames(simulation_results) <- c("m", "s^2")

pb <- txtProgressBar(min = 0, max = M_simulations, style = 3) # Progress bar

for (i in 1:M_simulations) {
  # Generate one sample of size n
  x_sample <- extraDistr::rkumar(n_sample_size, a = a_param, b = b_param)

  # Log-transform the sample
  # Add a check for zero, though rkumar should generate (0, 1)
  if(any(x_sample <= 0 | x_sample >= 1)) {
    warning(paste("Sample", i, "contained values <= 0 or >= 1. Skipping."))
    # Optionally filter: x_sample <- x_sample[x_sample > 0 & x_sample < 1]
    # If filtering, ensure n is still large enough after filtering
    y_sample <- log(x_sample[x_sample > 0 & x_sample < 1])
    if(length(y_sample) < 2) next # Need at least 2 points for variance
  } else {
    y_sample <- log(x_sample)
  }


  # Calculate sample log-mean (m) and sample log-variance (s^2)
  sample_m <- mean(y_sample)
  sample_s2 <- var(y_sample) # Uses n-1 denominator, which is correct for s^2

  # Store the results
  simulation_results[i, ] <- c(sample_m, sample_s2)

  setTxtProgressBar(pb, i) # Update progress bar
}
close(pb) # Close progress bar

# Remove rows with NA if any samples were skipped (unlikely with rkumar > 0)
simulation_results <- na.omit(simulation_results)
actual_M <- nrow(simulation_results)
if(actual_M < M_simulations) {
  cat("Warning: Some simulations were skipped due to invalid values. Actual M =", actual_M, "\n")
}

cat("Simulation complete.\n\n")


# --- 4. Calculate Empirical Covariance Matrix ---
# Calculate covariance matrix of the M pairs of (m, s^2)
empirical_cov_raw <- cov(simulation_results)

# Scale by n to compare with the asymptotic theoretical matrix Sigma
empirical_cov_scaled <- n_sample_size * empirical_cov_raw

cat("--- Scaled Empirical Covariance Matrix (n * Cov(m, s^2)) ---\n")
print(empirical_cov_scaled)
cat("\n")


# --- 5. Compare ---
cat("--- Comparison ---\n")
difference_matrix <- theoretical_cov_mat - empirical_cov_scaled
relative_difference_matrix <- difference_matrix / theoretical_cov_mat

cat("Absolute Difference (Theoretical - Empirical):\n")
print(difference_matrix)
cat("\n")

cat("Relative Difference ((Theoretical - Empirical) / Theoretical):\n")
print(relative_difference_matrix)
cat("\n")

# Check if matrices are reasonably close (e.g., relative difference < 5-10%)
tolerance <- 0.10
if (all(abs(relative_difference_matrix) < tolerance, na.rm = TRUE)) {
  cat("Verification successful: Empirical matrix is reasonably close to the theoretical matrix (relative diff <", tolerance * 100, "%).\n")
} else {
  cat("Verification warning: Empirical matrix differs significantly from the theoretical matrix.\n")
  cat("Try increasing n_sample_size and/or M_simulations.\n")
}

lmom <- function(mu, sigma2) {
  neuler <- digamma(1) # Negative Euler-Mascheroni constant
  basel <- pi^2 / 6 # trigamma(1)

  f_over_df <- function(a) {
    b <- inverse_digamma(neuler - a * mu)
    tri <- trigamma(b)
    tetra <- psigamma(b, deriv = 2)

    f <- a * tri * (basel - tri - a^2 * sigma2)
    df <- a * tetra * mu - 2 * tri * (basel + tri)
    f / df
  }

  a <- newton_raphson_1d(f_over_df, sqrt(basel / sigma2))
  b <- inverse_digamma(neuler - a * mu) - 1
  c(a=a, b=b)
}

# --- 3. Theoretical Moment Functions ---
E_logX <- function(a, b) {
  if(a <= 0 || b <= 0) return(NA)
  (1/a) * (digamma(1) - digamma(b + 1))
}

Var_logX <- function(a, b) {
  if(a <= 0 || b <= 0) return(NA)
  (1/a^2) * (psigamma(1, deriv = 1) - psigamma(b + 1, deriv = 1))
}

# --- 4. Asymptotic Covariance Matrix Function ---
# (Copied from previous answer)
kumaraswamy_log_moment_cov_matrix <- function(a, b) {
  if (!is.numeric(a) || length(a) != 1 || a <= 0) return(matrix(NA, 2, 2))
  if (!is.numeric(b) || length(b) != 1 || b <= 0) return(matrix(NA, 2, 2))

  delta1 <- tryCatch(psigamma(1, deriv = 1) - psigamma(b + 1, deriv = 1), error = function(e) NA)
  delta2 <- tryCatch(psigamma(1, deriv = 2) - psigamma(b + 1, deriv = 2), error = function(e) NA)
  delta3 <- tryCatch(psigamma(1, deriv = 3) - psigamma(b + 1, deriv = 3), error = function(e) NA)

  if(anyNA(c(delta1, delta2, delta3))) return(matrix(NA, 2, 2))

  sigma_11 <- (1 / a^2) * delta1
  sigma_12 <- (1 / a^3) * delta2
  sigma_21 <- sigma_12
  sigma_22 <- (1 / a^4) * (2 * delta1^2 + delta3)

  cov_matrix <- matrix(c(sigma_11, sigma_21,
                         sigma_12, sigma_22),
                       nrow = 2, ncol = 2, byrow = TRUE)
  dimnames(cov_matrix) <- list(c("m", "s^2"), c("m", "s^2"))
  return(cov_matrix)
}


# --- 5. One-Step Optimal GMM Estimator ---

#' One-Step Optimal GMM for Kumaraswamy using Log-Moments
#'
#' Estimates parameters (a, b) using sample log-mean and log-variance.
#'
#' @param mu_sample Sample mean of log(X).
#' @param sigma2_sample Sample variance of log(X).
#' @param a_init Optional initial guess for 'a'. If NULL, uses lmom.
#' @param b_init Optional initial guess for 'b'. If NULL, uses lmom.
#' @param lower_bound Lower bound for parameters during optimization.
#' @param optim_control List of control parameters for optim.
#'
#' @return A list containing:
#'   - estimates: The estimated parameters (a, b).
#'   - initial_estimates: The initial parameters used for optimization.
#'   - weighting_matrix_inv: The inverse weighting matrix (W*) used.
#'   - optim_result: The raw output from the optim function.
#'   - convergence: Convergence code from optim (0 = success).
#'
#' @examples
#' # --- Generate Sample Data ---
#' library(extraDistr)
#' set.seed(456)
#' a_true <- 3.5
#' b_true <- 1.5
#' n <- 1000
#' x_data <- rkumar(n, a = a_true, b = b_true)
#' y_data <- log(x_data)
#' mu_s <- mean(y_data)
#' s2_s <- var(y_data)
#'
#' # --- Estimate ---
#' gmm_result <- optimal_gmm_kumaraswamy(mu_s, s2_s)
#' print("GMM Estimates:")
#' print(gmm_result$estimates)
#' print("True Parameters:")
#' print(c(a=a_true, b=b_true))
#' print("Initial Estimates:")
#' print(gmm_result$initial_estimates)
#' print("Convergence Code:")
#' print(gmm_result$convergence)

optimal_gmm_kumaraswamy <- function(mu_sample, sigma2_sample,
                                    a_init = NULL, b_init = NULL,
                                    lower_bound = 1e-6,
                                    optim_control = list(maxit=500)) {

  # --- Step 1: Get Initial Estimates ---
  if (is.null(a_init) || is.null(b_init)) {
    initial_est <- lmom(mu_sample, sigma2_sample)
    if (anyNA(initial_est)) {
      stop("Failed to get valid initial estimates from lmom.")
    }
    a0 <- initial_est['a']
    b0 <- initial_est['b']
  } else {
    a0 <- a_init
    b0 <- b_init
    initial_est <- c(a=a0, b=b0)
    if(a0 <= 0 || b0 <= 0) stop("Provided initial estimates must be positive.")
  }
  cat("Initial estimates: a =", sprintf("%.4f", a0), ", b =", sprintf("%.4f", b0), "\n")

  # --- Step 2: Calculate Optimal Weighting Matrix ---
  Sigma <- kumaraswamy_log_moment_cov_matrix(a0, b0)
  if (anyNA(Sigma)) {
    stop("Failed to calculate covariance matrix at initial estimates.")
  }

  W_inv <- tryCatch({
    solve(Sigma)
  }, error = function(e) {
    warning(paste("Covariance matrix Sigma is singular or near-singular at initial estimates. Using identity matrix.\nSigma:\n", paste(capture.output(print(Sigma)), collapse="\n")))
    # Fallback to identity matrix (non-optimal GMM)
    diag(2)
  })
  # Check if inversion was successful even without error (might produce NAs/Infs)
  if (any(!is.finite(W_inv))) {
    warning(paste("Inverse of covariance matrix contains non-finite values. Using identity matrix.\nW_inv:\n", paste(capture.output(print(W_inv)), collapse="\n")))
    W_inv <- diag(2)
  }


  # --- Step 3: Define GMM Objective Function ---
  gmm_objective <- function(params, mu_s, s2_s, W_inv_mat) {
    a_par <- params[1]
    b_par <- params[2]

    # Parameter validity check (handled by L-BFGS-B bounds, but good practice)
    if (a_par <= 0 || b_par <= 0) {
      return(1e10) # Penalize invalid parameters
    }

    # Calculate theoretical moments at current params
    E_logX_theo <- E_logX(a_par, b_par)
    Var_logX_theo <- Var_logX(a_par, b_par)

    if (is.na(E_logX_theo) || is.na(Var_logX_theo)) {
      return(1e10) # Penalize if theoretical moments are NA
    }

    # Moment conditions vector g(theta)
    g <- c(mu_s - E_logX_theo,
           s2_s - Var_logX_theo)

    # GMM quadratic form Q = g' * W_inv * g
    Q <- t(g) %*% W_inv_mat %*% g

    # Ensure result is scalar and finite
    if (!is.finite(Q) || length(Q)!=1) return(1e10)

    return(as.numeric(Q))
  }

  # --- Step 4: Minimize Objective Function ---
  optim_result <- optim(
    par = c(a0, b0),             # Start at initial estimates
    fn = gmm_objective,          # Function to minimize
    mu_s = mu_sample,            # Pass sample moments to fn
    s2_s = sigma2_sample,
    W_inv_mat = W_inv,           # Pass weighting matrix to fn
    method = "L-BFGS-B",         # Method supporting box constraints
    lower = c(lower_bound, lower_bound), # Ensure a > 0, b > 0
    control = optim_control      # Control parameters (e.g., max iterations)
  )

  # --- Step 5: Return Results ---
  estimates <- setNames(optim_result$par, c("a", "b"))

  return(list(
    estimates = estimates,
    initial_estimates = initial_est,
    weighting_matrix_inv = W_inv,
    optim_result = optim_result,
    convergence = optim_result$convergence # 0 indicates success
  ))
}

# --- Example Usage ---
library(extraDistr)
set.seed(456)
a_true <- 3.5
b_true <- 1.5
n <- 1000 # Sample size
x_data <- rkumar(n, a = a_true, b = b_true)

# Handle potential zeros/ones although unlikely for Kumaraswamy
y_data <- log(x_data)

# Calculate sample moments
mu_s <- mean(y_data)
s2_s <- var(y_data) # Use sample variance (n-1 denominator)

cat("\n--- Running GMM Estimation ---\n")
cat("Sample log-mean (mu_sample):", mu_s, "\n")
cat("Sample log-variance (sigma2_sample):", s2_s, "\n")

# Estimate using the function
gmm_result <- tryCatch({
  optimal_gmm_kumaraswamy(mu_s, s2_s)
}, error = function(e){
  cat("Error during GMM estimation:", e$message, "\n")
  NULL
})


if (!is.null(gmm_result)) {
  cat("\n--- GMM Results ---\n")
  print("GMM Estimates:")
  print(gmm_result$estimates)
  print("True Parameters:")
  print(c(a=a_true, b=b_true))
  # print("Initial Estimates:")
  # print(gmm_result$initial_estimates)
  print("Convergence Code (0=success):")
  print(gmm_result$convergence)
  # print("Weighting Matrix Inverse (W*):")
  # print(gmm_result$weighting_matrix_inv)
}
# --- 1. Theoretical Moment Functions ---

E_logX <- function(a, b) {
  if(a <= 0 || b <= 0) return(NA)
  (1/a) * (digamma(1) - digamma(b + 1))
}

Var_logX <- function(a, b) {
  if(a <= 0 || b <= 0) return(NA)
  (1/a^2) * (psigamma(1, deriv = 1) - psigamma(b + 1, deriv = 1))
}

E_X <- function(a, b) {
  if(a <= 0 || b <= 0) return(NA)
  # Use log-gamma for numerical stability
  tryCatch(b * exp(lgamma(1 + 1/a) + lgamma(b) - lgamma(1 + 1/a + b)),
           warning = function(w) NA, # Handle potential issues in lgamma args
           error = function(e) NA)
  # Alternative: b * beta(1 + 1/a, b) # Might be less stable
}

# --- 2. Functions for Covariance Matrix Components ---
# Let Y = log(X), mu_y = E[Y]

# E[Y^k] = E[log^k X]
E_logkX <- function(a, b, k) {
  if(a <= 0 || b <= 0) return(NA)
  term_mult <- 1 / (a^k)
  psi <- function(x) base::digamma(x) # shorthand
  psi1 <- function(x) psigamma(x, deriv = 1)
  psi2 <- function(x) psigamma(x, deriv = 2)
  psi3 <- function(x) psigamma(x, deriv = 3)
  y <- b + 1
  term1 <- tryCatch(psi(1) - psi(y), error = function(e) NA)
  term2 <- tryCatch(psi1(1) - psi1(y), error = function(e) NA)
  term3 <- tryCatch(psi2(1) - psi2(y), error = function(e) NA)
  term4 <- tryCatch(psi3(1) - psi3(y), error = function(e) NA)

  if(anyNA(c(term1, term2, term3, term4))) return(NA)

  res <- NA
  if(k==1) res <- term_mult * term1
  if(k==2) res <- term_mult * (term1^2 + term2)
  if(k==3) res <- term_mult * (term1^3 + 3*term1*term2 + term3)
  if(k==4) res <- term_mult * (term1^4 + 6*(term1^2)*term2 + 4*term1*term3 + 3*term2^2 + term4)

  return(res)
}

# Central moments of Y = log(X)
kappa_k_logX <- function(a, b, k) {
  if(a <= 0 || b <= 0) return(NA)
  term_mult <- 1 / (a^k)
  psi1 <- function(x) psigamma(x, deriv = 1)
  psi2 <- function(x) psigamma(x, deriv = 2)
  psi3 <- function(x) psigamma(x, deriv = 3)
  y <- b + 1
  term2 <- tryCatch(psi1(1) - psi1(y), error = function(e) NA)
  term3 <- tryCatch(psi2(1) - psi2(y), error = function(e) NA)
  term4 <- tryCatch(psi3(1) - psi3(y), error = function(e) NA)

  if(anyNA(c(term2, term3, term4))) return(NA)

  res <- NA
  if(k==2) res <- term_mult * term2 # Var(logX)
  if(k==3) res <- term_mult * term3
  if(k==4) res <- term_mult * (3*term2^2 + term4)

  return(res)
}


# E[X^r]
E_Xr <- function(a, b, r) {
  if(a <= 0 || b <= 0 || r <= -a) return(NA) # Ensure 1+r/a > 0
  tryCatch(b * exp(lgamma(1 + r/a) + lgamma(b) - lgamma(1 + r/a + b)),
           warning = function(w) NA, error = function(e) NA)
}

# E[X^r log^k X]
E_XrlogkX <- function(a, b, r, k) {
  if(a <= 0 || b <= 0 || r <= -a) return(NA)
  EXr <- E_Xr(a, b, r)
  if(is.na(EXr)) return(NA)

  psi <- function(d, x) tryCatch(psigamma(x, deriv = d), error = function(e) NA)
  arg1 <- 1 + r/a
  arg2 <- 1 + r/a + b

  term1 <- tryCatch((1/a)*(psi(0, arg1) - psi(0, arg2)), error = function(e) NA) # logX part
  term2 <- tryCatch((1/a^2)*(psi(1, arg1) - psi(1, arg2)), error = function(e) NA) # logX var part

  if(anyNA(c(term1, term2))) return(NA)

  res <- NA
  if(k==1) res <- EXr * term1
  if(k==2) res <- EXr * (term1^2 + term2)
  # Higher k needed? Only k=1, 2 needed for V matrix calculation below

  return(res)
}


# --- 3. Function for 3x3 Covariance Matrix V ---
# Covariance matrix for [log(X), (log(X)-ElogX)^2, X]'

kumaraswamy_V_matrix_3x3 <- function(a, b){
  if(a <= 0 || b <= 0) return(matrix(NA, 3, 3))

  # Calculate needed moments
  mu_y <- E_logX(a, b)
  k2_y <- kappa_k_logX(a, b, 2) # Var(logX)
  k3_y <- kappa_k_logX(a, b, 3)
  k4_y <- kappa_k_logX(a, b, 4)
  mu_x <- E_X(a, b)
  mu_x2 <- E_Xr(a, b, 2)
  var_x <- mu_x2 - mu_x^2
  mu_ylogy <- E_logkX(a, b, 2) # E[Y^2] needed for E[XY^2] calc below
  mu_xy <- E_XrlogkX(a, b, 1, 1) # E[X logX]
  mu_xy2 <- E_XrlogkX(a, b, 1, 2) # E[X log^2 X]

  # Check if any moment failed
  moment_list <- c(mu_y, k2_y, k3_y, k4_y, mu_x, mu_x2, var_x, mu_ylogy, mu_xy, mu_xy2)
  if(anyNA(moment_list) || any(!is.finite(moment_list))) {
    warning("Calculation of one or more moments failed or resulted in non-finite values.")
    return(matrix(NA, 3, 3))
  }


  # Calculate elements of V
  # V[i,j] = Cov(h_i(X), h_j(X)) where h = [Y, (Y-mu_y)^2, X]
  v11 <- k2_y
  v12 <- k3_y
  v13 <- mu_xy - mu_y * mu_x
  v21 <- v12
  v22 <- k4_y - k2_y^2
  # V23 = Cov((Y-mu_y)^2, X) = E[X(Y-mu_y)^2] - E[(Y-mu_y)^2]E[X]
  # E[X(Y-mu_y)^2] = E[X(Y^2 - 2mu_y Y + mu_y^2)]
  #                 = E[XY^2] - 2mu_y E[XY] + mu_y^2 E[X]
  e_x_ymuy2 <- mu_xy2 - 2*mu_y*mu_xy + mu_y^2*mu_x
  v23 <- e_x_ymuy2 - k2_y * mu_x
  v31 <- v13
  v32 <- v23
  v33 <- var_x

  V <- matrix(c(v11, v21, v31,
                v12, v22, v32,
                v13, v23, v33), nrow = 3, ncol = 3, byrow = TRUE)

  # Add names for clarity
  mom_names <- c("E[logX]", "Var[logX]", "E[X]")
  dimnames(V) <- list(mom_names, mom_names)

  # Check for positive definiteness (optional but good)
  eigenvalues <- tryCatch(eigen(V, symmetric = TRUE, only.values = TRUE)$values, error = function(e) NA)
  if(anyNA(eigenvalues) || any(eigenvalues <= 1e-10)){
    warning("Covariance matrix V is numerically not positive definite.")
  }

  return(V)
}


# --- 4. GMM Estimator Function ---

#' Overidentified GMM for Kumaraswamy using 3 Moments
#'
#' Estimates parameters (a, b) using E[logX], Var[logX], and E[X].
#'
#' @param x_data Vector of sample data.
#' @param a_init Optional initial guess for 'a'. If NULL, uses lmom.
#' @param b_init Optional initial guess for 'b'. If NULL, uses lmom.
#' @param lower_bound Lower bound for parameters during optimization.
#' @param optim_control List of control parameters for optim.
#'
#' @return A list containing estimates, initial estimates, weighting matrix, etc.
optimal_gmm_kumaraswamy_3mom <- function(x_data,
                                         a_init = NULL, b_init = NULL,
                                         lower_bound = 1e-6,
                                         optim_control = list(maxit=500)) {

  # --- Calculate Sample Moments ---
  x_data <- x_data[x_data > 0 & x_data < 1] # Ensure data is in (0, 1)
  if(length(x_data) < 10) stop("Need at least 10 valid data points.")
  y_data <- log(x_data)

  mu_sample <- mean(y_data)
  sigma2_sample <- var(y_data)
  x_bar_sample <- mean(x_data)

  m_sample <- c(mu_sample, sigma2_sample, x_bar_sample)
  names(m_sample) <- c("E[logX]", "Var[logX]", "E[X]")

  # --- Step 1: Get Initial Estimates ---
  if (is.null(a_init) || is.null(b_init)) {
    initial_est <- lmom(mu_sample, sigma2_sample)
    if (anyNA(initial_est)) {
      stop("Failed to get valid initial estimates from lmom based on log-moments.")
    }
    a0 <- initial_est['a']
    b0 <- initial_est['b']
  } else {
    a0 <- a_init
    b0 <- b_init
    initial_est <- c(a=a0, b=b0)
    if(a0 <= 0 || b0 <= 0) stop("Provided initial estimates must be positive.")
  }
  cat("Initial estimates: a =", sprintf("%.4f", a0), ", b =", sprintf("%.4f", b0), "\n")

  # --- Step 2: Calculate Optimal Weighting Matrix ---
  V_matrix <- kumaraswamy_V_matrix_3x3(a0, b0)
  if (anyNA(V_matrix)) {
    stop("Failed to calculate 3x3 covariance matrix V at initial estimates.")
  }

  W_inv <- tryCatch({
    solve(V_matrix)
  }, error = function(e) {
    warning(paste("Covariance matrix V is singular or near-singular. Using identity matrix.\nV:\n", paste(capture.output(print(V_matrix)), collapse="\n")))
    diag(3) # Fallback to identity
  })
  if (any(!is.finite(W_inv))) {
    warning(paste("Inverse of covariance matrix V contains non-finite values. Using identity matrix.\nW_inv:\n", paste(capture.output(print(W_inv)), collapse="\n")))
    W_inv <- diag(3)
  }

  # --- Step 3: Define GMM Objective Function ---
  gmm_objective_3mom <- function(params, sample_moments, W_inv_mat) {
    a_par <- params[1]
    b_par <- params[2]

    if (a_par <= 0 || b_par <= 0) return(1e10)

    # Calculate theoretical moments at current params
    e_logx_theo <- E_logX(a_par, b_par)
    var_logx_theo <- Var_logX(a_par, b_par)
    e_x_theo <- E_X(a_par, b_par)

    # Check for valid theoretical moments
    if (anyNA(c(e_logx_theo, var_logx_theo, e_x_theo))) return(1e10)

    h_theo <- c(e_logx_theo, var_logx_theo, e_x_theo)

    # Moment conditions vector g(theta) = m_sample - h(theta)
    g <- sample_moments - h_theo

    # GMM quadratic form Q = g' * W_inv * g
    Q <- tryCatch(t(g) %*% W_inv_mat %*% g, error = function(e) 1e10)

    if (!is.finite(Q) || length(Q)!=1) return(1e10)

    return(as.numeric(Q))
  }

  # --- Step 4: Minimize Objective Function ---
  optim_result <- optim(
    par = c(a0, b0),
    fn = gmm_objective_3mom,
    sample_moments = m_sample,
    W_inv_mat = W_inv,
    method = "L-BFGS-B",
    lower = c(lower_bound, lower_bound),
    control = optim_control
  )

  # --- Step 5: Return Results ---
  estimates <- setNames(optim_result$par, c("a", "b"))

  # Calculate final moments and g vector for J-stat (optional)
  h_final <- c(E_logX(estimates[1], estimates[2]),
               Var_logX(estimates[1], estimates[2]),
               E_X(estimates[1], estimates[2]))
  g_final <- m_sample - h_final
  min_Q <- optim_result$value # Minimized objective function value
  # J_stat = n * min_Q # J-statistic for overidentification test

  return(list(
    estimates = estimates,
    initial_estimates = initial_est,
    sample_moments = m_sample,
    final_theoretical_moments = h_final,
    final_moment_conditions_g = g_final,
    minimized_Q = min_Q,
    # J_statistic = length(x_data) * min_Q, # Need original N if data was cleaned
    weighting_matrix_inv = W_inv,
    V_matrix_at_initial = V_matrix,
    optim_result = optim_result,
    convergence = optim_result$convergence
  ))
}


# --- Example Usage ---
library(extraDistr)
set.seed(101)
a_true <- 2.0
b_true <- 5.0
n <- 2000 # Sample size
x_data <- rkumar(n, a = a_true, b = b_true)

# Ensure data is strictly within (0,1) if needed
x_data <- pmax(1e-10, pmin(x_data, 1 - 1e-10))

cat("\n--- Running Overidentified GMM Estimation (3 moments) ---\n")

# Estimate using the function
gmm_result_3mom <- tryCatch({
  optimal_gmm_kumaraswamy_3mom(x_data)
}, error = function(e){
  cat("Error during GMM estimation:", e$message, "\n")
  NULL
})


if (!is.null(gmm_result_3mom)) {
  cat("\n--- GMM Results (3 moments) ---\n")
  print("GMM Estimates:")
  print(gmm_result_3mom$estimates)
  print("True Parameters:")
  print(c(a=a_true, b=b_true))
  print("Initial Estimates (from lmom):")
  print(gmm_result_3mom$initial_estimates)
  print("Sample Moments Used:")
  print(gmm_result_3mom$sample_moments)
  print("Theoretical Moments at Final Estimates:")
  print(gmm_result_3mom$final_theoretical_moments)
  print("Final Moment Condition Vector (g):")
  print(gmm_result_3mom$final_moment_conditions_g)
  print("Minimized Objective Function Value (Q):")
  print(gmm_result_3mom$minimized_Q)
  print("Convergence Code (0=success):")
  print(gmm_result_3mom$convergence)

  # Compare with lmom (2 moments)
  lmom_est <- lmom(mean(log(x_data)), var(log(x_data)))
  cat("\n--- Comparison with lmom (2 moments) ---\n")
  print("lmom Estimates:")
  print(lmom_est)
  cat("Note: GMM estimates should differ now due to the influence of E[X].\n")

  # Optional: J-statistic test for overidentifying restrictions
  J_stat <- length(x_data) * gmm_result_3mom$minimized_Q
  df_J <- length(gmm_result_3mom$sample_moments) - length(gmm_result_3mom$estimates) # 3 - 2 = 1
  p_val_J <- pchisq(J_stat, df = df_J, lower.tail = FALSE)
  cat("\n--- Overidentification Test ---\n")
  cat("J-statistic:", J_stat, "\n")
  cat("Degrees of freedom:", df_J, "\n")
  cat("P-value:", p_val_J, "\n")
  cat("(Small p-value suggests model misspecification / moments don't fit well)\n")

} else {
  cat("GMM estimation failed.\n")
}
# --- Ensure all previous helpers are loaded ---
# inverse_digamma, newton_raphson_1d, lmom
# E_logX, Var_logX, E_X
# kappa_k_logX, E_Xr, E_XrlogkX, E_logkX (needed for V)
# kumaraswamy_V_matrix_3x3


# --- 5. Jacobian Matrix D(theta) = dh/d(theta)' ---

#' Calculate Jacobian of Theoretical Moments for Kumaraswamy
#'
#' Computes the matrix D = dh/d(theta)' where h = [E[logX], Var[logX], E[X]]
#' and theta = [a, b].
#'
#' @param a Parameter a.
#' @param b Parameter b.
#' @return A 3x2 matrix of derivatives, or matrix of NAs if calculation fails.
kumaraswamy_jacobian_D <- function(a, b) {
  if(a <= 0 || b <= 0) return(matrix(NA, 3, 2))

  # Use safe versions of polygamma functions
  safe_psi <- function(k, x) {
    res <- tryCatch(psigamma(x, deriv = k), error = function(e) NA)
    if(!is.finite(res)) return(NA)
    return(res)
  }
  safe_digamma <- function(x) safe_psi(0, x)
  safe_trigamma <- function(x) safe_psi(1, x)
  safe_tetragamma <- function(x) safe_psi(2, x)

  # Calculate necessary terms
  psi1 <- safe_digamma(1)
  psib1 <- safe_digamma(b + 1)
  psi1_1 <- safe_trigamma(1)
  psi1_b1 <- safe_trigamma(b + 1)
  psi2_b1 <- safe_tetragamma(b + 1)

  # Check for NA from polygamma calls
  if(anyNA(c(psi1, psib1, psi1_1, psi1_b1, psi2_b1))) return(matrix(NA, 3, 2))

  # --- Derivatives for h1 = E[logX] ---
  h1 <- (1/a) * (psi1 - psib1)
  dh1_da <- -(1/a^2) * (psi1 - psib1) # = -h1 / a
  dh1_db <- (1/a) * (-psi1_b1)

  # --- Derivatives for h2 = Var[logX] ---
  h2 <- (1/a^2) * (psi1_1 - psi1_b1)
  dh2_da <- -(2/a^3) * (psi1_1 - psi1_b1) # = -2*h2 / a
  dh2_db <- (1/a^2) * (-psi2_b1)

  # --- Derivatives for h3 = E[X] ---
  h3 <- E_X(a, b) # Use the existing stable function
  if(is.na(h3)) return(matrix(NA, 3, 2))

  p <- 1 + 1/a
  psi_p <- safe_digamma(p)
  psi_b <- safe_digamma(b)
  psi_pb <- safe_digamma(p + b)

  if(anyNA(c(psi_p, psi_b, psi_pb))) return(matrix(NA, 3, 2))

  # Use relationship E[X] = b * Beta(p, b)
  # dE[X]/da = E[X] * [psi(p) - psi(p+b)] * dp/da
  # dp/da = -1/a^2
  dh3_da <- h3 * (psi_p - psi_pb) * (-1 / a^2)

  # dE[X]/db = Beta(p, b) + b * dBeta/db
  #          = Beta(p, b) + b * Beta(p, b) * [psi(b) - psi(p+b)]
  #          = (E[X]/b) + E[X] * [psi(b) - psi(p+b)]
  # Handle b=0 case? Parameters assumed > 0
  dh3_db <- (h3 / b) + h3 * (psi_b - psi_pb)

  # --- Construct Matrix D ---
  D_mat <- matrix(c(dh1_da, dh2_da, dh3_da,  # Column for derivatives wrt a
                    dh1_db, dh2_db, dh3_db), # Column for derivatives wrt b
                  nrow = 3, ncol = 2, byrow = FALSE) # Fill column by column

  # Check final matrix elements
  if (anyNA(D_mat) || any(!is.finite(D_mat))) {
    warning("Derivative matrix D contains non-finite values.")
    return(matrix(NA, 3, 2))
  }

  rownames(D_mat) <- c("h1=E[logX]", "h2=Var[logX]", "h3=E[X]")
  colnames(D_mat) <- c("d/da", "d/db")

  return(D_mat)
}


# --- 6. One-Step GMM Estimator Function ---

#' One-Step Optimal GMM for Kumaraswamy using 3 Moments
#'
#' Estimates parameters (a, b) using E[logX], Var[logX], and E[X]
#' via a single Newton-like update step from initial estimates.
#'
#' @param x_data Vector of sample data.
#' @param a_init Optional initial guess for 'a'. If NULL, uses lmom.
#' @param b_init Optional initial guess for 'b'. If NULL, uses lmom.
#' @param lower_bound Lower bound check for final estimates.
#'
#' @return A list containing:
#'   - estimates: The one-step estimated parameters (a1, b1).
#'   - initial_estimates: The initial parameters (a0, b0).
#'   - update_step: The calculated step (theta1 - theta0).
#'   - sample_moments: The sample moments used.
#'   - g0: Moment conditions at initial estimates g(theta0).
#'   - W: The optimal weighting matrix W used.
#'   - D0: The Jacobian matrix D(theta0).
#'   - status: Indicates success or failure reason.
#'
#' @examples
#' library(extraDistr)
#' set.seed(101) # Use same seed as before
#' a_true <- 2.0
#' b_true <- 5.0
#' n <- 2000
#' x_data <- rkumar(n, a = a_true, b = b_true)
#' x_data <- pmax(1e-10, pmin(x_data, 1 - 1e-10))
#'
#' cat("\n--- Running One-Step GMM Estimation (3 moments) ---\n")
#' onestep_result <- tryCatch({
#'     gmm_onestep_kumaraswamy_3mom(x_data)
#' }, error = function(e){
#'     cat("Error during One-Step GMM estimation:", e$message, "\n")
#'     NULL
#' })
#'
#' if (!is.null(onestep_result)) {
#'     cat("\n--- One-Step GMM Results (3 moments) ---\n")
#'     print("One-Step Estimates (a1, b1):")
#'     print(onestep_result$estimates)
#'     print("Initial Estimates (a0, b0):")
#'     print(onestep_result$initial_estimates)
#'     print("True Parameters:")
#'     print(c(a=a_true, b=b_true))
#'     print("Calculation Status:")
#'     print(onestep_result$status)
#' }
#'
#' # Compare with the fully optimized GMM from previous step
#' # gmm_result_3mom <- optimal_gmm_kumaraswamy_3mom(x_data)
#' # print("Full GMM Estimates:")
#' # print(gmm_result_3mom$estimates)
#' # cat("Note: One-step should be close to full GMM if initial estimate is good.\n")

gmm_onestep_kumaraswamy_3mom <- function(x_data,
                                         a_init = NULL, b_init = NULL,
                                         lower_bound = 1e-6) {

  # --- Calculate Sample Moments ---
  x_data <- x_data[x_data > 0 & x_data < 1]
  if(length(x_data) < 10) stop("Need at least 10 valid data points.")
  y_data <- log(x_data)
  m_sample <- c(mean(y_data), var(y_data), mean(x_data))
  names(m_sample) <- c("E[logX]", "Var[logX]", "E[X]")

  # --- Step 1: Get Initial Estimates theta0 = [a0, b0]' ---
  status <- "OK"
  if (is.null(a_init) || is.null(b_init)) {
    theta0_vec <- lmom(m_sample[1], m_sample[2])
    if (anyNA(theta0_vec)) {
      return(list(status="Failed to get initial estimates from lmom."))
    }
  } else {
    theta0_vec <- c(a=a_init, b=b_init)
    if(theta0_vec[1] <= 0 || theta0_vec[2] <= 0) {
      return(list(status="Provided initial estimates must be positive."))
    }
  }
  names(theta0_vec) <- c("a", "b")
  a0 <- theta0_vec[1]
  b0 <- theta0_vec[2]

  # --- Step 2: Calculate Components at theta0 ---

  # h(theta0)
  h0 <- c(E_logX(a0, b0), Var_logX(a0, b0), E_X(a0, b0))
  if(anyNA(h0) || any(!is.finite(h0))) {
    return(list(status="Failed to calculate theoretical moments h(theta0).", initial_estimates=theta0_vec))
  }

  # g(theta0) = m_sample - h(theta0)
  g0 <- m_sample - h0

  # V(theta0)
  V0 <- kumaraswamy_V_matrix_3x3(a0, b0)
  if(anyNA(V0) || any(!is.finite(V0))) {
    return(list(status="Failed to calculate covariance matrix V(theta0).", initial_estimates=theta0_vec, g0=g0))
  }

  # W = V(theta0)^-1
  W <- tryCatch(solve(V0), error = function(e) NA)
  if(anyNA(W) || any(!is.finite(W))) {
    return(list(status="Failed to invert covariance matrix V(theta0).", initial_estimates=theta0_vec, g0=g0, V0=V0))
  }

  # D(theta0) = dh/dtheta' | theta0
  D0 <- kumaraswamy_jacobian_D(a0, b0)
  if(anyNA(D0) || any(!is.finite(D0))) {
    return(list(status="Failed to calculate Jacobian matrix D(theta0).", initial_estimates=theta0_vec, g0=g0, V0=V0, W=W))
  }

  # --- Step 3: Calculate the Update Step ---
  # Inner part: D0' * W * D0 (should be 2x2)
  D0t_W <- tryCatch(t(D0) %*% W, error = function(e) NA)
  if(anyNA(D0t_W)) return(list(status="Failed matrix multiplication D0t*W.", initial_estimates=theta0_vec, g0=g0, W=W, D0=D0))

  G0 <- tryCatch(D0t_W %*% D0, error = function(e) NA) # G0 = D0' W D0
  if(anyNA(G0)) return(list(status="Failed matrix multiplication D0t*W*D0.", initial_estimates=theta0_vec, g0=g0, W=W, D0=D0))

  # Inverse: [D0' W D0]^-1
  G0_inv <- tryCatch(solve(G0), error = function(e) NA)
  if(anyNA(G0_inv) || any(!is.finite(G0_inv))) {
    return(list(status="Failed to invert (D0t W D0).", initial_estimates=theta0_vec, g0=g0, W=W, D0=D0, G0=G0))
  }

  # Right part: D0' * W * g0 (should be 2x1)
  D0t_W_g0 <- tryCatch(D0t_W %*% g0, error = function(e) NA)
  if(anyNA(D0t_W_g0)) return(list(status="Failed matrix multiplication D0t*W*g0.", initial_estimates=theta0_vec, g0=g0, W=W, D0=D0, G0_inv=G0_inv))


  # Full update step = G0_inv * D0t_W_g0 (should be 2x1)
  update_step <- tryCatch(G0_inv %*% D0t_W_g0, error = function(e) NA)
  if(anyNA(update_step) || any(!is.finite(update_step))) {
    return(list(status="Failed final update step calculation.", initial_estimates=theta0_vec, g0=g0, W=W, D0=D0, G0_inv=G0_inv, D0t_W_g0=D0t_W_g0))
  }


  # --- Step 4: Calculate One-Step Estimate theta1 = theta0 + update ---
  theta1_vec <- theta0_vec + update_step
  names(theta1_vec) <- c("a", "b")

  # Check validity
  if(theta1_vec[1] < lower_bound || theta1_vec[2] < lower_bound){
    warning("One-step estimate resulted in non-positive parameter(s). Check initial estimates or data.")
    status <- "Warning: Final estimate(s) non-positive."
    # Decide: return NA, return theta0, or return invalid theta1? Returning theta1 for inspection.
  }


  # --- Step 5: Return Results ---
  return(list(
    estimates = theta1_vec,
    initial_estimates = theta0_vec,
    update_step = update_step,
    sample_moments = m_sample,
    g0 = g0,
    W = W,
    D0 = D0,
    status = status
  ))
}

# --- Example Usage ---
library(extraDistr)
set.seed(101) # Use same seed as before
a_true <- 2.0
b_true <- 5.0
n <- 2000
x_data <- rkumar(n, a = a_true, b = b_true)
x_data <- pmax(1e-10, pmin(x_data, 1 - 1e-10))
x <- x_data

cat("\n--- Running One-Step GMM Estimation (3 moments) ---\n")
onestep_result <- tryCatch({
  gmm_onestep_kumaraswamy_3mom(x_data)
}, error = function(e){
  cat("Error during One-Step GMM estimation:", e$message, "\n")
  NULL
})

if (!is.null(onestep_result) && onestep_result$status %in% c("OK", "Warning: Final estimate(s) non-positive.")) {
  cat("\n--- One-Step GMM Results (3 moments) ---\n")
  print("One-Step Estimates (a1, b1):")
  print(onestep_result$estimates)
  print("Initial Estimates (a0, b0):")
  print(onestep_result$initial_estimates)
  print("True Parameters:")
  print(c(a=a_true, b=b_true))
  print("Update Step (theta1 - theta0):")
  print(onestep_result$update_step)
  print("Moment Conditions at Start (g0):")
  print(onestep_result$g0)
  print("Calculation Status:")
  print(onestep_result$status)

  # Compare with the fully optimized GMM from previous step if available
  # (Assuming gmm_result_3mom exists from previous run)
  # if(exists("gmm_result_3mom") && !is.null(gmm_result_3mom)){
  #    cat("\n--- Comparison with Full GMM ---\n")
  #    print("Full GMM Estimates:")
  #    print(gmm_result_3mom$estimates)
  #    cat("Note: One-step should be close to full GMM if initial estimate is good.\n")
  # }

} else {
  cat("One-step GMM estimation failed or encountered critical error.\n")
  if(!is.null(onestep_result)) print(paste("Status:", onestep_result$status))
}

print(lmom(mean(log(x)), var(log(x))))
# Need these globally for the nested functions
library(stats)

gmm_onestep_minimal <- function(mu, sigma2, nu, a_init, b_init) {

  # --- Nested Helper Functions ---
  E_logX_ <- function(a, b) (1/a) * (digamma(1) - digamma(b + 1))
  Var_logX_ <- function(a, b) (1/a^2) * (psigamma(1, 1) - psigamma(b + 1, 1))
  E_X_ <- function(a, b) b * exp(lgamma(1 + 1/a) + lgamma(b) - lgamma(1 + 1/a + b))

  kappa_k_logX_ <- function(a, b, k) {
    term_mult <- 1 / (a^k); y <- b + 1
    psi1_diff <- psigamma(1, 1) - psigamma(y, 1) # Needed for k=2 and k=4
    if(k==2) return(term_mult * psi1_diff)
    if(k==3) return(term_mult * (psigamma(1, 2) - psigamma(y, 2)))
    if(k==4) {
      psi3_diff <- psigamma(1, 3) - psigamma(y, 3)
      return(term_mult * (3*psi1_diff^2 + psi3_diff))
    }
  }
  E_Xr_ <- function(a, b, r) b * exp(lgamma(1 + r/a) + lgamma(b) - lgamma(1 + r/a + b))
  E_logkX_ <- function(a, b, k) {
    term_mult <- 1 / (a^k); y <- b + 1
    psi0_diff <- digamma(1) - digamma(y)
    if(k==1) return(term_mult * psi0_diff)
    if(k==2) {
      psi1_diff <- psigamma(1, 1) - psigamma(y, 1)
      return(term_mult * (psi0_diff^2 + psi1_diff))
    }
  }
  E_XrlogkX_ <- function(a, b, r, k) {
    EXr <- E_Xr_(a, b, r)
    arg1 <- 1 + r/a; arg2 <- 1 + r/a + b
    term1 <- (1/a)*(digamma(arg1) - digamma(arg2))
    if(k==1) return(EXr * term1)
    if(k==2) {
      term2 <- (1/a^2)*(psigamma(arg1, 1) - psigamma(arg2, 1))
      return(EXr * (term1^2 + term2))
    }
  }

  V_matrix_ <- function(a, b){
    mu_y <- E_logX_(a, b)
    k2_y <- kappa_k_logX_(a, b, 2)
    k3_y <- kappa_k_logX_(a, b, 3)
    k4_y <- kappa_k_logX_(a, b, 4)
    mu_x <- E_X_(a, b)
    mu_x2 <- E_Xr_(a, b, 2)
    mu_xy <- E_XrlogkX_(a, b, 1, 1)
    mu_xy2 <- E_XrlogkX_(a, b, 1, 2)

    v11 <- k2_y
    v12 <- k3_y
    v13 <- mu_xy - mu_y * mu_x
    v22 <- k4_y - k2_y^2
    v23 <- (mu_xy2 - 2*mu_y*mu_xy + mu_y^2*mu_x) - k2_y * mu_x
    v33 <- mu_x2 - mu_x^2

    # Construct matrix directly
    matrix(c(v11, v12, v13, v12, v22, v23, v13, v23, v33), 3, 3)
  }

  jacobian_D_ <- function(a, b) {
    psi1_diff <- digamma(1) - digamma(b + 1)
    psi11_diff <- psigamma(1, 1) - psigamma(b + 1, 1)
    psi1_b1 <- psigamma(b + 1, 1)
    psi2_b1 <- psigamma(b + 1, 2)

    h1 <- (1/a) * psi1_diff
    dh1_da <- -h1 / a
    dh1_db <- -psi1_b1 / a

    h2 <- (1/a^2) * psi11_diff
    dh2_da <- -2*h2 / a
    dh2_db <- -psi2_b1 / (a^2)

    h3 <- E_X_(a, b)
    p <- 1 + 1/a
    psi_p_pb_diff <- digamma(p) - digamma(p + b)
    dh3_da <- h3 * psi_p_pb_diff * (-1 / a^2)
    dh3_db <- (h3 / b) + h3 * (digamma(b) - digamma(p + b))

    # Construct matrix directly
    matrix(c(dh1_da, dh2_da, dh3_da, dh1_db, dh2_db, dh3_db), 3, 2)
  }

  jacobian_D_logparams_ <- function(alpha, beta) {
    a <- exp(alpha)
    b <- exp(beta)
    D_orig <- jacobian_D_(a, b)
    D_orig[, 1] <- D_orig[, 1] * a
    D_orig[, 2] <- D_orig[, 2] * b
    D_orig
  }

  # --- Main Calculation ---
  a0 <- a_init
  b0 <- b_init
  alpha0 <- log(a0)
  beta0 <- log(b0)
  theta0_log <- c(alpha0, beta0)

  m_sample <- c(mu, sigma2, nu)

  h0 <- c(E_logX_(a0, b0), Var_logX_(a0, b0), E_X_(a0, b0))
  g0 <- m_sample - h0
  V0 <- V_matrix_(a0, b0)
  W <- solve(V0)
  D0_log <- jacobian_D_logparams_(alpha0, beta0)

  D0t_W <- t(D0_log) %*% W
  G0_log <- D0t_W %*% D0_log
  G0_log_inv <- solve(G0_log)
  D0t_W_g0 <- D0t_W %*% g0
  update_step_log <- G0_log_inv %*% D0t_W_g0

  theta1_log <- theta0_log + update_step_log
  estimates_ab <- exp(theta1_log)
  estimates_ab
  #setNames(estimates_ab, c("a", "b"))
}

lmom <- function(mu, sigma2) {
  neuler <- digamma(1) # Negative Euler-Mascheroni constant
  basel <- pi^2 / 6 # trigamma(1)

  f_over_df <- function(a) {
    b <- inverse_digamma(neuler - a * mu)
    tri <- trigamma(b)
    tetra <- psigamma(b, deriv = 2)

    f <- a * tri * (basel - tri - a^2 * sigma2)
    df <- a * tetra * mu - 2 * tri * (basel + tri)
    f / df
  }

  a <- newton_raphson_1d(f_over_df, sqrt(basel / sigma2))
  b <- inverse_digamma(neuler - a * mu) - 1
  c(a=a, b=b)
}
# install.packages("numDeriv")  # if you don't already have it
library(numDeriv)

# 1) Covariance matrix of the three sample moments
cov_sample_moments_kumar <- function(a, b, n = NULL) {
  # polygamma: psigamma(x, m) is the m-th derivative of digamma
  Δ1 <- digamma(1) - digamma(1 + b)
  Δ2 <- trigamma(1) - trigamma(1 + b)
  Δ3 <- psigamma(1, 2) - psigamma(1 + b, 2)
  Δ4 <- psigamma(1, 3) - psigamma(1 + b, 3)

  E_lnX    <-     Δ1  / a
  E_lnX2   <- (Δ1^2 + Δ2) / a^2
  μX       <-      b * beta(1 + 1/a, b)

  t1 <- digamma(1 + 1/a) - digamma(1 + 1/a + b)
  E_XlnX   <- (1/a) * μX * t1

  t2 <- trigamma(1 + 1/a) - trigamma(1 + 1/a + b)
  E_XlnX2  <- (1/a^2) * μX * (t1^2 + t2)

  # variances & covariances
  σ11 <-      Δ2 / a^2
  σ22 <- (1/a^4)*(Δ1^4 + 6*Δ1^2*Δ2 + 3*Δ2^2 + 4*Δ1*Δ3 + Δ4) - E_lnX2^2
  σ33 <-  b * beta(1 + 2/a, b) - μX^2

  σ12 <- (1/a^3)*(Δ1^3 + 3*Δ1*Δ2 + Δ3) - E_lnX * E_lnX2
  σ13 <-        E_XlnX - E_lnX  * μX
  σ23 <-       E_XlnX2 - E_lnX2 * μX

  matrix(c(
    σ11, σ12, σ13,
    σ12, σ22, σ23,
    σ13, σ23, σ33
  ), nrow = 3, byrow = TRUE)

}

# 2) One‐step GMM update for (a,b)
one_step_gmm_kumar <- function(a0, b0, mu, mu2, nu) {
  m_theoretical <- function(theta) {
    a <- theta[1]; b <- theta[2]
    Δ1 <- digamma(1) - digamma(1 + b)
    Δ2 <- trigamma(1) - trigamma(1 + b)
    E1 <-      Δ1  / a
    E2 <- (Δ1^2 + Δ2) / a^2
    E3 <- b * beta(1 + 1/a, b)
    c(E1, E2, E3)
  }
  cov_sample_moments_kumar <- function(a, b, n = NULL) {
    # polygamma: psigamma(x, m) is the m-th derivative of digamma
    Δ1 <- digamma(1) - digamma(1 + b)
    Δ2 <- trigamma(1) - trigamma(1 + b)
    Δ3 <- psigamma(1, 2) - psigamma(1 + b, 2)
    Δ4 <- psigamma(1, 3) - psigamma(1 + b, 3)

    E_lnX    <-     Δ1  / a
    E_lnX2   <- (Δ1^2 + Δ2) / a^2
    μX       <-      b * beta(1 + 1/a, b)

    t1 <- digamma(1 + 1/a) - digamma(1 + 1/a + b)
    E_XlnX   <- (1/a) * μX * t1

    t2 <- trigamma(1 + 1/a) - trigamma(1 + 1/a + b)
    E_XlnX2  <- (1/a^2) * μX * (t1^2 + t2)

    # variances & covariances
    σ11 <-      Δ2 / a^2
    σ22 <- (1/a^4)*(Δ1^4 + 6*Δ1^2*Δ2 + 3*Δ2^2 + 4*Δ1*Δ3 + Δ4) - E_lnX2^2
    σ33 <-  b * beta(1 + 2/a, b) - μX^2

    σ12 <- (1/a^3)*(Δ1^3 + 3*Δ1*Δ2 + Δ3) - E_lnX * E_lnX2
    σ13 <-        E_XlnX - E_lnX  * μX
    σ23 <-       E_XlnX2 - E_lnX2 * μX

    matrix(c(
      σ11, σ12, σ13,
      σ12, σ22, σ23,
      σ13, σ23, σ33
    ), nrow = 3, byrow = TRUE)

  }
  jacobian_kumar <- function(a, b) {
    # polygammas: trigamma = psigamma( ,1), etc.
    Δ1 <- digamma(1) - digamma(1 + b)
    Δ2 <- trigamma(1) - trigamma(1 + b)
    ψ1b <- trigamma(1 + b)
    ψ2b <- psigamma(1 + b, 2)

    # helpers
    N2  <- Δ1^2 + Δ2
    x   <- 1 + 1/a
    B_x <- beta(x, b)            # = B(1+1/a, b)
    muX <- b * B_x

    # derivatives
    dm1_da <- -Δ1 / a^2
    dm1_db <- -ψ1b / a

    dm2_da <- -2 * N2 / a^3
    dm2_db <- -(2*Δ1*ψ1b + ψ2b) / a^2

    dm3_da <- - muX * (digamma(x) - digamma(x + b)) / a^2
    dm3_db <- B_x * (1 + b * (digamma(b) - digamma(x + b)))

    # assemble 3×2
    matrix(c(
      dm1_da, dm1_db,
      dm2_da, dm2_db,
      dm3_da, dm3_db
    ), nrow = 3, byrow = TRUE,
    dimnames = list(
      c("d m1","d m2","d m3"),
      c("∂/∂a","∂/∂b")
    ))
  }

  θ0 <- c(a0, b0)
  m_hat <- c(mu, mu2, nu)
  m0 <- m_theoretical(θ0)
  g <- m_hat - m0

  Σ0 <- cov_sample_moments_kumar(a0, b0)
  W <- solve(Σ0)
  D <- jacobian_kumar(a0, b0)

  M <- solve(t(D) %*% W %*% D)
  θ1 <- θ0 + M %*% t(D) %*% W %*% g

  c(a = as.numeric(θ1[1]), b = as.numeric(θ1[2]))
}

one_step_gmm_kumar_log <- function(a0, b0, mu, mu2, nu) {
  # a0, b0 : initial log(a), log(b)
  # mu     : sample mean of log(x)
  # mu2    : sample mean of log(x)^2
  # nu     : sample mean of x

  # 1) back-transform
  a <- exp(a0)
  b <- exp(b0)

  # 2) common polygamma constants at 1 and at 1+b
  d1   <-      digamma(1)
  t1   <-      trigamma(1)
  p21  <-      psigamma(1, 2)
  p31  <-      psigamma(1, 3)

  d1pb <-      digamma(1 + b)
  t1pb <-      trigamma(1 + b)
  p21pb<-      psigamma(1 + b, 2)
  p31pb<-      psigamma(1 + b, 3)

  # 3) Δ’s
  D1 <- d1   - d1pb
  D2 <- t1   - t1pb
  D3 <- p21  - p21pb
  D4 <- p31  - p31pb

  # 4) theoretical moments m = (m1, m2, m3)
  m1 <- D1 / a
  m2 <- (D1^2 + D2) / a^2
  x  <- 1 + 1/a
  Bx <-     beta(x, b)
  m3 <- b * Bx

  # 5) moment residual
  g  <- c(mu, mu2, nu) - c(m1, m2, m3)

  # 6) covariance matrix of the three sample moments (up to a constant n that drops out)
  #    Var(log X) etc.
  σ11 <-    D2 / a^2
  N2  <-   D1^2 + D2
  σ22 <- (D1^4 + 6*D1^2*D2 + 3*D2^2 + 4*D1*D3 + D4)/a^4 - m2^2
  σ33 <-  b * beta(1 + 2/a, b) - m3^2

  #    cross‐moments
  σ12 <- (D1^3 + 3*D1*D2 + D3)/a^3 - m1*m2

  t1x   <- digamma(x)      - digamma(x + b)
  EXlnX <- (1/a)*m3 * t1x
  σ13   <- EXlnX - m1*m3

  t2x   <- trigamma(x)     - trigamma(x + b)
  EXlnX2<- (1/a^2)*m3*(t1x^2 + t2x)
  σ23   <- EXlnX2 - m2*m3

  Σ <- matrix(c(
    σ11, σ12, σ13,
    σ12, σ22, σ23,
    σ13, σ23, σ33
  ), nrow = 3, byrow = TRUE)

  W <- solve(Σ)   # weight matrix (n drops out in one‐step update)

  # 7) analytic Jacobian ∂m/∂φ with φ=(log a, log b)
  #    recall ∂/∂φ1 = a * ∂/∂a,  ∂/∂φ2 = b * ∂/∂b
  dm1_da <- -D1 / a^2
  dm1_db <- -t1pb / a
  J11    <- a * dm1_da    # = -D1/a
  J12    <- b * dm1_db    # = -b*t1pb/a

  dm2_da <- -2*N2 / a^3
  dm2_db <- -(2*D1*t1pb + p21pb) / a^2
  J21    <- a * dm2_da    # = -2*N2/a^2
  J22    <- b * dm2_db

  dm3_da <- -m3 * t1x / a^2
  dm3_db <-      Bx * (1 + b*(digamma(b) - digamma(x + b)))
  J31    <- a * dm3_da    # = -m3 * t1x / a
  J32    <- b * dm3_db

  Dphi <- matrix(c(
    J11, J12,
    J21, J22,
    J31, J32
  ), nrow = 3, byrow = TRUE)

  # 8) one‐step GMM in φ
  K    <- solve( t(Dphi) %*% W %*% Dphi )
  φ1   <- c(a0, b0) + K %*% t(Dphi) %*% W %*% g

  # 9) back to original scale
  c(a = exp(φ1[1]), b = exp(φ1[2]))
}


a <- 5
b <- 2

x <- extraDistr::rkumar(100, a, b)
mlkumar_(x)$iterations
#mlkumar_quad(x)$iterations
#mlkumar_log(x)$iterations

n <- length(x)
log_x <- log(x)
n_div <- 1 / n
mu <- sum(log_x) * n_div
mu2 <- sum(log_x^2) * n_div
sigma2 <- sum(log_x^2) * n_div - mu^2
nu <- sum(x) / n
par <- lmom(mu, sigma2)

kumar_lmom(mu, sigma2)
one_step_gmm_kumar(par[1], par[2], mu, mu2, nu)
one_step_gmm_kumar_log(par[1], par[2], mu, mu2, nu)
gmm_onestep_minimal(mu, sigma2, nu, par[1], par[2])
mlkumar_(x)

mlkumar_init_fisher_then_profile <- function(x,
                                             reltol = sqrt(.Machine$double.eps),
                                             iterlim = 100) {
  n     <- length(x)
  log_x <- log(x)
  mu    <- mean(log_x)

  ## 1) initial a0 by L-moments, b0 profiled
  sigma2 <- mean(log_x^2) - mu^2
  a0     <- kumar_lmom(mu, sigma2)
  xa0    <- exp(a0 * log_x)
  alpha0 <- mean(log1p(-xa0))
  b0     <- -1/alpha0

  ## 2) compute full‐info and score U at (a0,b0)
  ##   per‐obs E[m], E[γ] as above
  Em <- (digamma(2) - digamma(b0+1)) / (a0 * (b0-1))
  Eg <- b0 / (a0^2*(b0-1)*(b0-2)) *
    ((digamma(2)-digamma(b0))^2 + trigamma(2) - trigamma(b0))

  I_aa <- 1/a0^2 + (b0-1)*Eg
  I_ab <- Em
  I_bb <- 1/b0^2
  I    <- matrix(c(I_aa, I_ab, I_ab, I_bb), 2, 2)

  ## score components per obs
  z0    <- log_x/(1 - xa0)
  beta0 <- mean(xa0 * z0)
  f0    <- 1/a0 + mu + (1/alpha0 + 1)*beta0
  U     <- c(f0, 1/b0 + alpha0)

  ## 3) one Fisher step
  delta <- solve(I, U)
  a1    <- a0 + delta[1]
  b1    <- b0 + delta[2]

  ## 4) now profile-Newton on a, starting at a1:
  a   <- a1
  fdf <- function(a) {
    xa    <- exp(a * log_x)
    z     <- log_x/(1-xa)
    alpha <- mean(log1p(-xa))
    beta  <- mean(xa*z)
    gamma <- mean(xa*z^2)
    f  <- 1/a + mu + (1/alpha+1)*beta
    df <- -1/a^2 + (gamma*alpha + beta^2)/alpha^2 + gamma
    c(f,f/df)
  }
  for (i in seq_len(iterlim)) {
    step <- fdf(a)[2]
    a1   <- a - step
    if (abs(a1 - a) < abs(a)*reltol) break
    a <- a1
  }
  a_hat <- a
  b_hat <- -n / sum(log1p(-x^a_hat))
  logLik <- n*(log(a_hat) + log(b_hat) + (a_hat-1)*mu + 1/b_hat - 1)

  list(estimates  = c(a = a_hat, b = b_hat),
       logLik     = logLik,
       init_fisher = c(a0 = a1, b0 = b1),
       iterations = i)
}


## ------------------------------------------------------------------------------
## 2) Halley’s method on t = log(a)
## ------------------------------------------------------------------------------
mlkumar_halley_log <- function(x, reltol = sqrt(.Machine$double.eps), iterlim = 100) {
  n     <- length(x)
  log_x <- log(x)
  mu    <- mean(log_x)

  ## per‐obs f, f', and f'' in 'a'
  score_f2 <- function(a) {
    xa     <- exp(a * log_x)
    z      <- log_x / (1 - xa)
    alpha  <- mean(log1p(-xa))
    beta   <- mean(xa * z)
    gamma  <- mean(xa * z^2)
    ## derivative of gamma:
    gp     <- mean(xa * log_x^3 * (1 - xa^2) / (1 - xa)^4)

    f   <- 1/a + mu + (1/alpha + 1)*beta
    f1  <- -1/a^2 + (gamma*alpha + beta^2)/alpha^2 + gamma
    f2  <-  2/a^3 +
      gp*(1/alpha + 1) +
      3*beta*gamma/alpha^2 +
      2*beta^3/alpha^3
    list(f = f, f1 = f1, f2 = f2)
  }

  ## Halley‐step in t:
  ##   U  = dℓ/dt = a*f
  ##   H  = dU/dt = a^2*f' + a*f
  ##   D  = d^2U/dt^2 = a^3*f'' + 3a^2*f' + a*f
  ##   Δt =  2 U H / [2 H^2 − U D]
  step_halley_t <- function(t) {
    a  <- exp(t)
    sc <- score_f2(a)
    U  <- a   * sc$f
    H  <- a^2 * sc$f1 + a * sc$f
    D  <- a^3 * sc$f2 + 3*a^2 * sc$f1 + a * sc$f
    (2 * U * H) / (2*H^2 - U*D)
  }

  ## initialize
  sigma2 <- mean(log_x^2) - mu^2
  a0     <- kumar_lmom(mu, sigma2)
  t0     <- log(a0)

  for (i in seq_len(iterlim)) {
    dt <- step_halley_t(t0)
    t1 <- t0 - dt
    if (abs(t1 - t0) < abs(t0)*reltol) break
    t0 <- t1
  }

  a_hat <- exp(t0)
  b_hat <- -n / sum(log1p(-x^a_hat))

  list(
    estimates  = c(a = a_hat, b = b_hat),
    logLik     = n*(log(a_hat) + log(b_hat) + (a_hat-1)*mu + 1/b_hat - 1),
    iterations = i
  )
}
## 2. Quadratic‐parameterization: θ = sqrt(a)
mlkumar_quad <- function(x, reltol = sqrt(.Machine$double.eps), iterlim = 100) {
  n     <- length(x)
  log_x <- log(x)
  mu    <- mean(log_x)

  score_f <- function(a) {
    xa    <- exp(a * log_x)
    z     <- log_x / (1 - xa)
    alpha <- mean(log1p(-xa))
    beta  <- mean(xa * z)
    gamma <- mean(xa * z^2)

    f  <- 1/a + mu + (1/alpha + 1)*beta
    df <- -1/a^2 + (gamma*alpha + beta^2)/alpha^2 + gamma
    list(f = f, df = df)
  }

  ## Newton step on θ:
  ##   S(θ) = dℓ/dθ = 2θ * f(a)
  ##   H(θ) = dS/dθ = 4θ^2 f'(a) + 2 f(a)
  ## so Δθ = S/H = (2θ f) / (4θ^2 f' + 2 f) = (θ f)/(2θ^2 f' + f)
  f_over_df_theta <- function(theta) {
    a <- theta^2
    sc <- score_f(a)
    theta * sc$f / (2*theta^2 * sc$df + sc$f)
  }

  sigma2  <- mean(log_x^2) - mu^2
  a0 <- kumar_lmom(mu, sigma2)
  a0 <- if (a0 > 0) a0 else sqrt(pi^2 / (6 * sigma2))
  theta0  <- sqrt(a0)

  for (i in seq_len(iterlim)) {
    th1 <- theta0 - f_over_df_theta(theta0)
    if (abs(th1 - theta0) < abs(theta0)*reltol) break
    theta0 <- th1
  }
  a_hat <- theta0^2
  b_hat <- -n / sum(log1p(-x^a_hat))

  list(
    estimates  = c(a = a_hat, b = b_hat),
    logLik     = n*(log(a_hat) + log(b_hat) + (a_hat-1)*mu + 1/b_hat - 1),
    iterations = i
  )
}




mlkumar_profile_fisher <- function(x,
                                   reltol = sqrt(.Machine$double.eps),
                                   iterlim = 100) {
  n     <- length(x)
  log_x <- log(x)
  mu    <- mean(log_x)

  ## 1) per‐obs scoring f(a), observed f'(a)
  score_f <- function(a) {
    xa    <- exp(a * log_x)
    z     <- log_x / (1 - xa)
    alpha <- mean(log1p(-xa))
    beta  <- mean(xa * z)
    #gamma <- mean(xa * z^2)
    f  <- 1/a + mu + (1/alpha + 1)*beta
    #f1 <- -1/a^2 + (gamma*alpha + beta^2)/alpha^2 + gamma
    list(f = f, alpha = alpha)
  }

  ## 2) profile‐expected Fisher I_p(a)
  I_profile <- function(a) {
    sc    <- score_f(a)
    alpha <- sc$alpha
    b     <- -1/alpha

    ## E[m] and E[gamma]:
    Em <- (digamma(2) - digamma(b+1)) / (a * (b-1))
    Eg <- b / (a^2*(b-1)*(b-2)) *
      ((digamma(2)-digamma(b))^2 + trigamma(2) - trigamma(b))

    I_aa <- 1/a^2 + (b-1)*Eg
    I_ab <- Em
    I_bb <- 1/b^2

    I_aa - (I_ab^2)/I_bb
  }

  ## init by L‐moments
  sigma2 <- mean(log_x^2) - mu^2
  a0     <- kumar_lmom(mu, sigma2)

  for (i in seq_len(iterlim)) {
    sc <- score_f(a0)
    Ip <- I_profile(a0)
    a1 <- a0 + sc$f / Ip
    if (abs(a1 - a0) < abs(a0)*reltol) break
    a0 <- a1
  }

  a_hat <- a0
  b_hat <- -n / sum(log1p(-x^a_hat))
  logLik <- n*(log(a_hat) + log(b_hat) + (a_hat-1)*mu + 1/b_hat - 1)

  list(estimates = c(a = a_hat, b = b_hat),
       logLik     = logLik,
       iterations = i)
}

## 1. Log‐parameterization: t = log(a)
mlkumar_log <- function(x, reltol = sqrt(.Machine$double.eps), iterlim = 100) {
  n     <- length(x)
  log_x <- log(x)
  mu    <- mean(log_x)

  ## The per‐obs score in a (as before)
  score_f <- function(a) {
    xa    <- exp(a * log_x)
    z     <- log_x / (1 - xa)
    alpha <- mean(log1p(-xa))
    beta  <- mean(xa * z)
    gamma <- mean(xa * z^2)

    f  <- 1/a + mu + (1/alpha + 1)*beta
    df <- -1/a^2 + (gamma*alpha + beta^2)/alpha^2 + gamma
    list(f = f, df = df)
  }

  ## Newton step on t:
  ##   S(t)    = dℓ/dt = a * f(a)
  ##   H(t)    = dS/dt = a^2 f'(a) + a f(a)
  ## so Δt = S/H = (a f) / (a^2 f' + a f) = f / (a f' + f)
  f_over_df_t <- function(t) {
    a <- exp(t)
    sc <- score_f(a)
    sc$f / (a*sc$df + sc$f)
  }

  ## start at a₀ from L‐moments, then t₀ = log(a₀)
  sigma2 <- mean(log_x^2) - mu^2
  a0 <- kumar_lmom(mu, sigma2)
  a0 <- if (a0 > 0) a0 else sqrt(pi^2 / (6 * sigma2))
  t0     <- log(a0)

  for (i in seq_len(iterlim)) {
    t1 <- t0 - f_over_df_t(t0)
    if (abs(t1 - t0) < abs(t0)*reltol) break
    t0 <- t1
  }
  a_hat <- exp(t0)
  b_hat <- -n / sum(log1p(-x^a_hat))

  list(
    estimates  = c(a = a_hat, b = b_hat),
    logLik     = n*(log(a_hat) + log(b_hat) + (a_hat-1)*mu + 1/b_hat - 1),
    iterations = i
  )
}

## -----------------------------------------------------------------------------
## (a.1) Standard scale: 1-step profiled‐Fisher in 'a', then profile‐Newton in 'a'
## -----------------------------------------------------------------------------
mlkumar_pf_std <- function(x, reltol = sqrt(.Machine$double.eps), iterlim = 100) {
  n     <- length(x)
  log_x <- log(x)
  mu    <- mean(log_x)

  ## 1) score f(a) and profiled‐Fisher I_p(a)
  score_f <- function(a) {
    xa    <- exp(a * log_x)
    z     <- log_x/(1 - xa)
    alpha <- mean(log1p(-xa))
    beta  <- mean(xa * z)
    gamma <- mean(xa * z^2)
    f  <- 1/a + mu + (1/alpha + 1)*beta
    df <- -1/a^2 + (gamma*alpha + beta^2)/alpha^2 + gamma
    list(f = f, df = df, alpha = alpha)
  }
  I_profile <- function(a) {
    sc    <- score_f(a)
    alpha <- sc$alpha
    b     <- -1/alpha
    Em <- (digamma(2) - digamma(b+1)) / (a * (b-1))
    Eg <- b / (a^2*(b-1)*(b-2)) *
      ((digamma(2)-digamma(b))^2 + trigamma(2) - trigamma(b))
    I_aa <- 1/a^2 + (b-1)*Eg
    I_ab <- Em
    I_bb <- 1/b^2
    I_aa - I_ab^2/I_bb
  }

  ## 2) initialize by log‐L‐moments
  sigma2 <- mean(log_x^2) - mu^2
  a0     <- kumar_lmom(mu, sigma2)

  ## 3) one profiled‐Fisher step
  sc0 <- score_f(a0)
  a1  <- a0 + sc0$f / I_profile(a0)

  ## 4) refine with profile‐Newton in 'a'
  a <- a1
  for (i in seq_len(iterlim)) {
    sc <- score_f(a)
    a_new <- a - sc$f/sc$df
    if (abs(a_new - a) < abs(a)*reltol) { a <- a_new; break }
    a <- a_new
  }

  b   <- -n/ sum(log1p(-x^a))
  logLik <- n*(log(a) + log(b) + (a-1)*mu + 1/b - 1)
  list(estimates  = c(a = a, b = b),
       logLik     = logLik,
       iterations = i)
}


## -----------------------------------------------------------------------------
## (a.2) Log‐scale: 1-step profiled‐Fisher in t=log(a), then profile‐Newton in t
## -----------------------------------------------------------------------------
mlkumar_pf_log <- function(x, reltol = sqrt(.Machine$double.eps), iterlim = 100) {
  n     <- length(x)
  log_x <- log(x)
  mu    <- mean(log_x)

  ## score f(a) and I_p(a) as above
  score_f <- function(a) {
    xa    <- exp(a * log_x)
    z     <- log_x/(1 - xa)
    alpha <- mean(log1p(-xa))
    beta  <- mean(xa * z)
    gamma <- mean(xa * z^2)
    f  <- 1/a + mu + (1/alpha + 1)*beta
    df <- -1/a^2 + (gamma*alpha + beta^2)/alpha^2 + gamma
    list(f = f, df = df, alpha = alpha)
  }
  I_profile <- function(a) {
    sc    <- score_f(a)
    alpha <- sc$alpha
    b     <- -1/alpha
    Em <- (digamma(2) - digamma(b+1)) / (a * (b-1))
    Eg <- b / (a^2*(b-1)*(b-2)) *
      ((digamma(2)-digamma(b))^2 + trigamma(2) - trigamma(b))
    I_aa <- 1/a^2 + (b-1)*Eg
    I_ab <- Em
    I_bb <- 1/b^2
    I_aa - I_ab^2/I_bb
  }

  ## initialize in t
  sigma2 <- mean(log_x^2) - mu^2
  a0     <- kumar_lmom(mu, sigma2)
  t0     <- log(a0)

  ## 1-step profiled‐Fisher in t:  Δt = f(a)/(a * I_p(a))
  sc0 <- score_f(a0)
  t1  <- t0 + sc0$f / (a0 * I_profile(a0))

  ## refine via profile‐Newton in t:
  t <- t1
  for (i in seq_len(iterlim)) {
    a    <- exp(t)
    sc   <- score_f(a)
    dfdt <- a*sc$df + sc$f           # d/dt [score in t]
    ft   <- a*sc$f                   # profile–score in t
    t_new<- t - ft/dfdt
    if (abs(t_new - t) < abs(t)*reltol) { t <- t_new; break }
    t <- t_new
  }

  a <- exp(t)
  b <- -n/ sum(log1p(-x^a))
  logLik <- n*(log(a) + log(b) + (a-1)*mu + 1/b - 1)
  list(estimates  = c(a = a, b = b),
       logLik     = logLik,
       iterations = i)
}


## -----------------------------------------------------------------------------
## (a.3) Quadratic‐scale: 1-step profiled‐Fisher in θ=√a, then profile‐Newton in θ
## -----------------------------------------------------------------------------
mlkumar_pf_quad <- function(x, reltol = sqrt(.Machine$double.eps), iterlim = 100) {
  n     <- length(x)
  log_x <- log(x)
  mu    <- mean(log_x)

  ## score f(a) and I_p(a) as above
  score_f <- function(a) {
    xa    <- exp(a * log_x)
    z     <- log_x/(1 - xa)
    alpha <- mean(log1p(-xa))
    beta  <- mean(xa * z)
    gamma <- mean(xa * z^2)
    f  <- 1/a + mu + (1/alpha + 1)*beta
    df <- -1/a^2 + (gamma*alpha + beta^2)/alpha^2 + gamma
    list(f = f, df = df, alpha = alpha)
  }
  I_profile <- function(a) {
    sc    <- score_f(a)
    alpha <- sc$alpha
    b     <- -1/alpha
    Em <- (digamma(2) - digamma(b+1)) / (a * (b-1))
    Eg <- b / (a^2*(b-1)*(b-2)) *
      ((digamma(2)-digamma(b))^2 + trigamma(2) - trigamma(b))
    I_aa <- 1/a^2 + (b-1)*Eg
    I_ab <- Em
    I_bb <- 1/b^2
    I_aa - I_ab^2/I_bb
  }

  ## init in θ
  sigma2 <- mean(log_x^2) - mu^2
  a0     <- kumar_lmom(mu, sigma2)
  θ0     <- sqrt(a0)

  ## 1-step profiled‐Fisher in θ:  Δθ = (θ * f(a)) / (2 θ^2 I_p(a))
  sc0 <- score_f(a0)
  θ1  <- θ0 + θ0*sc0$f / (2*θ0^2 * I_profile(a0))

  ## refine via profile‐Newton in θ:
  θ <- θ1
  for (i in seq_len(iterlim)) {
    a    <- θ^2
    sc   <- score_f(a)
    dfdd <- 2*θ^2*sc$df + sc$f
    fth  <- 2*θ*sc$f
    θ_new<- θ - fth/dfdd
    if (abs(θ_new - θ) < abs(θ)*reltol) { θ <- θ_new; break }
    θ <- θ_new
  }

  a <- θ^2
  b <- -n/ sum(log1p(-x^a))
  logLik <- n*(log(a) + log(b) + (a-1)*mu + 1/b - 1)
  list(estimates  = c(a = a, b = b),
       logLik     = logLik,
       iterations = i)
}



## -----------------------------------------------------------------------------
## (b) Profiled‐Fisher init + Halley’s method in t=log(a)
## -----------------------------------------------------------------------------
mlkumar_pf_halley_log <- function(x, reltol = sqrt(.Machine$double.eps), iterlim = 100) {
  n     <- length(x); log_x <- log(x); mu <- mean(log_x)

  ## f, f', f'' in 'a'
  score2 <- function(a) {
    xa    <- exp(a*log_x)
    z     <- log_x/(1 - xa)
    alpha <- mean(log1p(-xa))
    beta  <- mean(xa * z)
    gamma <- mean(xa * z^2)
    ## gamma'
    gp    <- mean(xa * log_x^3 * (1 + xa)/(1 - xa)^3)
    f   <- 1/a + mu + (1/alpha+1)*beta
    f1  <- -1/a^2 + (gamma*alpha+beta^2)/alpha^2 + gamma
    f2  <- 2/a^3 + gp*(1/alpha+1) + 3*beta*gamma/alpha^2 + 2*beta^3/alpha^3
    list(f=f, f1=f1, f2=f2, alpha=alpha)
  }
  I_profile <- function(a) { ## same as above
    sc    <- score2(a); alpha<-sc$alpha; b<- -1/alpha
    Em <- (digamma(2)-digamma(b+1))/(a*(b-1))
    Eg <- b/(a^2*(b-1)*(b-2))*((digamma(2)-digamma(b))^2 + trigamma(2)-trigamma(b))
    I_aa<-1/a^2+(b-1)*Eg; I_ab<-Em; I_bb<-1/b^2
    I_aa - I_ab^2/I_bb
  }

  ## init in t
  sigma2 <- mean(log_x^2)-mu^2
  a0     <- kumar_lmom(mu, sigma2)
  t0     <- log(a0)
  sc0    <- score2(a0)
  t1     <- t0 + sc0$f/(a0 * I_profile(a0))

  ## Halley in t
  t <- t1
  for (i in seq_len(iterlim)) {
    a    <- exp(t)
    sc   <- score2(a)
    U    <- a * sc$f
    H    <- a^2*sc$f1 + a*sc$f
    D    <- a^3*sc$f2 + 3*a^2*sc$f1 + a*sc$f
    dt   <- 2*U*H/(2*H^2 - U*D)
    t_new<- t - dt
    if (abs(t_new - t) < abs(t)*reltol) { t <- t_new; break }
    t <- t_new
  }

  a <- exp(t); b <- -n/sum(log1p(-x^a))
  logLik <- n*(log(a)+log(b)+(a-1)*mu+1/b-1)
  list(estimates=c(a=a,b=b), logLik=logLik, iterations=i)
}



## -----------------------------------------------------------------------------
## (c) Profiled‐Fisher init + Halley’s method in 'a' (no transform)
## -----------------------------------------------------------------------------
mlkumar_pf_halley_std <- function(x, reltol = sqrt(.Machine$double.eps), iterlim = 100) {
  n     <- length(x); log_x <- log(x); mu <- mean(log_x)

  ## f, f', f'' in 'a' (same score2 as above)
  score2 <- function(a) {
    xa    <- exp(a*log_x)
    z     <- log_x/(1 - xa)
    alpha <- mean(log1p(-xa))
    beta  <- mean(xa * z)
    gamma <- mean(xa * z^2)
    gp    <- mean(xa * log_x^3 * (1 + xa)/(1 - xa)^3)
    f   <- 1/a + mu + (1/alpha+1)*beta
    f1  <- -1/a^2 + (gamma*alpha+beta^2)/alpha^2 + gamma
    f2  <- 2/a^3 + gp*(1/alpha+1) + 3*beta*gamma/alpha^2 + 2*beta^3/alpha^3
    list(f=f, f1=f1, f2=f2, alpha=alpha)
  }
  I_profile <- function(a) {
    sc    <- score2(a); alpha<-sc$alpha; b<- -1/alpha
    Em <- (digamma(2)-digamma(b+1))/(a*(b-1))
    Eg <- b/(a^2*(b-1)*(b-2))*((digamma(2)-digamma(b))^2 + trigamma(2)-trigamma(b))
    I_aa<-1/a^2+(b-1)*Eg; I_ab<-Em; I_bb<-1/b^2
    I_aa - I_ab^2/I_bb
  }

  ## init in a
  sigma2 <- mean(log_x^2)-mu^2
  a0     <- kumar_lmom(mu, sigma2)
  sc0    <- score2(a0)
  a1     <- a0 + sc0$f / I_profile(a0)

  ## Halley in a
  a <- a1
  for (i in seq_len(iterlim)) {
    sc <- score2(a)
    U  <- sc$f
    H  <- sc$f1
    D  <- sc$f2
    da <- 2*U*H/(2*H^2 - U*D)
    a_new <- a - da
    if (abs(a_new - a) < abs(a)*reltol) { a <- a_new; break }
    a <- a_new
  }

  b <- -n/sum(log1p(-x^a))
  logLik <- n*(log(a)+log(b)+(a-1)*mu+1/b-1)
  list(estimates=c(a=a,b=b), logLik=logLik, iterations=i)
}

mlkumar_pf_log <- function(x, reltol = sqrt(.Machine$double.eps), iterlim = 100) {
  n     <- length(x)
  log_x <- log(x)
  mu    <- mean(log_x)

  ## per‐obs score f(a) and observed derivative f'(a)
  score_f <- function(a) {
    xa    <- exp(a * log_x)
    z     <- log_x / (1 - xa)
    alpha <- mean(log1p(-xa))
    beta  <- mean(xa * z)
    gamma <- mean(xa * z^2)
    f  <- 1/a + mu + (1/alpha + 1)*beta
    df <- -1/a^2 + (gamma*alpha + beta^2)/alpha^2 + gamma
    list(f = f, df = df, alpha = alpha)
  }

  ## profile‐expected Fisher I_p(a)
  I_profile <- function(a) {
    sc    <- score_f(a)
    b     <- -1/sc$alpha
    Em <- (digamma(2) - digamma(b+1)) / (a * (b-1))
    Eg <- b / (a^2*(b-1)*(b-2)) *
      ((digamma(2)-digamma(b))^2 + trigamma(2) - trigamma(b))
    I_aa <-  1/a^2 + (b-1)*Eg
    I_ab <-  Em
    I_bb <-  1/b^2
    I_aa - I_ab^2/I_bb
  }

  ## init in t = log(a)
  sigma2 <- mean(log_x^2) - mu^2
  a0     <- kumar_lmom(mu, sigma2)
  t      <- log(a0)

  ## one profiled‐Fisher step in t:
  sc0 <- score_f(a0)
  t <- t + sc0$f / (a0 * I_profile(a0))

  ## now refine via profile‐Newton in t:
  for (i in seq_len(iterlim)) {
    a    <- exp(t)
    sc   <- score_f(a)
    U_t  <- a * sc$f                     # dℓ/dt per obs
    H_t  <- a^2 * sc$df + a * sc$f       # d²ℓ/dt² per obs
    t_new<- t - U_t / H_t
    if (abs(t_new - t) < abs(t)*reltol) { t <- t_new; break }
    t <- t_new
  }

  a <- exp(t)
  b <- -n / sum(log1p(-x^a))
  list(
    estimates  = c(a = a, b = b),
    logLik     = n*(log(a) + log(b) + (a-1)*mu + 1/b - 1),
    iterations = i
  )
}

mlkumar_pf_quad <- function(x, reltol = sqrt(.Machine$double.eps), iterlim = 100) {
  n     <- length(x)
  log_x <- log(x)
  mu    <- mean(log_x)

  score_f <- function(a) {
    xa    <- exp(a * log_x)
    z     <- log_x / (1 - xa)
    alpha <- mean(log1p(-xa))
    beta  <- mean(xa * z)
    gamma <- mean(xa * z^2)
    f  <- 1/a + mu + (1/alpha + 1)*beta
    df <- -1/a^2 + (gamma*alpha + beta^2)/alpha^2 + gamma
    list(f = f, df = df, alpha = alpha)
  }
  I_profile <- function(a) {
    sc    <- score_f(a)
    b     <- -1/sc$alpha
    Em <- (digamma(2) - digamma(b+1)) / (a * (b-1))
    Eg <- b / (a^2*(b-1)*(b-2)) *
      ((digamma(2)-digamma(b))^2 + trigamma(2) - trigamma(b))
    I_aa <-  1/a^2 + (b-1)*Eg
    I_ab <-  Em
    I_bb <-  1/b^2
    I_aa - I_ab^2/I_bb
  }

  ## init in θ = sqrt(a)
  sigma2 <- mean(log_x^2) - mu^2
  a0     <- kumar_lmom(mu, sigma2)
  θ      <- sqrt(a0)

  ## one profiled‐Fisher step in θ:
  sc0 <- score_f(a0)
  θ <- θ + (θ*sc0$f) / (2*θ^2 * I_profile(a0))

  ## refine via profile‐Newton in θ:
  for (i in seq_len(iterlim)) {
    a   <- θ^2
    sc  <- score_f(a)
    U   <- 2*θ * sc$f                   # dℓ/dθ per obs
    H   <- 4*θ^2 * sc$df + 2*sc$f       # d²ℓ/dθ² per obs
    θ_new <- θ - U / H
    if (abs(θ_new - θ) < abs(θ)*reltol) { θ <- θ_new; break }
    θ <- θ_new
  }

  a <- θ^2
  b <- -n / sum(log1p(-x^a))
  list(
    estimates  = c(a = a, b = b),
    logLik     = n*(log(a) + log(b) + (a-1)*mu + 1/b - 1),
    iterations = i
  )
}

mlkumar_pf_halley_profile <- function(x,
                                      reltol = sqrt(.Machine$double.eps),
                                      iterlim = 100) {
  n     <- length(x)
  log_x <- log(x)
  mu    <- mean(log_x)

  ## 1) Compute f(a), f''(a), and alpha
  score2 <- function(a) {
    xa    <- exp(a*log_x)
    z     <- log_x/(1 - xa)
    alpha <- mean(log1p(-xa))
    beta  <- mean(xa * z)
    gamma <- mean(xa * z^2)
    gp    <- mean(xa * log_x^3 * (1 + xa)/(1 - xa)^3)  # derivative of gamma

    f   <- 1/a + mu + (1/alpha+1)*beta
    f2  <-  2/a^3 +
      gp*(1/alpha + 1) +
      3*beta*gamma/alpha^2 +
      2*beta^3/alpha^3
    list(f = f, f2 = f2, alpha = alpha)
  }

  ## 2) Profile‐Fisher I_p(a)
  I_profile <- function(a) {
    sc    <- score2(a)
    alpha <- sc$alpha
    b     <- -1/alpha
    Em <- (digamma(2) - digamma(b+1)) / (a * (b-1))
    Eg <- b / (a^2*(b-1)*(b-2)) *
      ((digamma(2)-digamma(b))^2 + trigamma(2) - trigamma(b))
    I_aa <- 1/a^2 + (b-1)*Eg
    I_ab <- Em
    I_bb <- 1/b^2
    I_aa - I_ab^2/I_bb
  }

  ## 3) one profile‐Fisher step to prime the pump
  sigma2 <- mean(log_x^2) - mu^2
  a0     <- kumar_lmom(mu, sigma2)
  sc0    <- score2(a0)
  a      <- a0 + sc0$f / I_profile(a0)

  ## 4) Halley‐step with I_profile in place of f'
  for (i in seq_len(iterlim)) {
    sc  <- score2(a)
    f   <- sc$f
    f2  <- sc$f2
    Ip  <- I_profile(a)
    numer <- 2 * f * Ip
    denom <- 2 * Ip^2 - f * f2
    da    <- numer / denom
    a_new <- a - da
    if (abs(a_new - a) < abs(a)*reltol) { a <- a_new; break }
    a <- a_new
  }

  b <- -n / sum(log1p(-x^a))
  list(
    estimates  = c(a = a, b = b),
    logLik     = n*(log(a) + log(b) + (a-1)*mu + 1/b - 1),
    iterations = i
  )
}

## 3) profiled-Fisher init + Halley in t = log(a)
mlkumar_pf_halley_profile_log <- function(x,
                                          reltol = sqrt(.Machine$double.eps),
                                          iterlim = 100) {
  n     <- length(x)
  log_x <- log(x)
  mu    <- mean(log_x)

  score2 <- function(a) {
    xa    <- exp(a*log_x)
    z     <- log_x/(1 - xa)
    alpha <- mean(log1p(-xa))
    beta  <- mean(xa * z)
    gamma <- mean(xa * z^2)
    gp    <- mean(xa*(log_x)^3*(1+xa)/(1 - xa)^3)
    f   <- 1/a + mu + (1/alpha + 1)*beta
    f1  <- -1/a^2 + (gamma*alpha + beta^2)/alpha^2 + gamma
    f2  <- 2/a^3 + gp*(1/alpha+1) + 3*beta*gamma/alpha^2 + 2*beta^3/alpha^3
    list(f = f, f1 = f1, f2 = f2, alpha = alpha)
  }

  I_profile <- function(a) {
    sc    <- score2(a)
    b     <- -1/sc$alpha
    Em    <- (digamma(2) - digamma(b+1)) / (a*(b-1))
    Eg    <- b/(a^2*(b-1)*(b-2)) *
      ((digamma(2)-digamma(b))^2 + trigamma(2) - trigamma(b))
    I_aa  <- 1/a^2 + (b-1)*Eg
    I_ab  <- Em
    I_bb  <- 1/b^2
    I_aa - I_ab^2/I_bb
  }

  ## init + 1 profiled-Fisher step
  sigma2 <- mean(log_x^2) - mu^2
  a0     <- kumar_lmom(mu, sigma2)
  sc0    <- score2(a0)
  a      <- a0 + sc0$f / I_profile(a0)
  t      <- log(a)

  ## Halley in t
  for (i in seq_len(iterlim)) {
    a    <- exp(t)
    sc   <- score2(a)
    f    <- sc$f
    f2   <- sc$f2
    Ip   <- I_profile(a)
    U    <- a * f
    H    <- a^2*sc$f1 + U
    D    <- a^3*f2 + 3*a^2*sc$f1 + U
    dt   <- 2*U*H / (2*H^2 - U*D)
    t1   <- t - dt
    if (abs(t1 - t) < abs(t)*reltol) { t <- t1; break }
    t <- t1
  }

  a <- exp(t)
  b <- -n / sum(log1p(-x^a))
  list(estimates = c(a = a, b = b),
       logLik     = n*(log(a)+log(b)+(a-1)*mu+1/b-1),
       iterations = i)
}

mlkumar_pf_halley_profile <- function(x,
                                      reltol = sqrt(.Machine$double.eps),
                                      iterlim = 100) {
  n     <- length(x)
  lx    <- log(x)
  mu    <- mean(lx)

  ##--- core numerics: f, f', f''
  score2 <- function(a) {
    xa    <- exp(a*lx)
    z     <- lx/(1 - xa)
    alpha <- mean(log1p(-xa))
    beta  <- mean(xa * z)
    gamma <- mean(xa * z^2)
    ## true derivative of gamma:
    #gp    <- mean(xa * lx^3 * (1 + xa)/(1 - xa)^3)

    f   <- 1/a + mu + (1/alpha + 1)*beta
    #f1  <- -1/a^2 + (gamma*alpha + beta^2)/alpha^2 + gamma
    #f2  <-  2/a^3 +
    #  gp*(1/alpha + 1) +
    #  3*beta*gamma/alpha^2 +
    #  2*beta^3/alpha^3
    list(f=f, f1=f1, f2=f2, alpha=alpha)
  }

  ##--- profiled Fisher on a
  I_profile <- function(a) {
    sc    <- score2(a)
    b     <- -1/sc$alpha
    Em    <- (digamma(2)-digamma(b+1))/(a*(b-1))
    Eg    <- b/(a^2*(b-1)*(b-2)) *
      ((digamma(2)-digamma(b))^2 + trigamma(2)-trigamma(b))
    Iaa   <- 1/a^2 + (b-1)*Eg
    Iab   <- Em
    Ibb   <- 1/b^2
    Iaa - Iab^2/Ibb
  }

  ##--- initialize in t = log(a)
  sigma2 <- mean(lx^2) - mu^2
  a0     <- kumar_lmom(mu, sigma2)
  sc0    <- score2(a0)
  t      <- log(a0 + sc0$f / I_profile(a0))

  ##--- Halley on t
  for (i in seq_len(iterlim)) {
    a   <- exp(t)
    sc  <- score2(a)
    U   <- a * sc$f
    H   <- a^2 * sc$f1 + U
    D   <- a^3 * sc$f2 + 3*a^2*sc$f1 + U
    dt  <- 2*U*H / (2*H^2 - U*D)
    t1  <- t - dt
    if (abs(t1 - t) < abs(t)*reltol) { t <- t1; break }
    t  <- t1
  }

  a <- exp(t)
  b <- -n/ sum(log1p(-x^a))
  list(
    estimates  = c(a = a, b = b),
    logLik     = n*(log(a)+log(b)+(a-1)*mu + 1/b - 1),
    iterations = i
  )
}



# x <- extraDistr::rkumar(100, 2, 3)
# mlkumar_init_fisher_then_profile(x)$iterations
# mlkumar_(x)$iterations
# mlkumar_pf_halley_std(x)$iterations
# mlkumar_pf_halley_log(x)$iterations
# mlkumar_pf_std(x)$iterations
#
# #mlkumar_profile_fisher(x)$iterations
# #mlkumar_halley_log(x)$iterations
# mlkumar_quad(x)$iterations
# mlkumar_log(x)$iterations
# #mlkumar_pf_halley_std(x)$iterations
# #mlkumar_pf_halley_log(x)$iterations
# mlkumar_pf_log(x)$iterations
# mlkumar_pf_quad(x)$iterations
# mlkumar_pf_std(x)$iterations



## 2. Quadratic‐parameterization: θ = sqrt(a)
mlkumar_quad <- function(x, reltol = sqrt(.Machine$double.eps), iterlim = 100) {
  n     <- length(x)
  log_x <- log(x)
  mu    <- sum(log_x) / n

  f_over_df_theta <- function(theta) {
    a <- theta^2

    xa    <- exp(a * log_x)
    z     <- log_x / (1 - xa)
    alpha <- sum(log1p(-xa)) / n
    beta  <- sum(xa * z) / n
    gamma <- sum(xa * z^2) / n

    f  <- 1/a + mu + (1/alpha + 1)*beta
    df <- -1/a^2 + (gamma*alpha + beta^2)/alpha^2 + gamma

    theta * f / (2*theta^2 * df + f)
  }

  sigma2  <- mean(log_x^2) - mu^2
  a0 <- kumar_lmom(mu, sigma2)
  theta0  <- sqrt(a0)

  for (i in seq_len(iterlim)) {
    th1 <- theta0 - f_over_df_theta(theta0)
    if (abs(th1 - theta0) < abs(theta0)*reltol) break
    theta0 <- th1
  }

  a_hat <- theta0^2
  b_hat <- -n / sum(log1p(-x^a_hat))

  list(
    estimates  = c(a = a_hat, b = b_hat),
    logLik     = n*(log(a_hat) + log(b_hat) + (a_hat-1)*mu + 1/b_hat - 1),
    iterations = i
  )
}

mlkumar_quad2 <- function(x, reltol = sqrt(.Machine$double.eps), iterlim = 100) {
  n     <- length(x)
  log_x <- log(x)
  mu    <- mean(log_x)

  score_f <- function(a) {
    xa    <- exp(a * log_x)
    z     <- log_x / (1 - xa)
    alpha <- mean(log1p(-xa))
    beta  <- mean(xa * z)
    gamma <- mean(xa * z^2)

    f  <- 1/a + mu + (1/alpha + 1)*beta
    df <- -1/a^2 + (gamma*alpha + beta^2)/alpha^2 + gamma
    list(f = f, df = df)
  }

  ## Newton step on θ:
  ##   S(θ) = dℓ/dθ = 2θ * f(a)
  ##   H(θ) = dS/dθ = 4θ^2 f'(a) + 2 f(a)
  ## so Δθ = S/H = (2θ f) / (4θ^2 f' + 2 f) = (θ f)/(2θ^2 f' + f)
  f_over_df_theta <- function(theta) {
    a <- theta^2
    sc <- score_f(a)
    theta * sc$f / (2*theta^2 * sc$df + sc$f)
  }

  mu2  <- mean(log_x^2)
  sigma2  <- mean(log_x^2) - mu^2
  a0 <- kumar_lmom(mu, sigma2)
  b0 <- inverse_digamma(neuler - a0 * mu) - 1

  one_step_gmm_kumar_log(a0,b0,mu,m2,nu)

  theta0  <- sqrt(a0)

  for (i in seq_len(iterlim)) {
    th1 <- theta0 - f_over_df_theta(theta0)
    if (abs(th1 - theta0) < abs(theta0)*reltol) break
    theta0 <- th1
  }
  a_hat <- theta0^2
  b_hat <- -n / sum(log1p(-x^a_hat))

  list(
    estimates  = c(a = a_hat, b = b_hat),
    logLik     = n*(log(a_hat) + log(b_hat) + (a_hat-1)*mu + 1/b_hat - 1),
    iterations = i
  )
}






a <- 1/2
b <- 77

x <- extraDistr::rkumar(10000, a, b)
#mlkumar_init_fisher_then_profile(x)$iterations
mlkumar_(x)$iterations
mlkumar_quad(x)$iterations
mlkumar_log(x)$iterations

microbenchmark::microbenchmark(mlkumar_(x), mlkumar_quad(x), times = 10000)

