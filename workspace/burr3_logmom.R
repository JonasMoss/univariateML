f <- \(k) (psigamma(1, 2) - psigamma(k, 2)) / (trigamma(1) + trigamma(k))^(3/2)

f_inv <- function(skew) {
  basel <- trigamma(1)
  napery <- psigamma(1, 2)

  f_over_df <- function(k) {
    tri <- trigamma(k)
    tetra <- psigamma(k, 2)
    penta <- psigamma(k, 3)
    base <- basel + tri
    diff <- napery - tetra

    f <- diff / base^(3/2) - skew
    df <- -(penta * base + 3/2 * diff * tetra) / base^(5/2)
    f / df
  }

  k0 <- exp(qlogis((2-skew) / (2 - napery / basel^(3/2)),
                   location = -0.3136848,
                   scale = 0.6394021))

  reltol <- .Machine$double.eps^0.5
  iterlim <- 100

  for (i in seq(iterlim)) {
    k <- k0 - f_over_df(k0)
    if (abs(k0 - k) < abs(k0)*reltol) break
    k0 <- k
  }

  k0
}





# --- Example Usage ---

# Target skewness values
skew_val_0 <- 0.0
skew_val_neg1 <- -1.0
skew_val_pos1 <- 1.0

# Find k for these skewness values
k_for_0   <- f_inv(skew_val_0)
k_for_neg1 <- f_inv(skew_val_neg1)
k_for_pos1 <- f_inv(skew_val_pos1)

print(paste("For target skewness =", skew_val_0, ", estimated k =", k_for_0))
print(paste("For target skewness =", skew_val_neg1, ", estimated k =", k_for_neg1))
print(paste("For target skewness =", skew_val_pos1, ", estimated k =", k_for_pos1))

# Verify by plugging k back into the original skewness function
# f <- function(k) { ... } # Make sure this is defined from previous steps
print(paste("Check: Skewness at k=", k_for_0, "is", f(k_for_0)))
print(paste("Check: Skewness at k=", k_for_neg1, "is", f(k_for_neg1)))
print(paste("Check: Skewness at k=", k_for_pos1, "is", f(k_for_pos1)))




estimate_burr_logmom <- function(m1, m2, m3) {
  # m1: E[log X] (mean of log data)
  # m2: Var(log X) (variance of log data)
  # m3: Mu_3(log X) (3rd central moment of log data)

  # 1. Calculate skewness from moments
  skewness_val <- m3 / m2^(3/2)

  # 2. Estimate k using the provided f_inv function
  k_hat <- f_inv(skewness_val)

  # Check if k_hat is valid
  if (is.na(k_hat) || k_hat <= 0) {
    warning("Failed to obtain a valid k estimate from f_inv.")
    return(list(alpha = NA, c = NA, k = NA))
  }

  # 3. Estimate c using the variance equation (m2) and k_hat
  psi1_1 <- trigamma(1)
  psi1_k_hat <- trigamma(k_hat)

  # Ensure trigamma is valid
  if(is.na(psi1_k_hat) || psi1_k_hat <= 0 || (psi1_1 + psi1_k_hat) <= 0 || m2 <= 0) {
    warning("Cannot calculate c due to invalid variance or trigamma results.")
    return(list(alpha = NA, c = NA, k = k_hat))
  }

  c_hat_sq <- (psi1_1 + psi1_k_hat) / m2
  c_hat <- sqrt(c_hat_sq)

  # 4. Estimate alpha using the mean equation (m1), k_hat, and c_hat
  psi_1 <- digamma(1)
  psi_k_hat <- digamma(k_hat)

  # Ensure digamma is valid
  if(is.na(psi_k_hat) || is.na(c_hat) || c_hat <= 0) {
    warning("Cannot calculate alpha due to invalid c or digamma results.")
    return(list(alpha = NA, c = c_hat, k = k_hat))
  }

  log_alpha_hat <- m1 - (1/c_hat) * (psi_1 - psi_k_hat)
  alpha_hat <- exp(log_alpha_hat)

  # Return the estimated parameters
  list(alpha = 1/alpha_hat, c = c_hat, k = k_hat)
}


# --- Example Usage with Theoretical Moments ---
# Choose some true parameters
alpha_true <- 5
c_true <- 2
k_true <- 3

# Calculate theoretical moments
true_m1 <- log(alpha_true) + (1/c_true) * (digamma(1) - digamma(k_true))
true_m2 <- (1/c_true^2) * (trigamma(1) + trigamma(k_true))
true_m3 <- (1/c_true^3) * (psigamma(1, deriv=2) - psigamma(k_true, deriv=2))

print("True Parameters:")
print(list(alpha=alpha_true, c=c_true, k=k_true))

print("Theoretical Moments:")
print(list(m1=true_m1, m2=true_m2, m3=true_m3))

# Estimate parameters from theoretical moments
estimates <- estimate_burr_logmom(true_m1, true_m2, true_m3)

print("Estimated Parameters from Moments:")
print(estimates)


# --- Example Usage with Sample Moments (from previous data) ---
set.seed(123)
x <- actuar::rburr(1000, 20, 2, 3)
log_x <- log(x)
m1 <- mean(log_x)
m2 <- var(log_x)
m3 <- mean((log_x - m1)^3)

logmom_burr3(m1,m2,m3)
estimate_burr_logmom(m1, m2, m3)

