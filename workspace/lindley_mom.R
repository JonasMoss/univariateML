# --- Random Sampler Function (from previous response) ---
rlindley2_ <- function(n, alpha, theta) {
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

verify_mom_equations <- function(x, alpha, theta) {
  m1_sample <- mean(x)
  m2_sample <- mean(x^2)
  E_X_theoretical <- (alpha * theta + alpha + 1) / (theta * (1 + theta))
  E_X2_theoretical <- ((alpha + 1) * (alpha * theta + alpha + 2)) / (theta^2 * (1 + theta))
  diff_m1 <- m1_sample - E_X_theoretical
  diff_m2 <- m2_sample - E_X2_theoretical
  list(
    sample_mean = m1_sample,
    theoretical_mean = E_X_theoretical,
    difference_mean = diff_m1,
    abs_diff_mean = abs(diff_m1),

    sample_mean_sq = m2_sample,
    theoretical_mean_sq = E_X2_theoretical,
    difference_mean_sq = diff_m2,
    abs_diff_mean_sq = abs(diff_m2),

    input_alpha = alpha,
    input_theta = theta
  )
}

get_mom_cubic_estimates <- function(x) {
  n <- length(x)
  m1 <- sum(x) / n
  m2 <- sum(x^2) / n
  s2 <- m2 - m1^2

  roots <- polyroot(c(-m1 - 1, s2 - 2 * m1, 2 * s2 - m1, s2))
  theta <- Re(roots[abs(Im(roots)) < 1e-10])
  alpha <- (m1 * theta * (1 + theta) - 1) / (theta + 1)

  c(alpha = alpha,
    theta = theta,
    roots = roots)
}


# --- Simulation and Verification ---

set.seed(2023) # For reproducibility
N_sim <- 10000   # Number of datasets to simulate
sample_size <- 200 # Sample size for each dataset

set.seed(313)
# True parameter values for simulation
true_alpha <- 0.001
true_theta <- 0.001
x <- rlindley2_(n = sample_size, alpha = true_alpha, theta = true_theta)

get_mom_cubic_estimates(x)

verify_mom_equations(x, get_mom_cubic_estimates(x)[1], get_mom_cubic_estimates(x)[2])


# Store results
mom_alpha_estimates <- numeric(N_sim)
mom_theta_estimates <- numeric(N_sim)

print(paste("Running", N_sim, "simulations with sample size", sample_size))
print(paste("True parameters: alpha =", true_alpha, ", theta =", true_theta))

pb <- txtProgressBar(min = 0, max = N_sim, style = 3) # Progress bar

for (i in 1:N_sim) {
  # 1. Simulate data
  sim_data <- rlindley2_(n = sample_size, alpha = true_alpha, theta = true_theta)

  # 2. Get MoM estimates
  mom_ests <- get_mom_cubic_estimates(sim_data)

  # 3. Store results (handle potential NA from estimator)
  mom_alpha_estimates[i] <- ifelse(is.na(mom_ests["alpha"]), NA, mom_ests["alpha"])
  mom_theta_estimates[i] <- ifelse(is.na(mom_ests["theta"]), NA, mom_ests["theta"])

  setTxtProgressBar(pb, i)
}
close(pb)

# --- Analysis of Results ---

# Remove NAs if any occurred (should be rare for valid simulation parameters)
valid_indices <- !is.na(mom_alpha_estimates) & !is.na(mom_theta_estimates)
n_valid <- sum(valid_indices)
print(paste("Number of valid estimates obtained:", n_valid, "out of", N_sim))

if (n_valid > 0) {
  mom_alpha_valid <- mom_alpha_estimates[valid_indices]
  mom_theta_valid <- mom_theta_estimates[valid_indices]

  # Check the average estimates (should be close to true values for large N_sim)
  mean_mom_alpha <- mean(mom_alpha_valid)
  mean_mom_theta <- mean(mom_theta_valid)

  print("--- Verification ---")
  print(paste("True alpha:", true_alpha))
  print(paste("Mean MoM alpha estimate:", round(mean_mom_alpha, 4)))
  print(paste("Bias alpha:", round(mean_mom_alpha - true_alpha, 4)))

  print(paste("True theta:", true_theta))
  print(paste("Mean MoM theta estimate:", round(mean_mom_theta, 4)))
  print(paste("Bias theta:", round(mean_mom_theta - true_theta, 4)))

  # Plot histograms of the estimates
  par(mfrow=c(1,2)) # Arrange plots side-by-side

  hist(mom_alpha_valid, breaks=50, main="Histogram of MoM alpha estimates",
       xlab="alpha_MoM", probability = TRUE)
  abline(v=true_alpha, col="red", lwd=2, lty=2)
  abline(v=mean_mom_alpha, col="blue", lwd=2, lty=1)
  legend("topright", legend=c("True Value", "Mean Estimate"), col=c("red", "blue"), lwd=2, lty=c(2,1))

  hist(mom_theta_valid, breaks=50, main="Histogram of MoM theta estimates",
       xlab="theta_MoM", probability = TRUE)
  abline(v=true_theta, col="red", lwd=2, lty=2)
  abline(v=mean_mom_theta, col="blue", lwd=2, lty=1)
  legend("topright", legend=c("True Value", "Mean Estimate"), col=c("red", "blue"), lwd=2, lty=c(2,1))

  par(mfrow=c(1,1)) # Reset plot layout

} else {
  print("No valid estimates obtained in the simulation.")
}