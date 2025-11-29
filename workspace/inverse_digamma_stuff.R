# --- Constants ---
PSI_1_R <- digamma(1)
PSI_P_1_R <- trigamma(1)

SQRT3_OVER_PI_R <- sqrt(3) / pi
PI_OVER_SQRT6_R <- pi / sqrt(6)
EULER_GAMMA_R <- -PSI_1_R # Euler-Mascheroni constant

U_LOW_CUTOFF_R <- -4.0
U_HIGH_CUTOFF_R <- 4.0

# --- Placeholder for your xi_1 and xi_1_prime functions ---
# You need to provide these, based on R's digamma, trigamma, psigamma
# For simplicity, I'll make dummy ones that won't actually work for real calculations
# but show the structure.
xi_1_func_r <- function(u_val) {
  # THIS IS A DUMMY - REPLACE WITH ACTUAL XI_1(u)
  if (u_val < -15) return(1.0 - (pi^2 / 3.0) * exp(2 * u_val))
  if (u_val > 20) return(-(sqrt(6)/pi) * u_val)
  # Simplified non-functional example for structure
  k <- exp(u_val)
  num <- digamma(1) - digamma(k)
  den <- sqrt(trigamma(1) + trigamma(k))
  return(num/den) # REPLACE WITH YOUR ACTUAL IMPLEMENTATION
}

xi_1_prime_func_r <- function(u_val) {
  # THIS IS A DUMMY - REPLACE WITH ACTUAL XI_1'(u)
  if (u_val < -15) return(-2.0 * (pi^2 / 3.0) * exp(2 * u_val))
  if (u_val > 20) return(-(sqrt(6)/pi))

  k <- exp(u_val)
  psi_k <- digamma(k)
  psi_p_k <- trigamma(k)
  psi_pp_k <- psigamma(k, deriv = 2) # tetragamma in R

  term_psi_p_1_plus_psi_p_k <- trigamma(1) + psi_p_k

  numerator_L_prime <- -psi_p_k * term_psi_p_1_plus_psi_p_k - 0.5 * (digamma(1) - psi_k) * psi_pp_k
  denominator_L_prime <- term_psi_p_1_plus_psi_p_k^1.5

  L_prime_k <- numerator_L_prime / denominator_L_prime
  return(L_prime_k * k) # REPLACE WITH YOUR ACTUAL IMPLEMENTATION
}


# --- Tail Approximations for Inverse (Initial Guess) ---
# For u -> -infty (y -> 1^-)
u_guess_left_tail <- function(y_target) {
  val_inside_log <- 1.0 - y_target
  # Simple guard for log
  if (val_inside_log < 1e-16) val_inside_log <- 1e-16
  return(0.5 * log(val_inside_log) + log(SQRT3_OVER_PI_R))
}

# For u -> +infty (y -> -infty)
u_guess_right_tail <- function(y_target) {
  return(-PI_OVER_SQRT6_R * y_target - EULER_GAMMA_R)
}


# --- Precompute y values at cutoffs ---
# You MUST ensure xi_1_func_r is working before these lines
Y_L_CUTOFF_R <- xi_1_func_r(U_LOW_CUTOFF_R)
Y_R_CUTOFF_R <- xi_1_func_r(U_HIGH_CUTOFF_R)


get_initial_u_guess_r <- function(y_target) {
  if (y_target > Y_L_CUTOFF_R) {
    return(u_guess_left_tail(y_target))
  } else if (y_target < Y_R_CUTOFF_R) {
    return(u_guess_right_tail(y_target))
  } else {
    # Linear interpolation for the middle
    u_guess <- U_LOW_CUTOFF_R +
      (U_HIGH_CUTOFF_R - U_LOW_CUTOFF_R) *
      (y_target - Y_L_CUTOFF_R) / (Y_R_CUTOFF_R - Y_L_CUTOFF_R)
    # Clamp guess
    return(max(U_LOW_CUTOFF_R, min(u_guess, U_HIGH_CUTOFF_R)))
  }
}


# --- Newton-Raphson Solver ---
solve_for_u_r <- function(y_target, max_iter = 30, tol = 1e-9) {
  u_n <- get_initial_u_guess_r(y_target)

  for (i in 1:max_iter) {
    xi_1_val <- xi_1_func_r(u_n)
    xi_1_prime_val <- xi_1_prime_func_r(u_n)

    f_u_n <- xi_1_val - y_target

    if (abs(f_u_n) < tol) {
      return(u_n)
    }

    if (abs(xi_1_prime_val) < 1e-12) { # Derivative too small
      # print(paste("Warning: Derivative too small at u_n =", u_n))
      return(u_n) # Stop if derivative is tiny, might be flat or stuck
    }

    u_n_plus_1 <- u_n - f_u_n / xi_1_prime_val

    # Basic check for NaN/Inf or large jumps
    if (is.nan(u_n_plus_1) || is.infinite(u_n_plus_1)) {
      # print(paste("Solver diverged (NaN/Inf) for y_target =", y_target, "at u_n =", u_n))
      return(NA) # Or u_n if a "best effort" is fine
    }
    if (abs(u_n_plus_1 - u_n) > 10 && i > 5) { # Detect very large jump after a few iterations
      # print(paste("Solver may be diverging (large step) for y_target =", y_target, "at u_n =", u_n))
      # return(u_n) # Return current u_n
    }


    u_n <- u_n_plus_1
  }

  # print(paste("Solver did not converge for y_target =", y_target, ". Last u =", u_n, "Error =", f_u_n))
  # Check final error, return u_n if close enough, otherwise NA
  final_xi_1_val <- xi_1_func_r(u_n)
  final_f_u_n <- final_xi_1_val - y_target
  if (abs(final_f_u_n) < tol * 10) return(u_n)

  return(NA)
}

# ... (keep constants and tail guess functions) ...

# You MUST have robust xi_1_func_r and xi_1_prime_func_r
# Specifically, xi_1_func_r(u) for u << 0 should accurately give a value slightly less than 1.
# And 1.0 - xi_1_func_r(u) should be accurate.

# --- Newton-Raphson Solver ---
solve_for_u_r_revised <- function(y_target, max_iter = 30, tol = 1e-9) {
  # Decide strategy based on y_target (or initial u_guess)
  # Y_L_CUTOFF_R is xi_1_func_r(-4.0). If y_target is greater, u is more negative.
  # Let's use a threshold for y_target to switch to log-transformed solve.
  # For y > Y_L_CUTOFF_R, xi_1 is very close to 1.
  # Threshold could be Y_L_CUTOFF_R, or even a bit lower like 0.95 if Y_L_CUTOFF_R itself is very stable.

  use_log_transform_solve <- (y_target > Y_L_CUTOFF_R && y_target < 1.0) # Only if y is (0,1) and close to 1

  u_n <- get_initial_u_guess_r(y_target) # Initial guess is still useful

  if (use_log_transform_solve) {
    if (y_target >= 1.0 || y_target <= 0) { # Should not happen if Y_L_CUTOFF is sane
      # print(paste("Warning: y_target is not suitable for log transform:", y_target))
      use_log_transform_solve <- FALSE
    } else {
      z_target <- log1p(-y_target)
      # Refine initial u_n for the log-transformed problem using its own tail approx:
      # z_target ~ 2*u + log(pi^2/3) => u ~ 0.5 * (z_target - log(pi^2/3))
      u_n <- 0.5 * (z_target - log(pi^2/3)) # Override previous u_n for this case
    }
  }

  for (i in 1:max_iter) {
    f_u_n_val <- NA
    f_prime_u_n_val <- NA

    if (use_log_transform_solve) {
      # We need g(u) = log(1 - xi_1(u)) and g'(u) = -xi_1'(u) / (1 - xi_1(u))
      xi_1_at_un <- xi_1_func_r(u_n)
      one_minus_xi_1 <- 1.0 - xi_1_at_un

      if (one_minus_xi_1 <= 1e-100) { # xi_1 is too close to 1 or >= 1
        # This region is tricky. Asymptotically g'(u) -> 2.
        # If xi_1_at_un is indistinguishable from 1, log(1-xi_1) is -Inf.
        # print(paste("Log-transform: 1-xi_1 too small at u_n =", u_n))
        # If u_n is already very negative, this might mean we are at the limit.
        # Fallback to asymptotic derivative if u_n is very negative
        if (u_n < -15) { # Arbitrary threshold for "very negative"
          f_u_n_val <- (2*u_n + log(pi^2/3)) - z_target # Use asymptotic g(u)
          f_prime_u_n_val <- 2.0
        } else {
          # Could try to nudge u_n or stop
          return(u_n) # Or NA
        }
      } else {
        g_u_n <- log(one_minus_xi_1)
        xi_1_prime_at_un <- xi_1_prime_func_r(u_n)

        f_u_n_val <- g_u_n - z_target
        if (abs(one_minus_xi_1) < 1e-16 && abs(xi_1_prime_at_un) < 1e-16 && u_n < -15) { # Both are tiny, use asymptotic g'(u)
          f_prime_u_n_val <- 2.0
        } else if (abs(one_minus_xi_1) < 1e-16) { # Denom too small for g'
          f_prime_u_n_val <- 2.0 # Fallback
        }
        else {
          f_prime_u_n_val <- -xi_1_prime_at_un / one_minus_xi_1
        }
      }
    } else { # Standard solve: xi_1(u) - y_target = 0
      f_u_n_val <- xi_1_func_r(u_n) - y_target
      f_prime_u_n_val <- xi_1_prime_func_r(u_n)
    }

    if (is.na(f_u_n_val) || is.na(f_prime_u_n_val)) {
      # print(paste("NA in f or f' at u_n=", u_n))
      return(NA)
    }

    if (abs(f_u_n_val) < tol) {
      return(u_n)
    }

    if (abs(f_prime_u_n_val) < 1e-12) {
      # print(paste("Warning: Derivative too small at u_n =", u_n, "f_prime=", f_prime_u_n_val))
      # If error is also small, accept. Otherwise, it's problematic.
      if (abs(f_u_n_val) < tol * 100) return(u_n)
      return(u_n) # Or NA
    }

    step <- f_u_n_val / f_prime_u_n_val
    # Simple step damping for log transform as g'(u) should be around 2
    if (use_log_transform_solve && abs(step) > 2.0) { # If step seems too large for g'(u)~2
      step <- sign(step) * min(abs(step), 2.0)
    } else if (abs(step) > 5.0 && i > 3) { # General step limiter for large steps
      step <- sign(step) * 5.0
    }

    u_n_plus_1 <- u_n - step

    if (is.nan(u_n_plus_1) || is.infinite(u_n_plus_1)) {
      # print(paste("Solver diverged (NaN/Inf) for y_target =", y_target, "at u_n =", u_n))
      return(NA)
    }

    u_n <- u_n_plus_1
  }

  # Check final error
  final_check_val <- NA
  if (use_log_transform_solve && exists("z_target")) {
    final_g_un <- tryCatch(log(1.0 - xi_1_func_r(u_n)), error = function(e) NA)
    if (!is.na(final_g_un)) final_check_val <- final_g_un - z_target
  } else {
    final_check_val <- xi_1_func_r(u_n) - y_target
  }

  if (!is.na(final_check_val) && abs(final_check_val) < tol * 10) return(u_n)

  # print(paste("Solver did not converge for y_target =", y_target, ". Last u =", u_n))
  return(NA)
}


uu <- seq(-19, 19, length.out = 10000)
yy <- sapply(uu, \(u) solve_for_u_r_revised(xi_1_func_r(u)))

# plot(yy, abs(yy - uu))
#
plot(uu, yy, type = "l")

uu <- seq(-20, Y_R_CUTOFF_R, length.out = 10000)
uu <- seq(-15, 15, length.out = 100000)
yy <- sapply(uu, \(uu) xi(exp(uu), 1))
plot(uu, sapply(yy, \(y) get_initial_u_guess_r(y)), type = "l", lwd = 2)
lines(uu, sapply(yy, \(y) solve_for_u_r_revised(y)), col = "red")

uu <- seq(-4, 4, length.out = 100000)
yy <- sapply(uu, \(uu) xi(exp(uu), 1))
f <- splinefun(x = yy, y = uu, method = "fmm")
lines(uu, f(yy), col = "blue")
#
# max(abs(yy - uu))
# which.max(abs(yy - uu))
#
# plot(yy, abs(yy - uu))

uu <- seq(-4, 4, length.out = 100000)
yy <- sapply(uu, \(uu) xi(exp(uu), 1))
#mod <- lm(uu ~ poly(yy, 10))
#lines(uu, predict(mod), col = "green")

f <- splinefun(x = yy, y = uu, method = "fmm")
lines(uu, f(yy), col = "blue")
