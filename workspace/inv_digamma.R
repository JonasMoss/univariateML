x <- seq(-100, -10, length.out = 1000)
y <- sapply(x, \(x) inverse_digamma(x))

γ <- 0.57721566490153286060
ζ3  <- 1.2020569031595942854
lower_cool <- \(x) {
  t  <- x + γ
  -1/t + (pi^2/6)/t^3 + (ζ3/2)/t^4

}


plot(x, y, type = "l", log = "y", lwd = 2)
lines(x, 1 / log1p(exp(-x)), col = "red")
lines(x, -1 / (x - digamma(1)), col = "green")
lines(x, exp(x) + 0.5, col = "blue")
lines(x, lower_cool(x), col = "purple")

x <- seq(-100, 100, length.out = 1000)
y <- sapply(x, \(x) inverse_digamma(x))

x <- seq(-2.2, 30, length.out = 1000)
y <- sapply(x, \(x) inverse_digamma(x))

plot(x, abs(y-lower_cool(x)), col = "purple", log = "y")

plot(x, abs(y- 1/log1p(exp(-x))), type = "l", col = "red")
lines(x, abs(y--1/(x - digamma(1))), col = "green")
lines(x, abs(y-(exp(x) + 0.5)), col = "blue")
lines(x, abs(y-lower_cool(x)), col = "purple")


x <- seq(-20, 5, length.out = 1000)
y <- sapply(x, \(x) inverse_digamma(x))

plot(x, abs(y*log1p(exp(-x))), type = "l", col = "red")
lines(x, abs((exp(x) + 0.5)/y), col = "blue")
lines(x[x < -3.5], abs(y[x < -3.5]/lower_cool(x[x < -3.5])), col = "purple")

abs(digamma(sapply(x, \(x) inverse_digamma(x))) - x)

x <- 3
y <- sapply(x, \(x) inverse_digamma(x))


library(Rmpfr)
# --- User's inverse_digamma function ---
# This is where YOUR inverse_digamma function should be defined or loaded.
# For the sake of a runnable example, I'll create a placeholder.
# Replace this with your actual function.
#
# inverse_digamma <- function(y) {
#   # ... your implementation here ...
#   # Example: return(1/y) # This is a BAD placeholder, just for structure
#   stop("Please replace this placeholder with your actual inverse_digamma function.")
# }
#
# As you have goftest installed from the previous attempt, let's use inversePsi
# to make the example runnable.
# ** YOU SHOULD REPLACE 'goftest::inversePsi' IN THE 'check_ulp_error' CALL
# ** WITH THE NAME OF YOUR FUNCTION AS A STRING.
if (!requireNamespace("goftest", quietly = TRUE)) {
  install.packages("goftest")
}
library(goftest)
# Example: if your function is called my_inv_dg, you'd use "my_inv_dg" later.
# For now, we'll use "goftest::inversePsi" as the function to test.

# --- Alternative ULP Calculation (if Rmpfr::nextUp is not available) ---
# Calculates the ULP for a double-precision number x_val.
ulp_at_alternative <- function(x_val) {
  if (!is.finite(x_val)) return(NaN)
  if (x_val == 0) return(.Machine$double.xmin) # ULP of 0 is smallest positive subnormal

  abs_x <- abs(x_val)

  # Smallest positive normalized double
  min_normalized_double <- .Machine$double.xmin / .Machine$double.eps

  # For subnormal numbers (abs_x > 0 and abs_x < min_normalized_double)
  # ULP is fixed at .Machine$double.xmin
  if (abs_x > 0 && abs_x < min_normalized_double) {
    return(.Machine$double.xmin)
  }

  # For normalized numbers: ulp(x) = 2^(exponent(x) - (p-1))
  # p = 53 for double precision. So, ulp(x) = 2^(exponent(x) - 52)
  # exponent(x) can be found by floor(log2(abs(x)))
  # We use ldexp(1.0, exponent_val - 52) to construct this value robustly.
  # Note: R doesn't have ldexp directly, but 2^(value) works.

  # Extract binary exponent
  # log2(0) is -Inf, floor(-Inf) is -Inf. 2^(-Inf) is 0. Covered by initial x_val == 0 check.
  if (abs_x == 0) return(.Machine$double.xmin) # Should be caught, but defensive

  binary_exponent <- floor(log2(abs_x))

  # ULP is 2^(binary_exponent - 52)
  ulp_val <- 2^(binary_exponent - 52)

  # If x_val is the smallest normalized, binary_exponent = -1022.
  # ulp_val = 2^(-1022 - 52) = 2^(-1074), which is .Machine$double.xmin.
  # This seems correct. If ulp_val calculates to 0 for a non-subnormal number,
  # it implies an issue or x_val was extremely small, close to subnormal range.
  # The subnormal check should handle cases where ulp_val might become 0 due to 2^ limitations
  # for numbers that are technically normalized but very small.
  if (ulp_val == 0 && abs_x >= min_normalized_double) {
    return(.Machine$double.xmin) # Fallback for smallest normalized if 2^ calculation hit limit
  }
  return(ulp_val)
}

# --- Reference Inverse Digamma (using double-precision solver) ---
# Finds the "best" double-precision x such that stats::digamma(x) is approximately y_target.
# This serves as x_ref if high-precision Rmpfr functions are unavailable.
inv_digamma_ref_dp_solver <- function(y_target, initial_guess_x = NULL, maxit = 200, tol_x_rel = .Machine$double.eps*2) {
  y <- as.double(y_target)

  if (!is.finite(y)) return(NaN)

  # Initial guess (Bernardo, AS 103; adapted from R's qgamma and other sources)
  if (is.null(initial_guess_x)) {
    if (y >= -2.22) {
      x_curr <- exp(y) + 0.5
    } else {
      # digamma(1) = -0.5772156649015329 (from R's digamma(1))
      # For y -> -Inf, x -> 0, digamma(x) ~ -1/x, so x ~ -1/y
      # The digamma(1) term helps for moderately negative y.
      dg1 <- -0.57721566490153286060651209008240243104215933593992 # digamma(1)
      x_curr <- -1 / (y - dg1)
    }
  } else {
    x_curr <- as.double(initial_guess_x)
  }

  # Ensure initial guess is valid (positive)
  if (!is.finite(x_curr) || x_curr <= 0) {
    if (y > 100) { # Very large y, x ~ exp(y)
      x_curr <- exp(y - 0.5) # Approximation: y ~ log(x) + 1/(2x) => log(x) ~ y - 1/(2x)
      # Simpler: x ~ exp(y)
    } else if (y > 0) { # Moderately positive y
      x_curr <- exp(y) + 0.5 # Standard guess
      if (!is.finite(x_curr) || x_curr <=0 ) x_curr <- 1.5 # Fallback if exp(y) overflows or y is tiny
    } else if (y > -10) { # Moderately negative y
      x_curr <- 0.1 # Generic small positive
    } else { # Very negative y
      x_curr <- -1/y
    }
    # Final safety net for initial guess
    if (!is.finite(x_curr) || x_curr <= 0) x_curr <- 1e-6
  }


  x_prev <- x_curr + 1 # Ensure loop runs at least once

  for (i in 1:maxit) {
    if (!is.finite(x_curr) || x_curr <= 0) {
      # warning(sprintf("inv_digamma_ref_dp_solver: x became non-positive/non-finite (x=%.17g) for y=%.17g at iter %d", x_curr, y, i))
      return(x_prev) # Return previous valid x, or NaN if never valid
    }

    fx <- digamma(x_curr) - y   # Uses stats::digamma (double precision)
    dfx <- trigamma(x_curr)     # Uses stats::trigamma (double precision)

    if (!is.finite(fx) || !is.finite(dfx)) {
      #   warning(sprintf("inv_digamma_ref_dp_solver: fx or dfx non-finite for x=%.17g, y=%.17g", x_curr, y))
      return(x_curr) # Or NaN
    }
    if (dfx == 0) {
      #   warning(sprintf("inv_digamma_ref_dp_solver: Trigamma is zero at x=%.17g for y=%.17g", x_curr, y))
      return(x_curr) # Or NaN
    }

    dx <- fx / dfx

    # Check for convergence: if x_new would be indistinguishable from x_curr
    # or if the step dx is very small relatively, or fx is very small.
    # Important: check x_curr - dx == x_curr for bitwise equality
    if ((x_curr - dx) == x_curr) break
    if (abs(dx) < tol_x_rel * abs(x_curr) + .Machine$double.eps*1e-16 ) break # Relative step + absolute for small x
    if (abs(fx) < .Machine$double.eps *10) break # Function value close to zero

    x_prev <- x_curr
    x_curr <- x_curr - dx

    # Simple damping if x becomes non-positive
    if (x_curr <= 0) {
      x_curr <- (x_prev + .Machine$double.xmin) / 2.0 # Try midpoint closer to positive
      if (x_curr <=0) { # Still bad
        # warning(sprintf("inv_digamma_ref_dp_solver: Failed to keep x positive for y=%.17g. Returning last valid x=%.17g", y, x_prev))
        return(x_prev)
      }
    }
  }
  # if (i == maxit) {
  #   warning(sprintf("inv_digamma_ref_dp_solver: Max iterations (%d) reached for y=%.17g. Last dx/x=%.3e, fx=%.3e", maxit, y, dx/x_curr, fx))
  # }
  return(x_curr)
}

# --- ULP Error Checking Function ---
# 'tested_func_name' should be a string with the name of YOUR inverse_digamma function.
check_ulp_error <- function(y_val, tested_func_name) {

  # Get the function to test
  user_inv_digamma_func <- tryCatch(match.fun(tested_func_name),
                                    error = function(e) {
                                      stop(paste("Could not find the function specified by 'tested_func_name':",
                                                 tested_func_name, "\nOriginal error:", e$message))
                                    })

  # Get the reference value (best double-precision solution)
  x_ref <- inv_digamma_ref_dp_solver(y_val)

  # Get the value from the function under test
  # Ensure it's evaluated with standard double precision
  x_approx <- user_inv_digamma_func(as.double(y_val))

  if (!is.finite(x_ref) || !is.finite(x_approx)) {
    cat(sprintf("y = %-10.6g | x_ref = %-24s | x_approx = %-24s | ULP error: Non-finite result(s)\n",
                y_val, format(x_ref, digits=17, scientific=FALSE), format(x_approx, digits=17, scientific=FALSE)))
    return(invisible(list(y=y_val, x_ref=x_ref, x_approx=x_approx, ulp_val=NaN, error=NaN, ulp_error=NaN)))
  }

  # Calculate ULP at the reference value
  ulp_val <- ulp_at_alternative(x_ref) # Using the alternative ULP function

  # Absolute error
  err <- abs(x_ref - x_approx)

  # ULP error
  ulp_error <- if (ulp_val == 0) { # Should only happen if x_ref is 0 AND ulp_at returns 0
    if (err == 0) 0 else Inf
  } else {
    err / ulp_val
  }

  cat(sprintf("y = %-10.4g | x_ref = %.17g | x_approx = %.17g | ULP(x_ref) = %.2e | Error = %.2e | ULP error = %.2f\n",
              y_val, x_ref, x_approx, ulp_val, err, ulp_error))

  return(invisible(list(y=y_val, x_ref=x_ref, x_approx=x_approx, ulp_val=ulp_val, error=err, ulp_error=ulp_error)))
}

# --- How to Test YOUR function ---
# 1. Make sure YOUR inverse_digamma function is defined in your R session.
#    For example, if your function is named 'my_inverse_digamma':
#
#    my_inverse_digamma <- function(y) {
#      # ... your brilliant implementation ...
#      # e.g. return(goftest::inversePsi(y)) # for testing the test
#    }
#
# 2. Call check_ulp_error with test values and the NAME of your function as a STRING.

cat("Testing ULP error (assuming Rmpfr special functions are NOT available):\n")
cat("Using 'goftest::inversePsi' as the function_to_test for demonstration.\n")
cat("REPLACE 'goftest::inversePsi' with the name of YOUR function.\n\n")

# Define a dummy function for demonstration if you don't have your own ready
# This is just so the example runs.
my_own_inv_digamma_example <- function(y) {
  # Replace this with your actual implementation
  return(goftest::inversePsi(y)) # Using goftest for a working example
}


# Test cases for your function (replace "my_own_inv_digamma_example" with your function's name)
# function_to_test_name <- "my_own_inv_digamma_example"
function_to_test_name <- "inverse_digamma" # More direct for this example

check_ulp_error(3.0,  tested_func_name = function_to_test_name)
check_ulp_error(1.0,  tested_func_name = function_to_test_name)
check_ulp_error(0.0,  tested_func_name = function_to_test_name) # x_ref should be ~1.461632
check_ulp_error(digamma(1.0), tested_func_name = function_to_test_name) # y = -0.577..., x_ref should be ~1.0
check_ulp_error(-1.0, tested_func_name = function_to_test_name)
check_ulp_error(-10.0,tested_func_name = function_to_test_name)
check_ulp_error(-100.0,tested_func_name = function_to_test_name) # x should be very small, ~0.01
check_ulp_error(-1e6, tested_func_name = function_to_test_name)  # x should be very small, ~1e-6
check_ulp_error(10.0, tested_func_name = function_to_test_name)
check_ulp_error(100.0,tested_func_name = function_to_test_name) # x should be large, ~exp(100) + 0.5
check_ulp_error(700,  tested_func_name = function_to_test_name) # x will be very large, exp(700) overflows double

# Example of an extreme value for x (small y)
# digamma(1e-10) is approx -1e10
check_ulp_error(-1e10, tested_func_name = function_to_test_name) # Expect x approx 1e-10

cat("\n--- Notes ---\n")
cat("1. Replace '", function_to_test_name, "' with the actual name of your inverse_digamma function (as a string).\n", sep="")
cat("2. `x_ref` is the 'best' double-precision value found by a careful Newton solver using R's `digamma` and `trigamma`.\n")
cat("3. A ULP error <= 0.5 means your function's result is the correctly rounded double for `x_ref`.\n")
cat("   Errors up to a few ULPs might be acceptable depending on requirements.\n")
cat("4. If `Rmpfr`'s `psigamma(mpfr_obj)` and `nextUp(mpfr_obj)` DO work in your environment,\n")
cat("   using them for `x_ref` and `ulp_at` would provide a reference closer to the true mathematical value,\n")
cat("   rather than just the best double-precision solution relative to `stats::digamma`.\n")

# Initial "round-trip" check suggested by ChatGPT (abs error in y-domain):
# This checks abs(digamma(MY_inverse_digamma(y)) - y)
# It's a functional check, not directly ULP error of inverse_digamma(y) itself.
cat("\nOptional: Original 'round-trip' error check for functional correctness:\n")
y_values_rt <- c(3.0, 1.0, 0.0, digamma(1.0), -1.0, -10.0, 10.0, 100.0)

# Get your function
your_function_obj <- match.fun(function_to_test_name)

x_approx_vec_rt <- sapply(y_values_rt, your_function_obj)
y_roundtrip_rt <- digamma(x_approx_vec_rt) # Using base R's digamma

df_roundtrip <- data.frame(
  y_original = y_values_rt,
  x_computed_by_your_func = x_approx_vec_rt,
  y_reconstructed = y_roundtrip_rt,
  abs_error_in_y_domain = abs(y_roundtrip_rt - y_values_rt)
)
print(df_roundtrip, digits=10)