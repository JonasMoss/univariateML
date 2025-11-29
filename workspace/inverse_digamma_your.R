# --- YOUR inverse_digamma function ---
# Ensure YOUR function named 'inverse_digamma' is defined here or loaded
# before running the rest of the script.
#
# Example (REPLACE THIS with your actual function if it's not already loaded):
# inverse_digamma <- function(y) {
#   # This is just a placeholder to make the script runnable if your function isn't loaded.
#   # Replace with your actual implementation or ensure your 'inverse_digamma' is available.
#   if (!requireNamespace("goftest", quietly = TRUE)) {
#     stop("goftest package needed for this placeholder. Please define your own inverse_digamma.")
#   }
#   return(goftest::inversePsi(y))
# }
#
# Verify that your function exists:
if (!exists("inverse_digamma") || !is.function(inverse_digamma)) {
  stop("ERROR: Your function named 'inverse_digamma' was not found. Please define it or load it.")
}


# --- Alternative ULP Calculation ---
ulp_at_alternative <- function(x_val) {
  if (!is.finite(x_val)) return(NaN)
  if (x_val == 0) return(.Machine$double.xmin)

  abs_x <- abs(x_val)
  min_normalized_double <- .Machine$double.xmin / .Machine$double.eps

  if (abs_x > 0 && abs_x < min_normalized_double) {
    return(.Machine$double.xmin)
  }
  if (abs_x == 0) return(.Machine$double.xmin)

  binary_exponent <- floor(log2(abs_x))
  ulp_val <- 2^(binary_exponent - 52)

  if (ulp_val == 0 && abs_x >= min_normalized_double) {
    return(.Machine$double.xmin)
  }
  return(ulp_val)
}

# --- Reference Inverse Digamma (using double-precision solver) ---
inv_digamma_ref_dp_solver <- function(y_target, initial_guess_x = NULL, maxit = 200, tol_x_rel = .Machine$double.eps*2) {
  y <- as.double(y_target)
  if (!is.finite(y)) return(NaN)

  if (is.null(initial_guess_x)) {
    if (y >= -2.22) {
      x_curr <- exp(y) + 0.5
    } else {
      dg1 <- -0.57721566490153286060651209008240243104215933593992 # digamma(1)
      x_curr <- -1 / (y - dg1)
    }
  } else {
    x_curr <- as.double(initial_guess_x)
  }

  if (!is.finite(x_curr) || x_curr <= 0) {
    if (y > 100) { x_curr <- exp(y - 0.5) }
    else if (y > 0) { x_curr <- exp(y) + 0.5; if (!is.finite(x_curr) || x_curr <=0 ) x_curr <- 1.5 }
    else if (y > -10) { x_curr <- 0.1 }
    else { x_curr <- -1/y }
    if (!is.finite(x_curr) || x_curr <= 0) x_curr <- 1e-6
  }

  x_prev <- x_curr + 1 # Ensure loop runs at least once
  last_fx <- Inf

  for (i in 1:maxit) {
    if (!is.finite(x_curr) || x_curr <= 0) return(x_prev)

    fx <- digamma(x_curr) - y   # Uses stats::digamma (double precision)
    dfx <- trigamma(x_curr)     # Uses stats::trigamma (double precision)

    if (!is.finite(fx) || !is.finite(dfx)) return(x_curr)
    if (dfx == 0) return(x_curr)

    dx <- fx / dfx

    if ((x_curr - dx) == x_curr) break
    if (abs(dx) < tol_x_rel * abs(x_curr) + .Machine$double.eps*1e-16 ) break
    if (abs(fx) < .Machine$double.eps *10 || abs(fx) > abs(last_fx) * 0.999 && i > 10) {
      if (abs(fx) < .Machine$double.eps * 100) break
    }
    last_fx <- fx

    x_prev <- x_curr
    x_curr <- x_curr - dx

    if (x_curr <= 0) {
      x_curr <- (x_prev + .Machine$double.xmin) / 2.0
      if (x_curr <=0) return(x_prev)
    }
  }
  return(x_curr)
}

# --- ULP Error Checking Function (returns a list, directly calls inverse_digamma) ---
check_ulp_error_data <- function(y_val) {
  x_ref <- inv_digamma_ref_dp_solver(y_val)

  # Directly call the user's function named inverse_digamma
  x_approx <- inverse_digamma(as.double(y_val))

  if (!is.finite(x_ref) || !is.finite(x_approx)) {
    return(list(
      y = y_val,
      x_ref = x_ref,
      x_approx = x_approx,
      ulp_at_xref = NA_real_,
      abs_error = NA_real_,
      ulp_error = NA_real_,
      notes = "Non-finite result(s)"
    ))
  }

  ulp_val <- ulp_at_alternative(x_ref)
  err <- abs(x_ref - x_approx)
  ulp_error_val <- if (ulp_val == 0) {
    if (err == 0) 0 else Inf
  } else {
    err / ulp_val
  }

  return(list(
    y = y_val,
    x_ref = x_ref,
    x_approx = x_approx,
    ulp_at_xref = ulp_val,
    abs_error = err,
    ulp_error = ulp_error_val,
    notes = ""
  ))
}

# --- Function to run tests and display table ---
run_ulp_tests <- function(test_values) {
  results_list <- list()

  cat("Running ULP tests for your function: 'inverse_digamma'\n")

  for (y_val in test_values) {
    cat(".") # Progress indicator
    result_data <- check_ulp_error_data(y_val) # Directly calls the modified function
    results_list[[length(results_list) + 1]] <- result_data
  }
  cat("\nDone.\n\n")

  results_df <- do.call(rbind.data.frame, results_list)

  num_digits_high_prec <- 17
  num_digits_error   <- 2

  formatted_df <- results_df
  formatted_df$y <- sprintf("%-10.4g", results_df$y)
  formatted_df$x_ref <- sprintf("%.*g", num_digits_high_prec, results_df$x_ref)
  formatted_df$x_approx <- sprintf("%.*g", num_digits_high_prec, results_df$x_approx)
  formatted_df$ulp_at_xref <- sprintf("%.2e", results_df$ulp_at_xref)
  formatted_df$abs_error <- sprintf("%.2e", results_df$abs_error)
  formatted_df$ulp_error <- ifelse(is.na(results_df$ulp_error) | !is.finite(results_df$ulp_error),
                                   as.character(results_df$ulp_error),
                                   sprintf("%.2f", results_df$ulp_error))

  cat("ULP Error Test Results for 'inverse_digamma':\n")
  print(formatted_df, row.names = FALSE, right = FALSE)

  cat("\n--- Column Explanations ---\n")
  cat("y           : Input value to the inverse digamma function.\n")
  cat("x_ref       : Reference value of inverse_digamma(y) (best double-precision solution).\n")
  cat("x_approx    : Value from your 'inverse_digamma' function.\n")
  cat("ulp_at_xref : ULP (Unit in Last Place) of x_ref.\n")
  cat("abs_error   : Absolute error |x_ref - x_approx|.\n")
  cat("ulp_error   : ULP error = abs_error / ulp_at_xref.\n")
  cat("              (<= 0.5 is correctly rounded; 1-2 often acceptable).\n")
  cat("notes       : Any specific notes for the test case (e.g., non-finite results).\n")

  invisible(results_df)
}


# --- Define Test Values ---
test_y_values <- c(
  3.0, 1.0, 0.0,
  digamma(1.0),
  digamma(0.5),
  digamma(1.461632144968362),
  -1.0, -2.22,
  -10.0, -100.0, -1e3, -1e6, -1e10,
  10.0, 100.0, 700.0,
  1e-3, 1e-6, 1e-9,
  -1e-3, -1e-6, -1e-9,
  # Values that make digamma input very small or very large
  # x values that might cause digamma(x) to be extreme:
  digamma(1e-100), # -> very large negative y
  # digamma(1e100)   # -> very large positive y (ln(1e100) ~ 230)
  230 # Example of a large positive y
)
test_y_values <- test_y_values[is.finite(test_y_values)]
test_y_values <- unique(sort(test_y_values))


# --- Run the Tests and Display Table ---
# Make sure your 'inverse_digamma' function is defined before this line!
ulp_results <- run_ulp_tests(test_y_values)


# --- Optional: Round-Trip Error Check Table ---
cat("\n\nOptional: Round-Trip Error Check for 'inverse_digamma' (Functional Correctness):\n")

# Directly use your inverse_digamma function
x_approx_vec_rt <- sapply(test_y_values, inverse_digamma)
valid_x_for_digamma <- is.finite(x_approx_vec_rt) & (x_approx_vec_rt > 0)

y_roundtrip_rt <- rep(NA_real_, length(x_approx_vec_rt))
if(any(valid_x_for_digamma)) {
  y_roundtrip_rt[valid_x_for_digamma] <- digamma(x_approx_vec_rt[valid_x_for_digamma])
}


df_roundtrip <- data.frame(
  y_original = test_y_values,
  x_computed_by_inverse_digamma = x_approx_vec_rt,
  y_reconstructed = y_roundtrip_rt,
  abs_error_in_y_domain = abs(y_roundtrip_rt - test_y_values)
)

df_roundtrip$y_original <- sprintf("%-10.4g", df_roundtrip$y_original)
df_roundtrip$x_computed_by_inverse_digamma <- sprintf("%.6e", df_roundtrip$x_computed_by_inverse_digamma)
df_roundtrip$y_reconstructed <- sprintf("%.6e", df_roundtrip$y_reconstructed)
df_roundtrip$abs_error_in_y_domain <- sprintf("%.2e", df_roundtrip$abs_error_in_y_domain)

print(df_roundtrip, row.names = FALSE, right = FALSE)