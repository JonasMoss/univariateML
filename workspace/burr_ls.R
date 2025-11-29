#--------------------------------------------------------------------------
# R function to estimate Burr XII parameters (c, k) using Least Squares
# Based on Hossain & Nath (1997), Communications in Statistics - Theory and Methods
# Uses the LS(a) approximation: E[F(x(i))] = (i - 0.5) / n
#--------------------------------------------------------------------------

#' Estimate Burr XII Parameters using Least Squares on Transformed CDF
#'
#' Estimates the shape parameters 'c' and 'k' of a two-parameter
#' Burr Type XII distribution using the least squares method proposed
#' by Hossain & Nath (1997), specifically their LS(a) variant.
#'
#' @param x A numeric vector of positive data assumed to follow a Burr XII(c, k)
#'   distribution.
#' @param start_params A numeric vector of length 2 providing starting values
#'   for the optimization process, in the form c(c_start, k_start).
#'   If NULL, default starting values c(1, 1) are used.
#' @param optim_method The optimization method to be passed to `optim`.
#'   "L-BFGS-B" is recommended as it allows box constraints (positivity).
#' @param lower_bounds Lower bounds for c and k for L-BFGS-B method.
#' @param control Control list passed to `optim`.
#' @param warn_on_failure Logical. If TRUE, issue a warning if `optim` does not
#'   converge (convergence code != 0).
#'
#' @return A list containing:
#'   \item{estimates}{A named numeric vector with the estimated parameters 'c' and 'k'.}
#'   \item{optim_results}{The full output object from the `optim` function.}
#'   \item{data_transformed}{A list containing the sorted data `x_sorted` and the
#'                           transformed target values `y_target` used in the LS fit.}
#'
#' @details
#' The Burr XII CDF is F(x) = 1 - (1 + x^c)^-k.
#' The method transforms this relationship to:
#' log(log(1 / (1 - F(x)))) = log(k) + log(log(1 + x^c))
#'
#' It approximates F(x(i)) for the i-th order statistic x(i) using
#' p_i = (i - 0.5) / n.
#' Let y_i = log(log(1 / (1 - p_i))).
#'
#' The function then minimizes the sum of squared residuals:
#' SumSq(c, k) = Sum [ y_i - (log(k) + log(log(1 + x(i)^c))) ]^2
#' with respect to c > 0 and k > 0 using `optim`.
#'
#' @references
#' Hossain, A. M., & Nath, S. K. (1997). Estimation of parameters in the
#' presence of outliers for a burr XII distribution. Communications in
#' Statistics-Theory and Methods, 26(3), 637-652.
#'
#' @examples
#' # 1. Define a function to generate Burr XII random numbers
#' rburr12 <- function(n, c_true, k_true) {
#'   if (c_true <= 0 || k_true <= 0) {
#'     stop("Parameters c and k must be positive.")
#'   }
#'   u <- runif(n)
#'   # Inverse CDF: Q(p) = ((1-p)^(-1/k) - 1)^(1/c)
#'   x <- ((1 - u)^(-1 / k_true) - 1)^(1 / c_true)
#'   return(x)
#' }
#'
#' # 2. Generate some sample data
#' set.seed(123)
#' c_true <- 2.0
#' k_true <- 3.0
#' n_sample <- 50
#' sample_data <- rburr12(n_sample, c_true, k_true)
#'
#' # Ensure data is positive
#' sample_data <- sample_data[sample_data > 0]
#' if(length(sample_data) < 5) stop("Need more positive data points")
#'
#' # 3. Estimate parameters using the LS method
#' ls_estimate <- estimate_burr12_ls(sample_data, start_params = c(1.5, 2.5))
#'
#' # 4. View results
#' print("True parameters:")
#' print(c(c = c_true, k = k_true))
#' print("LS estimates:")
#' print(ls_estimate$estimates)
#' print("Optim convergence code (0 is success):")
#' print(ls_estimate$optim_results$convergence)
#'
#' # Example with default starting values
#' ls_estimate_default_start <- estimate_burr12_ls(sample_data)
#' print("LS estimates (default start):")
#' print(ls_estimate_default_start$estimates)
#'
estimate_burr12_ls <- function(x,
                               start_params = NULL,
                               optim_method = "L-BFGS-B",
                               lower_bounds = c(1e-6, 1e-6),
                               control = list(),
                               warn_on_failure = TRUE) {

  # --- Input validation ---
  if (!is.numeric(x) || !is.vector(x)) {
    stop("Input 'x' must be a numeric vector.")
  }
  x <- x[!is.na(x)] # Remove NAs
  if (any(x <= 0)) {
    warning("Input data 'x' contains non-positive values. These will be removed.")
    x <- x[x > 0]
  }
  n <- length(x)
  if (n < 3) {
    stop("Need at least 3 positive data points for estimation.")
  }

  # --- Prepare data ---
  x_sorted <- sort(x)
  p_i <- (1:n - 0.5) / n # Plotting positions E[F(x(i))] approx

  # Avoid p_i = 1 or p_i = 0 issues (though (i-0.5)/n prevents this)
  # Ensure arguments to log are valid
  log_arg1 <- 1 / (1 - p_i)
  if(any(log_arg1 <= 0)) stop("Internal error: Non-positive argument for outer log.")
  log_arg2 <- log(log_arg1)
  if(any(log_arg2 <= 0)) stop("Internal error: Non-positive argument for inner log.")

  y_target <- log(log_arg2) # The target values for LS

  # --- Define Objective Function (Sum of Squared Residuals) ---
  sum_sq_resid <- function(params, x_s, y_t) {
    c_par <- params[1]
    k_par <- params[2]

    # Parameter constraints check (especially if not using L-BFGS-B)
    if (c_par <= 0 || k_par <= 0) {
      return(.Machine$double.xmax) # Return a large value if params are invalid
    }

    # Calculate predicted y values based on the model
    # log(k) + log(log(1 + x^c))
    term1_plus_x_pow_c <- 1 + x_s^c_par
    # Check for potential Inf from x^c if c is large
    if(any(!is.finite(term1_plus_x_pow_c))) return(.Machine$double.xmax)

    log_inner <- log(term1_plus_x_pow_c)
    # Ensure log_inner > 0 for the outer log
    if (any(log_inner <= 0)) return(.Machine$double.xmax)

    log_outer <- log(log_inner)
    # Ensure log_outer is finite
    if (any(!is.finite(log_outer))) return(.Machine$double.xmax)

    pred_y <- log(k_par) + log_outer

    # Check if log(k) resulted in NaN/Inf
    if (any(!is.finite(pred_y))) return(.Machine$double.xmax)

    # Calculate sum of squared residuals
    ssq <- sum((y_t - pred_y)^2)

    # Check if SSq is valid
    if(!is.finite(ssq)) return(.Machine$double.xmax)

    return(ssq)
  }

  # --- Set starting parameters ---
  if (is.null(start_params)) {
    # Default starting guess - may need tuning for difficult datasets
    start_params <- c(c = 1, k = 1)
  } else {
    if (length(start_params) != 2 || !is.numeric(start_params) || any(start_params <= 0)) {
      warning("Invalid 'start_params'. Using default c(1, 1).")
      start_params <- c(c = 1, k = 1)
    } else {
      names(start_params) <- c("c", "k")
    }
  }


  # --- Perform optimization ---
  optim_results <- tryCatch({
    optim(
      par = start_params,
      fn = sum_sq_resid,
      x_s = x_sorted, # Pass data via ...
      y_t = y_target, # Pass data via ...
      method = optim_method,
      lower = lower_bounds, # Use only if method is L-BFGS-B
      control = control
    )
  }, error = function(e) {
    warning("Optimization failed with error: ", e$message)
    return(NULL)
  })

  if (is.null(optim_results)) {
    return(list(estimates = c(c = NA, k = NA), optim_results = NULL, data_transformed = list(x_sorted=x_sorted, y_target=y_target)))
  }

  # --- Check convergence and return results ---
  if (warn_on_failure && optim_results$convergence != 0) {
    warning(
      "Optimization may not have converged. Code: ",
      optim_results$convergence,
      ". Message: ",
      optim_results$message
    )
  }

  estimates <- optim_results$par
  names(estimates) <- c("c", "k") # Ensure names are set

  return(list(
    estimates = estimates,
    optim_results = optim_results,
    data_transformed = list(x_sorted=x_sorted, y_target=y_target)
  ))
}

# --- Example Usage ---

# 1. Define a function to generate Burr XII random numbers
rburr12 <- function(n, c_true, k_true) {
  if (c_true <= 0 || k_true <= 0) {
    stop("Parameters c and k must be positive.")
  }
  u <- runif(n)
  # Inverse CDF: Q(p) = ((1-p)^(-1/k) - 1)^(1/c)
  # Handle potential division by zero if k_true is very large, or ^(1/c) if c is large/small
  term1 <- (1 - u)
  # Prevent term1 becoming zero exactly, clamp small values?
  term1[term1 <= 0] <- .Machine$double.eps
  term2 <- term1^(-1 / k_true)
  term3 <- term2 - 1
  # Prevent negative base for fractional power if term2 is slightly less than 1 due to precision
  term3[term3 <= 0] <- 0 # If term2<=1, x should be 0.

  x <- term3^(1 / c_true)
  return(x)
}

# 2. Generate some sample data
set.seed(456) # Different seed
c_true <- 1.5
k_true <- 2.5
n_sample <- 100 # Larger sample
sample_data <- rburr12(n_sample, c_true, k_true)
sample_data <- sample_data[sample_data > 0 & is.finite(sample_data)] # Ensure finite and positive
if(length(sample_data) < 5) stop("Need more valid data points")


# 3. Estimate parameters using the LS method
# Try with potentially better starting values if known, otherwise use default
ls_estimate <- estimate_burr12_ls(sample_data, start_params = c(1, 1))

# 4. View results
print("--------------------------")
print("True parameters:")
print(c(c = c_true, k = k_true))
print("LS estimates:")
print(ls_estimate$estimates)
print("Optim convergence code (0 is success):")
print(ls_estimate$optim_results$convergence)
print("--------------------------")

# Optional: Compare with Maximum Likelihood Estimation (MLE) using 'fitdistrplus'
# library(fitdistrplus)
#
# # Need density and CDF functions for fitdistrplus
# dburr12 <- function(x, c, k, log = FALSE) {
#   if (c <= 0 || k <= 0) return(rep(NaN, length(x)))
#   term1 <- log(c) + log(k) + (c - 1) * log(x) - (k + 1) * log(1 + x^c)
#   if (log) return(term1) else return(exp(term1))
# }
# pburr12 <- function(q, c, k, lower.tail = TRUE, log.p = FALSE) {
#   if (c <= 0 || k <= 0) return(rep(NaN, length(q)))
#   p <- 1 - (1 + q^c)^(-k)
#   if (!lower.tail) p <- 1 - p
#   if (log.p) return(log(p)) else return(p)
# }
#
# # Ensure data is strictly positive for Burr MLE fit
# sample_data_pos <- sample_data[sample_data > 1e-10]
#
# tryCatch({
#   fit_mle <- fitdist(sample_data_pos, "burr12",
#                      start = list(c = 1, k = 1), # Provide starting values
#                      lower = c(0, 0)) # Enforce positivity
#   print("MLE estimates (using fitdistrplus):")
#   print(summary(fit_mle))
# }, error = function(e) {
#   print("MLE fitting failed:")
#   print(e$message)
# })
# print("--------------------------")