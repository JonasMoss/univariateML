#' Density function for Z = X/(X+1) where X ~ Burr(alpha, c, k)
#'
#' @param z Vector of quantiles (values between 0 and 1).
#' @param alpha Burr scale parameter (alpha > 0).
#' @param c Burr shape parameter (c > 0).
#' @param k Burr shape parameter (k > 0).
#' @param log Logical; if TRUE, probabilities p are given as log(p).
#'
#' @return Vector of density values.
#' @export
#'
#' @examples
#' curve(dlogislogburr(x, alpha=1, c=1, k=2), from=0.001, to=0.999,
#'       main="Density (alpha=1, c=1, k=2 -> Kumaraswamy(1,2))",
#'       ylab="Density", xlab="z")
#' curve(dlogislogburr(x, alpha=2, c=1.5, k=3), from=0.001, to=0.999, add=TRUE, col="blue")
#' legend("topright", legend=c("Kumaraswamy(1,2)", "Burr(2, 1.5, 3) derived"),
#'        col=c("black", "blue"), lty=1)
dlogislogburr <- function(z, alpha, c, k, log = FALSE) {
  # --- Input Validation ---
  if (any(alpha <= 0) || length(alpha) != 1) stop("alpha must be a single positive number.")
  if (any(c <= 0) || length(c) != 1) stop("c must be a single positive number.")
  if (any(k <= 0) || length(k) != 1) stop("k must be a single positive number.")

  # --- Calculation ---
  # Initialize result vector
  res <- numeric(length(z))
  # Identify valid z values (strictly between 0 and 1)
  valid_idx <- (z > 0 & z < 1)

  # For valid z, calculate the log-density for numerical stability
  if (any(valid_idx)) {
    zv <- z[valid_idx] # Work only with valid z
    ck <- c * k

    # Log of Numerator parts
    log_num_part1 <- log(c) + log(k) + ck * log(alpha)
    log_num_part2 <- (c - 1) * log(zv)
    log_num_part3 <- (ck - 1) * log1p(-zv) # log1p(x) = log(1+x), so log(1-zv)

    # Log of Denominator part
    # Need to be careful if zv=0 or 1, but valid_idx excludes this
    term1_den <- (alpha * (1 - zv))^c
    term2_den <- zv^c
    log_den_part <- (k + 1) * log(term1_den + term2_den)

    # Combine for log density
    log_dens_valid <- log_num_part1 + log_num_part2 + log_num_part3 - log_den_part
    res[valid_idx] <- log_dens_valid
  }

  # Set log-density to -Inf for z outside (0, 1)
  res[!valid_idx] <- -Inf

  # Return log or density
  if (log) {
    return(res)
  } else {
    return(exp(res)) # exp(-Inf) is 0, which is correct
  }
}

# --- Example Visualization ---
opar <- par(mfrow=c(1,2)) # Set up plot area for two plots

# Example 1: The Kumaraswamy case
curve(dlogislogburr(x, alpha=1, c=1, k=2), from=0, to=1, n=501,
      main="Kumaraswamy Case (a=1, b=2)", ylab="Density", xlab="z",
      ylim=c(0, 2.1)) # Matches Kumaraswamy PDF 1*2*x^(1-1)*(1-x)^(2-1) = 2*(1-x)
library(VGAM) # For Kumaraswamy density check
curve(dkumar(x, shape1=1, shape2=2), from=0, to=1, add=TRUE, col="red", lty=2)
legend("topright", legend=c("Our function", "VGAM::dkumar"), col=c("black", "red"), lty=1:2)


# Example 2: A more general Burr case
alpha_val <- exp(0)
c_val <- 2
k_val <- 9
curve(dlogislogburr(x, alpha = alpha_val, c = c_val, k = k_val),
      from = 0, to = 1, n = 501,
      ylab = "Density f_Z(z)", xlab="z",
      main = paste("Derived Density for Burr\n(alpha=", alpha_val,
                   ", c=", c_val, ", k=", k_val, ")"))

par(opar) # Restore original plot settings
