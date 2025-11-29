# --- Prerequisites ---
# 1. Make sure the 'Finv' function is loaded.
# 2. Define necessary constants.

C <- trigamma(1)
A <- psigamma(1, deriv = 2)
K <- 2 - A / C^(1.5)
c_right <- -1.5 * A / (K * C^(2.5)) # Constant for right tail 1-F(u) ~ c_right/t
const_left <- K / (6 * C)           # Constant for left tail F(u) ~ exp(2u) / const_left

# --- Asymptotic Formulas for F^{-1}(p) ---
asymp_inv_left <- function(p) {
  p_safe <- pmax(p, .Machine$double.xmin)
  0.5 * log(p_safe) + 0.5 * log(const_left)
}

asymp_inv_right <- function(p) {
  q_safe <- pmax(1 - p, .Machine$double.xmin)
  -log(q_safe) + log(c_right)
}

# --- Generate data as requested ---
y <- seq(1e-8, 1-1e-8, length.out = 1000)
# Use Finv (replace if you used a different final name)
res <- sapply(y, \(p_val) tryCatch(Finv(p_val), error = function(e) {warning(paste("Error for p=", p_val, ":", e)); NA}))

# --- Calculate asymptotic values over the same range 'y' ---
res_asymp_left <- asymp_inv_left(y)
res_asymp_right <- asymp_inv_right(y)

# --- Create the plot ---

# Plot 1: Linear x-axis (standard)
plot(y, res, type = "l", lwd = 2, col = "black",
     xlab = "p", ylab = expression(F^{-1}(p)),
     main = "Computed Quantile vs. Asymptotic Approximations")

# Add the left tail asymptote
lines(y, res_asymp_left, col = "blue", lty = 2, lwd = 1.5)

# Add the right tail asymptote
lines(y, res_asymp_right, col = "red", lty = 2, lwd = 1.5)

# Add a legend
legend("topleft",
       legend = c("Computed F^{-1}(p) (Finv)",
                  "Left Asymptote (p -> 0)",
                  "Right Asymptote (p -> 1)"),
       col = c("black", "blue", "red"),
       lty = c(1, 2, 2),
       lwd = c(2, 1.5, 1.5),
       bty = "n") # No box around legend
grid()


#