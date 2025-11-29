library(univariateML)

logmom_burr3 <- function(m1, m2, m3) {

  basel <- trigamma(1)
  neuler <- digamma(1)
  psi2_1 <- psigamma(1, deriv = 2)

  f_over_df <- function(log_alpha) {
    mu <- m1 - log_alpha
    c <- logmom_burr_c(mu, m2)
    k <- inverse_digamma(neuler - c * mu)

    psi2_k <- psigamma(k, deriv = 2)

    psi1_k <- digamma(k)
    psiP1_k <- trigamma(k)
    psiP3_k <- psigamma(k, deriv = 3)

    J_sub11 <- -(neuler - psi1_k) / c^2
    J_sub12 <- -psiP1_k / c
    J_sub21 <- -2 * (basel + psiP1_k) / c^3
    J_sub22 <- psi2_k / c^2

    J_sub <- matrix(c(J_sub11, J_sub21, J_sub12, J_sub22), 2, 2)

    rhs <- c(-1, 0)
    v_vec <- solve(J_sub, rhs)
    v_c <- v_vec[1]
    v_k <- v_vec[2]

    f <- (1 / c^3) * (psi2_1 - psi2_k) - m3
    df <- (1 / c^4) * ( -3 * v_c * (psi2_1 - psi2_k) - c * psiP3_k * v_k )

    f / df
  }

  a0 <- m1
  ## Newton--Raphson for a.
  reltol <- .Machine$double.eps^0.25
  iterlim <- 100
  for (i in seq(iterlim)) {
    a <- a0 - f_over_df(a0)
    if (abs(a0 - a) < abs(a0)*reltol) break
    a0 <- a
  }
  log_alpha <- a0

  alpha <- exp(log_alpha)
  mu <- m1 - log_alpha
  c <- logmom_burr_c(mu, m2)
  k <- inverse_digamma(digamma(1) - c * mu)

  list(alpha = 1/alpha, c = c, k = k, iterations = i)
}

# --- Example Usage ---
# Generate Data
set.seed(123)
alpha_true <- 5
c_true <- 0.1
k_true <- 10
x <- actuar::rburr(100000, k_true, c_true, alpha_true)

log_x <- log(x)
m1 <- mean(log_x)
m2 <- var(log_x)
m3 <- mean((log_x - m1)^3)
estimates <- logmom_burr3(m1, m2, m3)

# Display Estimates
print(estimates)

# Display True Values
print(list(alpha_true = alpha_true, c_true = c_true, k_true = k_true))
