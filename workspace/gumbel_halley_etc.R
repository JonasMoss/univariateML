x <- extraDistr::rgumbel(2000, 1, 0.1)

microbenchmark::microbenchmark(
  ml_gumbel_halley_rcpp(x),
  ml_gumbel_halley_fast(x),
  ml_gumbel_halley2_rcpp(x),
  ml_gumbel_newt_rcpp(x),
  ml_gumbel_newt_fast(x),
  ml_gumbel_newt2_rcpp(x), times = 1000

)




x <- extraDistr::rgumbel(1000, 20, 1/10)
mlgumbel2_ <- function(x, ...) {
  x_bar <- sum(x) / length(x)
  sigma0 <- sqrt(var(x)) * 2.45 / pi
  x_bar <- x_bar + digamma(1) * sigma0
  estimates <- mlgumbel_estimate(x - x_bar, ...)
  mu <- estimates[1] + x_bar
  sigma <- estimates[2]
  x_bar <- sum(x) / length(x)
  logLik <- -length(x) * (log(sigma) + (x_bar - mu) / sigma + 1)
  list(estimates = c(mu = mu, sigma = sigma), logLik = logLik, iterations = estimates[3])
}

mlgumbel_(x)
mlgumbel2_(x)

n <- 1000
results <- t(replicate(10000, {
  x <- extraDistr::rgumbel(n, 7, 2)
  est <- ml_gumbel_halley_fast(x)
  c(est[1] -(digamma(1) + 1) * est[2], est[2] * sqrt(pi^2 / 6))
}))

cov(results) * n

n <- 1000
results <- t(replicate(10000, {
  x <- extraDistr::rgumbel(n, 7, 89)
  est <- ml_gumbel_halley_fast(x)
  c(est[1], log(est[2]))
}))
cov(results) * n
1/(pi^2/6)


1
6/pi^2


# 1) "φ"–parameterization (isotropic Fisher)
gumbel_mle_phi <- function(x, tol = 1e-8, maxit = 1000) {
  n <- length(x)
  m <- mean(x); v <- var(x)
  # MoM start
  beta <- sqrt(6 * v) / pi
  mu   <- m - 0.57721566490153286 * beta    # γ ≈ 0.5772
  c    <- 0.57721566490153286 - 1
  w    <- pi / sqrt(6)
  phi1 <- mu + c * beta
  phi2 <- w * beta
  inv_n_pi2 <- 6 / (n * pi^2)

  for (i in seq_len(maxit)) {
    beta <- phi2 / w
    mu   <- phi1 - c * beta
    z    <- (x - mu) / beta
    ez   <- exp(-z)
    U_mu   <- sum(1 - ez) / beta
    U_beta <- sum(z - 1 - z * ez) / beta
    scalar <- phi2 * phi2 * inv_n_pi2

    phi1_new <- phi1 + scalar * U_mu
    phi2_new <- phi2 + scalar * (U_beta / w)
    if (max(abs(phi1_new - phi1), abs(phi2_new - phi2)) < tol) break

    phi1 <- phi1_new; phi2 <- phi2_new
  }

  beta <- phi2 / w
  mu   <- phi1 - c * beta
  list(mu = mu, beta = beta, iterations = i)
}

# 2) "ψ"–parameterization (log‐scale & diagonal Fisher)
gumbel_mle_psi <- function(x, tol = 1e-8, maxit = 1000) {
  n <- length(x)
  m <- mean(x); v <- var(x)
  # MoM start for beta and mu
  beta0 <- sqrt(6 * v) / pi
  gamma <- 0.57721566490153286
  c     <- gamma - 1       # = –0.4228…
  w     <- pi / sqrt(6)    # scaling for phi2

  # ψ‐parameters: ψ1 = μ + c β, ψ2 = log(w β)
  #   μ0 = m – γ β0  ⇒  ψ1_0 = μ0 + c β0 = m – β0
  psi1 <- m - beta0
  psi2 <- log(w * beta0)

  inv_n   <- 1 / n
  inv_pi2 <- 6 / (pi^2)

  for (i in seq_len(maxit)) {
    # map back to (μ,β)
    phi2 <- exp(psi2)
    beta <- phi2 / w
    mu   <- psi1 - c * beta

    # scores in (μ,β)
    z      <- (x - mu) / beta
    ez     <- exp(-z)
    U_mu   <- sum(1 - ez) / beta
    U_beta <- sum( z - 1 - z * ez ) / beta

    # transform to ψ‐scores
    U_psi1 <-          U_mu
    U_psi2 <- beta * U_beta

    # Fisher‐scoring updates (now with 1/n!)
    psi1_new <- psi1 + inv_n * beta^2     * U_psi1
    psi2_new <- psi2 + inv_n * inv_pi2    * U_psi2

    if (max(abs(psi1_new - psi1), abs(psi2_new - psi2)) < tol) {
      psi1 <- psi1_new; psi2 <- psi2_new
      break
    }
    psi1 <- psi1_new; psi2 <- psi2_new
  }

  # back‐transform to (μ,β)
  beta_est <- exp(psi2) / w
  mu_est   <- psi1  - c * beta_est

  list(mu = mu_est, beta = beta_est, iterations = i)
}

# 1) Newton–Raphson in the φ‐parameterization
gumbel_mle_phi_nr <- function(x, tol = .Machine$double.eps^0.5, maxit = 100) {
  n <- length(x)
  x_bar <- mean(x)
  x <- x - x_bar
  m <- mean(x); v <- var(x)
  # constants
  gamma <- 0.57721566490153286
  c     <- gamma - 1
  w     <- pi / sqrt(6)

  # Method‐of‐Moments start
  beta <- sqrt(6 * v) / pi
  mu   <- m - gamma * beta
  phi1 <- mu + c * beta
  phi2 <- w * beta

  for (it in seq_len(maxit)) {
    # back‐transform
    beta <- phi2 / w
    mu   <- phi1 - c * beta

    # vectorized sums
    z         <- (x - mu) / beta
    ez        <- exp(-z)
    sum_ez    <- sum(ez)
    sum_z     <- sum(z)
    sum_z_ez  <- sum(z * ez)
    sum_z2_ez <- sum(z^2 * ez)

    # scores in (μ,β)
    U_mu   <- (n - sum_ez) / beta
    U_beta <- (sum_z - sum_z_ez - n) / beta

    # Hessian in (μ,β)
    H_mu_mu     <- -sum_ez / beta^2
    H_mu_beta   <- -(n - sum_ez + sum_z_ez) / beta^2
    H_beta_beta <- (n - 2*sum_z + 2*sum_z_ez - sum_z2_ez) / beta^2

    # transform to φ‐space
    U1 <- U_mu + c * U_beta
    U2 <-       w * U_beta

    H11 <- H_mu_mu + 2*c * H_mu_beta + c^2 * H_beta_beta
    H12 <- w * (H_mu_beta + c * H_beta_beta)
    H22 <- w^2 * H_beta_beta

    # Newton step in φ
    detH   <- H11 * H22 - H12^2
    d1     <- (  H22 * U1 - H12 * U2 ) / detH
    d2     <- ( -H12 * U1 + H11 * U2 ) / detH

    phi1_new <- phi1 - d1
    phi2_new <- phi2 - d2

    if (max(abs(phi1_new - phi1), abs(phi2_new - phi2)) < tol) {
      phi1 <- phi1_new; phi2 <- phi2_new
      break
    }
    phi1 <- phi1_new; phi2 <- phi2_new
  }

  # back‐transform final estimates
  beta_est <- phi2 / w
  mu_est   <- phi1 - c * beta_est
  list(mu = mu_est + x_bar, beta = beta_est, iterations = it)
}

# 2) Newton–Raphson in the ψ‐parameterization (log‐β)
gumbel_mle_psi_nr <- function(x, tol = .Machine$double.eps^0.5, maxit = 100) {
  n <- length(x)
  m <- mean(x);
  v <- var(x)
  # constants
  gamma <- 0.57721566490153286
  c     <- gamma - 1
  w     <- pi / sqrt(6)
  sum_x     <- sum(x)

  # Method‐of‐Moments start
  beta0 <- sqrt(6 * v) / pi
  psi1  <- m - beta0           # ψ1 = μ + cβ  ⇒ μ0 = m - γβ0  ⇒ ψ1_0 = μ0 + cβ0 = m - β0
  psi2  <- log(w * beta0)      # ψ2 = log(w β)

  for (it in seq_len(maxit)) {
    # back‐transform
    e2   <- exp(psi2)
    beta <- e2 / w
    mu   <- psi1 - c * beta

    # vectorized sums
    z         <- (x - mu) / beta
    ez        <- exp(-z)
    sum_z     <- (sum_x - n*mu) / beta
    sum_ez    <- sum(ez)
    sum_z_ez  <- sum(z * ez)
    sum_z2_ez <- sum(z^2 * ez)

    # scores in (μ,β)
    U_mu   <- (n - sum_ez) / beta
    U_beta <- (sum_z - sum_z_ez - n) / beta

    # Hessian in (μ,β)
    H_mu_mu     <- -sum_ez / beta^2
    H_mu_beta   <- -(n - sum_ez + sum_z_ez) / beta^2
    H_beta_beta <- (n - 2*sum_z + 2*sum_z_ez - sum_z2_ez) / beta^2

    # transform to ψ‐space
    U_psi1 <- U_mu
    U_psi2 <- beta * (U_beta - c * U_mu)

    H11_psi <- H_mu_mu
    H12_psi <- beta * (H_mu_beta - c * H_mu_mu)
    H22_psi <- beta^2 * (
      c^2 * H_mu_mu
      - 2*c   * H_mu_beta
      +       H_beta_beta
    ) +
      beta * (U_beta - c * U_mu)

    # Newton step in ψ
    detH   <- H11_psi * H22_psi - H12_psi^2
    dps1   <- (  H22_psi * U_psi1 - H12_psi * U_psi2 ) / detH
    dps2   <- ( -H12_psi * U_psi1 + H11_psi * U_psi2 ) / detH

    psi1_new <- psi1 - dps1
    psi2_new <- psi2 - dps2

    if (max(abs(psi1_new - psi1), abs(psi2_new - psi2)) < tol) {
      psi1 <- psi1_new; psi2 <- psi2_new
      break
    }
    psi1 <- psi1_new; psi2 <- psi2_new
  }

  # back‐transform final estimates
  beta_est <- exp(psi2) / w
  mu_est   <- psi1 - c * beta_est
  list(mu = mu_est, beta = beta_est, iterations = it)
}

n <- 100
x <- extraDistr::rgumbel(n, 20, 0.1)
#ml_gumbel_newt_fast(x)
#gumbel_mle_phi(x)
gumbel_mle_psi(x)
mlgumbel_(x)
gumbel_mle_psi_nr(x)
#gumbel_mle_phi_nr(x)
