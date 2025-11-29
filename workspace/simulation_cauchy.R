mlcauchy_newt <- function(x, tol = 1e-8, maxit = 100, init = NULL) {
  n <- length(x)
  if (is.null(init)) {
    mu    <- median(x)
    mad0  <- median(abs(x - mu))
    theta <- log(mad0 + .Machine$double.eps)
  } else {
    mu    <- init$mu
    theta <- init$theta
  }

  for (i in seq_len(maxit)) {
    s      <- exp(theta)
    inv_s  <- 1 / s
    inv_s2 <- inv_s * inv_s

    z     <- (x - mu) * inv_s
    z2    <- z * z
    dnom2 <- (1 + z2)^2

    # scores
    U_mu    <- 2 * inv_s * sum(z / (1 + z2))
    U_theta <-      sum((z2 - 1) / (1 + z2))

    # corrected observed information
    H_mm <- 2 * inv_s2 * sum((1 - z2) / dnom2)
    H_tt <- 4 *            sum(z2 / dnom2)
    H_mt <- 4 * inv_s     * sum(z / dnom2)

    # analytic 2×2 solve
    det  <- H_mm * H_tt - H_mt * H_mt
    dmu      <- ( H_tt * U_mu    - H_mt * U_theta) / det
    dtheta   <- (-H_mt * U_mu    + H_mm * U_theta) / det

    mu_new    <- mu + dmu
    theta_new <- theta + dtheta

    if (max(abs(dmu), abs(dtheta)) < tol) {
      mu    <- mu_new
      theta <- theta_new
      break
    }
    mu    <- mu_new
    theta <- theta_new
  }

  list(
    estimates  = c(mu = mu, scale = exp(theta)),
    iterations = i
  )
}

mlcauchy_fish <- function(x, tol = 1e-8, maxit = 100, init = NULL) {
  n <- length(x)
  if (is.null(init)) {
    mu    <- median(x)
    mad0  <- median(abs(x - mu))
    theta <- log(mad0 + .Machine$double.eps)
  } else {
    mu    <- init$mu
    theta <- init$theta
  }

  for (i in seq_len(maxit)) {
    s     <- exp(theta)
    inv_s <- 1/s

    z     <- (x - mu) * inv_s
    z2    <- z * z
    d1    <- 1 + z2

    U_mu    <- 2*inv_s * sum(z / d1)
    U_theta <-      sum((z2 - 1) / d1)

    dmu    <- (2 * s^2 / n) * U_mu
    dtheta <- (1 / (2 * n))   * U_theta

    mu_new    <- mu + dmu
    theta_new <- theta + dtheta

    if (max(abs(dmu), abs(dtheta)) < tol) {
      mu <- mu_new; theta <- theta_new
      break
    }
    mu <- mu_new; theta <- theta_new
  }

  list(
    estimates  = c(mu = mu, scale = exp(theta)),
    iterations = i
  )
}

mlcauchy_newt_fastR <- function(x, tol = 1e-8, maxit = 100, init = NULL) {
  n <- length(x)
  if (is.null(init)) {
    mu    <- median(x)
    mad0  <- median(abs(x - mu))
    theta <- log(mad0 + .Machine$double.eps)
  } else {
    mu    <- init$mu
    theta <- init$theta
  }

  for (i in seq_len(maxit)) {
    inv_s  <- exp(-theta)
    inv_s2 <- inv_s * inv_s
    z      <- (x - mu) * inv_s
    z2     <- z * z
    d1     <- 1 + z2
    inv_d1 <- 1 / d1
    inv_d1_2 <- inv_d1 * inv_d1

    # all four sums in one go
    sum1 <- sum(z * inv_d1)          # for U_mu
    sum2 <- sum((z2 - 1) * inv_d1)   # for U_theta
    sum3 <- sum((1 - z2) * inv_d1_2) # for H_mm
    sum4 <- sum(z * inv_d1_2)        # for H_mt
    sum5 <- sum(z2 * inv_d1_2)       # for H_tt

    U_mu    <- 2 * inv_s * sum1
    U_theta <-        sum2

    H_mm <- 2 * inv_s2 * sum3
    H_mt <- 4 * inv_s   * sum4
    H_tt <- 4             * sum5

    det  <- H_mm * H_tt - H_mt * H_mt
    dmu    <- ( H_tt * U_mu    - H_mt * U_theta) / det
    dtheta <- (-H_mt * U_mu    + H_mm * U_theta) / det

    mu_new    <- mu + dmu
    theta_new <- theta + dtheta

    if (max(abs(dmu), abs(dtheta)) < tol) {
      mu <- mu_new; theta <- theta_new
      break
    }
    mu <- mu_new; theta <- theta_new
  }

  list(
    estimates  = c(mu = mu, scale = exp(theta)),
    iterations = i
  )
}




rcauchy(500, 2, 10) -> x
mlcauchy(x)
mlcauchy_fish(x)
mlcauchy_newt(x)
mlcauchy_newt_fastR(x)
mlcauchy_rcpp(x)
microbenchmark::microbenchmark(mlcauchy(x), mlcauchy_newt(x), mlcauchy_newt_fastR(x), mlcauchy_rcpp(x))
