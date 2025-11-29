fisherBurr <- function(k, c) {
  ## digamma(2) == 1 - gamma; trigamma(2) == pi^2/6 - 1
  psi2 <- digamma(2)
  tp2 <- trigamma(2)

  I_cc <- 1 / c^2 + k / (c^2 * (k + 2)) * ((psi2 - digamma(k + 1))^2 + tp2 + trigamma(k + 1))
  I_ck <- (psi2 - digamma(k)) / (c * (k + 1))
  I_kk <- 1 / k^2

  mat <- matrix(c(I_kk, I_ck, I_ck, I_cc),
    nrow = 2, byrow = TRUE,
    dimnames = list(c("k", "c"), c("k", "c"))
  )
  return(mat)
}


c <- 0.1
k <- 0.1
n <- 100000
x <- actuar::rburr(n, k, c)
numDeriv::hessian(\(p) -sum(actuar::dburr(x, p[1], p[2], log = TRUE)), c(k, c)) / n
fisherBurr(k, c)

solve(fisherBurr(k, c))

fisherBurrlog <- function(k, c) {
  ## digamma(2) == 1 - gamma; trigamma(2) == pi^2/6 - 1
  psi2 <- digamma(2)
  tp2 <- trigamma(2)

  i_cc <- 1 + k / (k + 2) * ((psi2 - digamma(k + 1))^2 + tp2 + trigamma(k + 1))
  i_ck <- (psi2 - digamma(k)) * k / (k + 1)
  i_kk <- 1

  matrix(
    c(
      1, i_ck,
      i_ck, i_cc
    ),
    nrow = 2, byrow = TRUE,
    dimnames = list(c("k", "c"), c("k", "c"))
  )
}

c <- 7
k <- 4
c_log <- log(c)
k_log <- log(k)
n <- 100000
x <- actuar::rburr(n, k, c)
numDeriv::hessian(\(p) -sum(actuar::dburr(x, exp(p[1]), exp(p[2]), log = TRUE)), c(k_log, c_log)) / n
fisherBurrlog(k, c)


actuar::rburr(100, 8, 0.5) -> x

burr_one_step(x)
burr_lmom(mu, sigma2)[1:2]
mlburr(x)

fisherBurrlog <- function(k, c) {
  psi2 <- digamma(2)
  tp2 <- trigamma(2)

  i_cc <- 1 + k / (k + 2) * ((psi2 - digamma(k + 1))^2 + tp2 + trigamma(k + 1))
  i_ck <- (psi2 - digamma(k)) * k / (k + 1)
  i_kk <- 1

  matrix(
    c(
      1, i_ck,
      i_ck, i_cc
    ),
    nrow = 2, byrow = TRUE,
    dimnames = list(c("k", "c"), c("k", "c"))
  )
}

burr_one_step <- function(x) {
  n <- length(x)
  log_x <- log(x)
  mu <- sum(log_x) / n
  sigma2 <- sum(log_x^2) / n - mu^2
  theta0 <- burr_lmom(mu, sigma2)[1:2]
  k0 <- theta0[1]
  c0 <- theta0[2]

  u <- 1 - c(
    k0 * sum(log(1 + x^c0)) / n,
    c0 * ((k0 + 1) * sum(log_x * x^c0 / (1 + x^c0)) / n - mu)
  )

  i <- fisherBurrlog(k0, c0)
  theta <- stats::setNames(log(theta0) + solve(i, u), c("k", "c"))
  exp(theta)
}

burr_one_step(x)
burr_lmom(mu, sigma2)[1:2]
mlburr(x)


burr_mle_fisher_sq <- function(x, tol = 1e-8, max_iter = 100) {
  n     <- length(x)
  log_x <- log(x)
  mu    <- mean(log_x)

  # 1) start from your one‐step (k,c)
  init  <- burr_one_step(x)
  k     <- init["k"]
  c     <- init["c"]
  phi   <- c(k^2, c^2)
  names(phi) <- c("k","c")   # still in (k,c) order

  for (iter in seq_len(max_iter)) {
    # 2) unpack current
    k <- sqrt(phi["k"])
    c <- sqrt(phi["c"])

    # 3) compute the two‐dim score components
    s1 <- (1/k - mean(log(1 + x^c))) / (2*k)
    s2 <- (1/c + mu
           - (k + 1)*mean(x^c * log_x / (1 + x^c))
          ) / (2*c)
    u_phi <- c(s1, s2)

    # 4) get the log‐param Fisher and convert
    Ilog     <- fisherBurrlog(k, c)
    I11      <- Ilog["k","k"] / (4 * k^4)
    I12      <- Ilog["k","c"] / (4 * k^2 * c^2)
    I22      <- Ilog["c","c"] / (4 * c^4)
    I_phi    <- matrix(c(I11, I12, I12, I22),
                       nrow = 2, byrow = TRUE,
                       dimnames = list(c("k","c"), c("k","c")))

    # 5) Fisher‐scoring update
    delta    <- solve(I_phi, u_phi)
    phi_new  <- phi + delta

    # 6) convergence?
    if (max(abs(delta)) < tol) {
      phi <- phi_new
      break
    }
    phi <- phi_new
  }

  # 7) return MLE on original scale
  k_mle <- sqrt(phi["k"])
  c_mle <- sqrt(phi["c"])

  list(
    k_mle      = k_mle,
    c_mle      = c_mle,
    phi        = phi,
    iterations = iter,
    converged  = (iter < max_iter)
  )
}

burr_mle_fisher_raw <- function(x, tol = 1e-8, max_iter = 100) {
  n     <- length(x)
  log_x <- log(x)
  mu    <- mean(log_x)

  # start from your one‐step (k,c)
  init <- burr_one_step(x)
  k    <- init["k"]
  c    <- init["c"]

  for (iter in seq_len(max_iter)) {
    # score for raw parameters:
    s_k <- 1/k - mean(log(1 + x^c))
    s_c <- 1/c + mu - (k + 1)*mean(x^c * log_x / (1 + x^c))
    u    <- c(s_k, s_c)

    # get raw Fisher = D^{-1} I_log D^{-1}, D=diag(k,c)
    Ilog <- fisherBurrlog(k, c)
    Dinv <- diag(c(1/k, 1/c))
    Iraw <- Dinv %*% Ilog %*% Dinv

    # update
    delta <- solve(Iraw, u)
    k2    <- k + delta[1]
    c2    <- c + delta[2]

    # check convergence
    if (max(abs(delta)) < tol) {
      k <- k2; c <- c2
      converged <- TRUE
      break
    }

    k <- k2; c <- c2
  }
  if (!exists("converged")) converged <- FALSE

  list(
    k_mle      = k,
    c_mle      = c,
    iterations = iter,
    converged  = converged
  )
}

burr_mle_fisher_raw_fallback <- function(x, tol = 1e-8, max_iter = 100) {
  n     <- length(x)
  log_x <- log(x)
  mu    <- mean(log_x)

  init      <- burr_one_step(x)
  k         <- init["k"]
  c         <- init["c"]
  fallback  <- FALSE

  for (iter in seq_len(max_iter)) {
    # raw score
    s_k <- 1/k - mean(log(1 + x^c))
    s_c <- 1/c + mu - (k + 1)*mean(x^c * log_x / (1 + x^c))
    u    <- c(s_k, s_c)

    # raw Fisher
    Ilog <- fisherBurrlog(k, c)
    Dinv <- diag(c(1/k, 1/c))
    Iraw <- Dinv %*% Ilog %*% Dinv

    # attempt raw update
    delta   <- solve(Iraw, u)
    k2      <- k + delta[1]
    c2      <- c + delta[2]

    if (k2 > 0 && c2 > 0) {
      # accept raw update
      k <- k2; c <- c2
      if (max(abs(delta)) < tol) {
        converged <- TRUE
        break
      }
    } else {
      # fallback to log‐scale update
      fallback <- TRUE
      theta   <- c(log(k), log(c))
      # log‐scale score = c * s_c etc.
      u_log   <- c(
        k * (1/k - mean(log(1 + x^c))),
        c * (1/c + mu - (k + 1)*mean(x^c * log_x / (1 + x^c)))
      )
      Ilog    <- fisherBurrlog(k, c)  # as before
      delta_l <- solve(Ilog, u_log)
      theta2  <- theta + delta_l
      k       <- exp(theta2[1])
      c       <- exp(theta2[2])
      if (max(abs(delta_l)) < tol) {
        converged <- TRUE
        break
      }
    }
  }
  if (!exists("converged")) converged <- FALSE

  list(
    k_mle      = k,
    c_mle      = c,
    iterations = iter,
    converged  = converged,
    fallback   = fallback
  )
}

burr_mle_fisher_good <- function(x) {
  n     <- length(x)
  log_x <- log(x)
  mu    <- sum(log_x) / n
  sigma2 <- sum(log_x^2) / n - mu^2
  theta0  <- burr_lmom(mu, sigma2)[1:2]

  reltol = .Machine$double.eps^0.5
  max_iter = 100
  for (iter in seq_len(max_iter)) {
    k <- theta0[1]
    c <- theta0[2]
    x_c <- x^c

    u <- 1 - c(
      k * sum(log1p(x_c)) / n,
      c * ((k + 1) * sum(log_x * x_c / (1 + x_c)) / n - mu)
    )

    i <- fisherBurrlog(k, c)
    theta <- exp(log(theta0) + solve(i, u))

    if (max(abs(theta0[2] - theta[2]) / abs(theta0)[2]) < reltol) {
      theta0 <- theta
      break
    }
    theta0 <- theta
  }

  list(theta = theta0, iterations = iter)
}

burr_mle_fisher <- function(x, tol = 1e-8, max_iter = 100) {
  # 1) data summaries
  n     <- length(x)
  log_x <- log(x)
  mu    <- sum(log_x) / n

  # 2) starting values from your one‐step estimator
  init  <- burr_one_step(x)
  k     <- init["k"]
  c     <- init["c"]
  # work on the log-scale for scoring
  log_theta <- log(c(k, c))
  names(log_theta) <- c("k", "c")

  # 3) iterate
  for (iter in seq_len(max_iter)) {
    # score vector, exactly as in burr_one_step:
    u <- 1 - c(
      k * sum(log(1 + x^c))   / n,
      c * ((k + 1) * sum(log_x * x^c / (1 + x^c)) / n - mu)
    )

    # expected info (per observation)
    I <- fisherBurrlog(k, c)

    # Fisher-scoring step on log-parameters
    delta      <- solve(I, u)
    log_theta2 <- log_theta + delta

    # back‐transform to original scale
    k2 <- exp(log_theta2["k"])
    c2 <- exp(log_theta2["c"])

    # check convergence
    if (max(abs(delta)) < tol) {
      log_theta <- log_theta2
      k <- k2; c <- c2
      break
    }

    # otherwise update and continue
    log_theta <- log_theta2
    k <- k2; c <- c2
  }

  # 4) return
  list(
    k      = k,
    c      = c,
    log_par= log_theta,
    iter   = iter,
    converged = (iter < max_iter)
  )
}

burr_mle_fisher_exp <- function(x) {
  n      <- length(x)
  inv_n  <- 1/n
  log_x  <- log(x)
  mu     <- sum(log_x) * inv_n
  sigma2 <- sum(log_x^2) * inv_n - mu^2

  # 1) starting values from your LMOM
  theta0 <- burr_lmom(mu, sigma2)[1:2]  # [1]=k0, [2]=c0
  k0 <- theta0[1]; c0 <- theta0[2]
  # psi = (log(k+1), log(c))
  psi <- c(log(k0 + 1), log(c0))

  reltol   <- .Machine$double.eps^0.5
  max_iter <- 100

  for (iter in seq_len(max_iter)) {
    psi_old <- psi
    gamma   <- exp(psi[1])          # = k+1
    k       <- gamma - 1
    c       <- exp(psi[2])

    # precompute means
    x_c     <- x^c
    m1      <- mean(log1p(x_c))     # = mean(log(1+x^c))
    m2      <- mean(log_x * x_c/(1 + x_c))

    # score on psi
    u1 <- gamma * (1/k   - m1)       # dℓ/dψ₁
    u2 <- c     * (1/c + mu - (k+1)*m2)  # dℓ/dψ₂
    u  <- c(u1, u2)

    # fisher for (c,k) in log‐params
    I_log <- fisherBurrlog(k, c)    # rows/cols = ("k","c")
    # convert to raw (c,k)
    Dinv  <- diag(c(1/k, 1/c))
    I_raw <- Dinv %*% I_log %*% Dinv
    # Jacobian J = d(c,k)/d(ψ₁,ψ₂) = diag(γ, c)
    J     <- diag(c(gamma, c))
    I_psi <- t(J) %*% I_raw %*% J

    # update
    delta <- solve(I_psi, u)
    psi   <- psi + delta

    # break on ψ₂ rel-change (ψ₂ = log c)
    if (abs(psi_old[2] - psi[2]) / abs(psi_old[2]) < reltol) {
      break
    }
  }

  # back‐transform
  c_mle <- exp(psi[2])
  k_mle <- exp(psi[1]) - 1

  list(theta = c(k_mle, c_mle), iterations = iter)
}
burr_mle_fisher_tail <- function(x) {
  n      <- length(x)
  inv_n  <- 1/n
  log_x  <- log(x)
  mu     <- sum(log_x) * inv_n
  sigma2 <- sum(log_x^2) * inv_n - mu^2

  # 1) start from your one‐step (k,c)
  theta0 <- burr_lmom(mu, sigma2)[1:2]  # [1]=k0, [2]=c0
  k0 <- theta0[1]; c0 <- theta0[2]
  # φ = (log(c*k), log(c))
  phi <- c(log(c0 * k0), log(c0))

  reltol   <- .Machine$double.eps^0.5
  max_iter <- 100
  # Jacobian J for θ=(log k, log c)=J φ
  J <- matrix(c(1, -1,
                0,  1),
              nrow = 2, byrow = TRUE)

  for (iter in seq_len(max_iter)) {
    phi_old <- phi
    c       <- exp(phi[2])
    k       <- exp(phi[1] - phi[2])

    # precompute
    x_c     <- x^c
    m1      <- mean(log1p(x_c))
    m2      <- mean(log_x * x_c/(1 + x_c))

    # scores in (log k, log c)
    u_k <- 1 - k * m1
    u_c <- 1 + c * mu - c * (k + 1) * m2
    u_theta <- c(u_k, u_c)

    # transform score
    u_phi <- t(J) %*% u_theta

    # info in (log k, log c)
    I_log   <- fisherBurrlog(k, c)
    # transform to φ
    I_phi   <- t(J) %*% I_log %*% J

    # update
    delta <- solve(I_phi, u_phi)
    phi   <- phi + as.numeric(delta)

    # **robust** break on φ₂ rel-change (φ₂ = log c)
    den <- abs(phi_old[2])
    if (den > 0) {
      relchg <- abs(phi_old[2] - phi[2]) / den
    } else {
      relchg <- abs(phi_old[2] - phi[2])
    }
    if (relchg < reltol) {
      break
    }
  }

  # back-transform
  c_mle <- exp(phi[2])
  k_mle <- exp(phi[1] - phi[2])

  list(theta = c(k_mle, c_mle), iterations = iter)
}



actuar::rburr(1000, 8, 0.5) -> x
# microbenchmark::microbenchmark(
#   mlburr_(x),
#   burr_mle_fisher_good(x),
#   burr_one_step(x),
#   burr_lmom = {    n     <- length(x)
#       log_x <- log(x)
#     mu    <- sum(log_x) / n
#   sigma2 <- sum(log_x^2) / n - mu^2
#   theta0  <- burr_lmom(mu, sigma2)[1:2]})


#burr_one_step(x)
#burr_lmom(mu, sigma2)[1:2]
#mlburr(x)

burr_start_quantile <- function(x, p1=0.1, p2=0.9) {
  q <- as.numeric(quantile(x, c(p1, p2)))
  target <- log(1-p1)/log(1-p2)
  f <- function(c) log(1+q[1]^c)/log(1+q[2]^c) - target
  c0 <- uniroot(f, lower=1e-6, upper=40)$root
  k0 <- -log(1-p1)/log(1 + q[1]^c0)
  c(k=k0, c=c0)
}


burr_mle_fisher_quant <- function(x, tol = 1e-8, max_iter = 100) {
  # 1) data summaries
  n     <- length(x)
  log_x <- log(x)
  mu    <- sum(log_x) / n

  # 2) starting values from your one‐step estimator
  init  <- burr_start_quantile(x)
  k     <- init["k"]
  c     <- init["c"]
  # work on the log-scale for scoring
  log_theta <- log(c(k, c))
  names(log_theta) <- c("k", "c")

  # 3) iterate
  for (iter in seq_len(max_iter)) {
    # score vector, exactly as in burr_one_step:
    u <- 1 - c(
      k * sum(log(1 + x^c))   / n,
      c * ((k + 1) * sum(log_x * x^c / (1 + x^c)) / n - mu)
    )

    # expected info (per observation)
    I <- fisherBurrlog(k, c)

    # Fisher-scoring step on log-parameters
    delta      <- solve(I, u)
    log_theta2 <- log_theta + delta

    # back‐transform to original scale
    k2 <- exp(log_theta2["k"])
    c2 <- exp(log_theta2["c"])

    # check convergence
    if (max(abs(delta)) < tol) {
      log_theta <- log_theta2
      k <- k2; c <- c2
      break
    }

    # otherwise update and continue
    log_theta <- log_theta2
    k <- k2; c <- c2
  }

  # 4) return
  list(
    k      = k,
    c      = c,
    log_par= log_theta,
    iter   = iter,
    converged = (iter < max_iter)
  )
}

actuar::rburr(200, 0.1, 5) -> x
burr_mle_fisher(x)$iter
burr_mle_fisher_quant(x)$iter
burr_mle_fisher_sq(x)$iterations
burr_mle_fisher_raw(x)$iterations
burr_mle_fisher_raw_fallback(x)$iterations
burr_mle_fisher_tail(x)$iterations
burr_mle_fisher_exp(x)$iterations
#burr_mle_fisher_tail(x)




