# MLE for 3-parameter Burr XII via pure Newton–Raphson on log-scale

ml_burr_nlm <- function(x, ...) {
  # starting on log-scale: log(c), log(k), log(alpha)
  start <- c(log(1), log(1), log(median(x)))
  # negative log-likelihood on log-scale
  f <- function(p) {
    c <- exp(p[1]); k <- exp(p[2]); a <- exp(p[3])
    -sum(
      dcBurr <- log(c * k / a) + (c - 1)*(log(x) - log(a)) - (k + 1)*log(1 + (x/a)^c)
    )
  }
  values <- stats::nlm(f = f, p = start, ...)
  est <- exp(values$estimate)
  names(est) <- c("c", "k", "alpha")
  list(
    estimates = est,
    logLik = -values$minimum,
    code = values$code
  )
}

ml_burr_newton <- function(x,
                           mle_2p_func = NULL, # Pass the 2p MLE function here
                           init = NULL,
                           tol = 1e-8,
                           maxit = 100,
                           trace = FALSE) {
  # x: positive numeric vector
  n <- length(x)

  # initial values on log-scale: phi = (log(cp), log(kp), log(ap))
  if (is.null(init)) {
    ap_init <- median(x)
    mle_2p_result <- mlburr_(x/ap_init)
    kp_init <- mle_2p_result$estimates[1]
    cp_init <- mle_2p_result$estimates[2]

    # Construct initial phi vector
    init <- c(log_cp = log(cp_init), log_kp = log(kp_init), log_ap = log(ap_init))
    if(trace) cat("Initial estimates (cp, kp, ap):", signif(exp(init), 4), "\n")
  }
  phi <- init

  # log-likelihood function
  loglik <- function(cp, kp, ap) {
    tryCatch({
      if(any(c(cp, kp, ap) <= 0)) return(-Inf)
      sum(log(cp) + log(kp) - log(ap) + (cp - 1) * (log(x) - log(ap))
          - (kp + 1) * log1p((x / ap)^cp))
    }, error = function(e) -Inf)
  }

  current_logLik_val <- loglik(exp(phi[1]), exp(phi[2]), exp(phi[3]))
  if (!is.finite(current_logLik_val)) {
    stop("Initial parameters produce non-finite log-likelihood.")
  }

  for (iter in seq_len(maxit)) {
    # unpack parameters
    cp <- exp(phi[1]); kp <- exp(phi[2]); ap <- exp(phi[3])

    # auxiliaries (vectorized calculations)
    v <- x / ap
    t_vec <- v^cp
    L_vec <- log(v)
    log1p_t_vec <- log1p(t_vec)
    t_L_vec <- t_vec * L_vec
    one_plus_t_vec <- 1 + t_vec
    common_denom_sq <- one_plus_t_vec^2
    t_over_one_plus_t <- t_vec / one_plus_t_vec

    # gradient of TOTAL logL on original scale
    s_cp <- n/cp + sum(L_vec) - (kp + 1) * sum(t_L_vec / one_plus_t_vec)
    s_kp <- n/kp - sum(log1p_t_vec)
    s_ap <- (cp / ap) * ((kp + 1) * sum(t_over_one_plus_t) - n)

    # gradient on phi-scale
    g <- c(cp * s_cp, kp * s_kp, ap * s_ap)

    # Hessian of TOTAL logL on original scale
    H_cc <- -n/cp^2 - (kp + 1) * sum(t_vec * L_vec^2 / common_denom_sq)
    H_ck <- -sum(t_L_vec / one_plus_t_vec)
    H_kk <- -n/kp^2
    H_ca <- (-1/ap) * (n - (kp + 1) * sum( t_vec * (cp*L_vec + 1 + t_vec) / common_denom_sq ) )
    H_ka <- (cp / ap) * sum(t_over_one_plus_t)
    H_aa <- (-cp / ap^2) * ( (kp + 1) * sum(t_over_one_plus_t) - n + cp * (kp + 1) * sum(t_vec / common_denom_sq) )

    H_orig <- matrix(
      c(H_cc, H_ck, H_ca,
        H_ck, H_kk, H_ka,
        H_ca, H_ka, H_aa),
      nrow = 3, byrow = TRUE)

    # Hessian on phi-scale: D H_orig D + diag(g)
    D <- diag(c(cp, kp, ap))
    H_phi <- D %*% H_orig %*% D + diag(g)

    # Newton update
    delta_try <- try(solve(H_phi, g), silent = TRUE)

    if (inherits(delta_try, "try-error")) {
      warning("Hessian numerically singular in iteration ", iter, ". Stopping.")
      iter <- maxit
      break
    }
    delta <- delta_try
    phi_new <- phi - delta

    if(any(!is.finite(phi_new))) {
      warning("Non-finite parameters proposed in iteration ", iter, ". Stopping.")
      iter <- maxit
      break
    }

    cp_new <- exp(phi_new[1]); kp_new <- exp(phi_new[2]); ap_new <- exp(phi_new[3])
    new_logLik_val <- loglik(cp_new, kp_new, ap_new)

    if (trace) {
      cat(sprintf("iter=%d, max|delta|=%.3g, logLik=%.4f\n",
                  iter, max(abs(delta)), new_logLik_val))
    }

    if (max(abs(delta)) < tol) {
      if (!is.finite(new_logLik_val)){
        warning("Converged to non-finite log-likelihood. Returning last valid parameters.")
      } else {
        phi <- phi_new
        current_logLik_val <- new_logLik_val
      }
      break
    }

    if(!is.finite(new_logLik_val)){
      warning("Proposed step leads to non-finite log-likelihood in iteration ", iter, ". Stopping.")
      iter <- maxit
      break
    }

    phi <- phi_new
    current_logLik_val <- new_logLik_val

  }

  if (iter == maxit && max(abs(delta)) >= tol) {
    warning("Newton-Raphson did not converge within maxit iterations")
  }

  # return estimates in original scale
  est <- exp(phi)
  names(est) <- c("cp", "kp", "ap")

  list(
    estimates = est,
    logLik    = current_logLik_val,
    iter      = iter
  )
}


x <- actuar::rburr(100, 3, 6, scale = 17)
ml_burr_nlm(x)
ml_burr_newton(x)
ml_burr_newton(x)