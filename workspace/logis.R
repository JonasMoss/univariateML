mllogis_ <- function(x) {
  m <- sum(x) / length(x)
  mad <- sqrt(3)*sd(x)/pi
  f <- function(p) -sum(stats::dlogis(x, p[1], exp(p[2]), log = TRUE))
  values <- suppressWarnings(stats::nlm(f = f, p = c(m, log(mad))))
  list(
    estimates = c(values$estimate[1], exp(values$estimate[2])),
    i = values$iterations
  )
}

mllogis_iso <- function(x) {
  # data summaries
  m <- sum(x) / length(x)
  mad <- sqrt(3)*sd(x)/pi

  # constant from I_ss
  C <- 1.4273248

  # negative log-likelihood in (eta1,eta2)
  f_iso <- function(eta) {
    theta <- eta[2] * sqrt(C)      # recover theta = log s
    s     <- exp(theta)            # scale
    mu    <- eta[1] / (sqrt(3) * s)  # location
    -sum(stats::dlogis(x, loc = mu, scale = s, log = TRUE))
  }

  # Method-of-Moments start in eta
  theta0   <- log(mad)
  eta2_0   <- theta0 / sqrt(C)
  eta1_0   <- sqrt(3) * exp(theta0) * m

  # optimize
  vals <- suppressWarnings(stats::nlm(f = f_iso,
                                      p = c(eta1_0, eta2_0)))

  # map back
  theta_hat <- vals$estimate[2] * sqrt(C)
  s_hat     <- exp(theta_hat)
  mu_hat    <- vals$estimate[1] / (sqrt(3) * exp(theta_hat))

  list(
    estimates = c(mu = mu_hat, scale = s_hat),
    iterations = vals$iterations
  )
}

mlsquare <- function(x) {
  p <- c(sum(x) / length(x), sqrt(3)*sd(x)/pi)
  f <- function(p) -sum(stats::dlogis(x, p[1], p[2]^2, log = TRUE))
  values <- suppressWarnings(stats::nlm(f = f, p = c(p[1], sqrt(p[2]))))
  list(
    estimates = c(values$estimate[1], values$estimate[2]^2),
    i = values$iterations
  )
}

# hyperbolic secant
sech <- function(z) 1 / cosh(z)

# per‐observation Fisher info for θ = log(s)
I1 <- 1.42995

# -------------------------------------------------------------------
# Newton–Raphson on (μ, θ)
mllogis_newt <- function(x, tol=1e-8, maxit=100, init=NULL) {
  n <- length(x)
  # initial guesses
  if (is.null(init)) {
    mu    <- median(x)
    theta <- log( sqrt(3) * sd(x) / pi )
  } else {
    mu    <- init$mu
    theta <- init$theta
  }

  for (i in seq_len(maxit)) {
    s <- exp(theta)
    z <- (x - mu) / s

    # score components
    U1 <- sum( tanh(z/2) / s )
    U2 <- sum( z*tanh(z/2) - 1 )

    # observed‐info matrix entries
    J11 <- sum( sech(z/2)^2 ) / (2 * s^2)
    J12 <- sum( tanh(z/2)/s + z*sech(z/2)^2/(2*s) )
    J22 <- sum( z*tanh(z/2) + (z^2)*sech(z/2)^2/2 )

    J <- matrix(c(J11, J12, J12, J22), nrow=2, byrow=TRUE)
    delta <- solve(J, c(U1, U2))

    mu_new    <- mu    + delta[1]
    theta_new <- theta + delta[2]
    if (max(abs(c(mu_new-mu, theta_new-theta))) < tol) {
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


mllogis_newt_fast <- function(x, tol = 1e-8, maxit = 100) {
  n     <- length(x)
  mu    <- median(x)                  # cheap one‐time location start
  theta <- log(sd(x) * sqrt(3) / pi)  # one‐time log‐scale start

  for (i in seq_len(maxit)) {
    # avoid recomputing exp() multiple times
    inv_s  <- exp(-theta)
    inv_s2 <- inv_s * inv_s

    # vectorized z and helpers
    z   <- (x - mu) * inv_s
    h   <- z * 0.5
    th  <- tanh(h)
    c2  <- 1 / (cosh(h) * cosh(h))    # sech^2(h)

    # score
    U1 <- sum(th) * inv_s
    U2 <- sum(z * th) - n

    # observed‐info entries
    sum_c2   <- sum(c2)
    sum_tz   <- sum(th + 0.5 * z * c2)
    J11      <- 0.5 * sum_c2 * inv_s2   # = sum(sech^2)/ (2 s^2)
    J12      <- inv_s * sum_tz
    J22      <- sum(z * th + h * z * c2)

    # solve [J11 J12; J12 J22] [dμ; dθ] = [U1; U2] by hand
    det      <- J11 * J22 - J12 * J12
    dmu      <- ( J22 * U1 - J12 * U2) / det
    dtheta   <- (-J12 * U1 + J11 * U2) / det

    # check convergence
    if (max(abs(dmu), abs(dtheta)) < tol) {
      mu    <- mu + dmu
      theta <- theta + dtheta
      break
    }

    mu    <- mu + dmu
    theta <- theta + dtheta
  }

  list(
    estimates  = c(mu = mu, scale = exp(theta)),
    iterations = i
  )
}


# -------------------------------------------------------------------
# Fisher scoring on (μ, θ)
mllogis_fish <- function(x, tol=1e-8, maxit=100, init=NULL) {
  n <- length(x)
  if (is.null(init)) {
    mu    <- median(x)
    theta <- log( sqrt(3) * sd(x) / pi )
  } else {
    mu    <- init$mu
    theta <- init$theta
  }

  for (i in seq_len(maxit)) {
    s <- exp(theta)
    z <- (x - mu) / s

    # score components
    U1 <- sum( tanh(z/2) / s )
    U2 <- sum( z*tanh(z/2) - 1 )

    # expected‐info updates (off‐diagonal = 0)
    delta_mu    <- (U1) / (n/(3*s^2))          # = 3*s * sum(tanh(z/2)) / n
    delta_theta <- U2 / (n * I1)

    mu_new    <- mu    + delta_mu
    theta_new <- theta + delta_theta
    if (max(abs(c(delta_mu, delta_theta))) < tol) {
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




x <- rlogis(100, 1, 3)
mllogis_iso(x)
mllogis_(x)
mlsquare(x)
mllogis_newt(x)
mllogis_fish(x)
mllogis_newt_fast(x)
mllogis_fast(x)


microbenchmark(mllogis_newt_fast(x), mllogis_fast(x))

funs <- list(
  mllogis = mllogis,
  #mllogis_iso = mllogis_iso,
  #mlsquare  = mlsquare,
  #mllogis_fish,
  mllogis_newt,
  mllogis_newt_fast,
  mllogis_fast
)


set.seed(313)
n_reps <- 10
location <- rnorm(n_reps, 0, 10)
scale    <- rgamma(n_reps, 2, 1)
n        <- sample(c(10, 100, 1000), size = n_reps, replace = TRUE)

results <- sapply(seq_len(n_reps), \(i) {
  x <- rlogis(n[i], location[i], scale[i])

  out <- sapply(funs, \(f) {
    # test once
    ok <- tryCatch({
      f(x)
      TRUE
    }, error = function(e) FALSE)

    if (!ok) {
      NA_real_
    } else {
      bench <- microbenchmark::microbenchmark(f(x), times = 100L, unit = "ns")
      summary(bench)$mean
    }
  })

  out
})


results2 <- data.frame(t(results))
colMeans(results2)