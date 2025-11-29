mlgumbel_estimate_logsigma <- function(x, ...) {
  x2      <- x^2
  reltol  <- .Machine$double.eps^0.5
  iterlim <- 100

  sigma_init <- sqrt(sum(x2) / length(x)) * 2.45/pi
  theta0     <- log(sigma_init)

  f_over_df<- function(t0) {
    s0   <- exp(t0)
    neg1 <- -1 / s0
    exps <- exp(x * neg1)
    psi0 <- sum(exps)
    psi1 <- sum(x * exps) / psi0
    psi2 <- sum(x2 * exps) / psi0 - psi1^2
    f <- s0 + psi1
    df <- s0 + 1/s0 * psi2  # f'(sigma)
    f / df
  }

  for (i in seq_len(iterlim)) {
    theta <- theta0 - f_over_df(theta0)
    if (abs(theta - theta0) < abs(theta0) * reltol) break
    theta0 <- theta
  }

  sigma <- exp(theta0)
  neg1 <- -1 / sigma
  psi0 <- sum(exp(x * neg1))
  mu   <- -sigma * log(psi0 / length(x))

  c(mu, sigma, iterations = i)
}

mlgumbel_estimate_halley_logsigma <- function(x, ...) {
  x2      <- x^2
  reltol  <- .Machine$double.eps^0.5
  iterlim <- 100

  sigma_init <- sqrt(sum(x2) / length(x)) * 2.45/pi * (1 - 1/length(x))
  theta0     <- log(sigma_init)

  for (i in seq_len(iterlim)) {
    # current sigma
    s0   <- exp(theta0)
    neg1 <- -1/s0
    exps <- exp(x * neg1)
    psi0 <- sum(exps)
    psi1 <- sum(x * exps)/psi0
    psi2 <- sum(x2 * exps)/psi0 - psi1^2
    # third central moment m3 = raw3 - 3 psi1 raw2 + 2 psi1^3
    raw2 <- sum(x2 * exps)/psi0
    raw3 <- sum((x^3) * exps)/psi0
    m3   <- raw3 - 3*psi1*raw2 + 2*psi1^3
    # f(s), f'(s), f''(s)
    f    <- s0 + psi1
    fp   <- 1 + psi2/s0^2
    fpp  <- m3/s0^4 - 2*psi2/s0^3
    # Halley update on s: s_new = s - (2 f fp)/(2 fp^2 - f fpp)
    s_new <- s0 - (2 * f * fp)/(2 * fp^2 - f * fpp)
    theta <- log(s_new)
    if (abs(theta - theta0) < abs(theta0) * reltol) break
    theta0 <- theta
  }
  sigma <- exp(theta0)
  neg1  <- -1/sigma
  psi0  <- sum(exp(x * neg1))
  mu    <- -sigma * log(psi0 / length(x))
  c(mu, sigma, iterations = i)
}

mlgumbel_estimate_newt_then_halley <- function(x, ...) {
  x2      <- x^2
  reltol  <- .Machine$double.eps^0.5
  iterlim <- 100

  # initial log-scale theta
  sigma0 <- sqrt(sum(x2)/length(x)) * 2.45/pi * (1 - 1/length(x))
  theta  <- log(sigma0)

  # one Newton step on log-scale
  s0    <- exp(theta); neg1 <- -1/s0
  exps  <- exp(x * neg1)
  psi0  <- sum(exps); psi1 <- sum(x * exps)/psi0; psi2 <- sum(x2 * exps)/psi0 - psi1^2
  f     <- s0 + psi1; df_ds <- 1 + psi2/s0^2
  theta <- theta - f/(df_ds * s0)

  # Halley iterations until convergence
  for (j in seq_len(iterlim)) {
    s0    <- exp(theta); neg1 <- -1/s0
    exps  <- exp(x * neg1)

    psi0 <- sum(exps);
    psi1 <- sum(x * exps)/psi0;

    raw2  <- sum(x2 * exps)/psi0
    psi2 <- raw2 - psi1^2
    raw3 <- sum(x^3 * exps)/psi0

    m3    <- raw3 - 3 * psi1 * raw2 + 2 * psi1^3
    f     <- s0 + psi1; fp <- 1 + psi2/s0^2; fpp <- m3/s0^4 - 2 * psi2/s0^3
    s_new <- s0 - (2 * f * fp)/(2 * fp^2 - f * fpp)
    theta_new <- log(s_new)
    if (abs(theta_new - theta) < abs(theta) * reltol) {
      theta <- theta_new
      break
    }
    theta <- theta_new
  }

  sigma <- exp(theta)
  mu    <- {
    neg1 <- -1/sigma; psi0 <- sum(exp(x * neg1))
    -sigma * log(psi0/length(x))
  }
  c(mu, sigma, iterations = 1 + j)
}

mlgumbel_estimate_fisherscoring <- function(x, steps=10, ...) {
  x2     <- x^2
  n      <- length(x)
  reltol <- .Machine$double.eps^0.5

  # initial theta
  sigma0 <- sqrt(sum(x2)/n) * 2.45/pi * (1 - 1/n)
  theta  <- log(sigma0)

  for (i in seq_len(steps)) {
    s0    <- exp(theta)
    neg1  <- -1/s0
    exps  <- exp(x * neg1)
    psi0  <- sum(exps)
    # back-calculate mu; here mu cancels via centering usage
    mu    <- -s0 * log(psi0 / n)
    xp    <- x - mu

    # Compute score U_σ at current sigma
    U_sigma <- -n/s0 + sum(xp)/s0^2 - sum(xp * exp(-xp/s0))/s0^2
    # Profile score on theta: U_θ = U_σ * σ
    U_theta <- U_sigma * s0
    # Fisher information on theta: I_θθ = n * π^2/6
    I_theta <- n * pi^2/6

    # Fisher scoring update
    delta    <- U_theta / I_theta
    theta    <- theta + delta
    if (abs(delta) < abs(theta) * reltol) break
  }

  sigma <- exp(theta)
  # final mu
  mu    <- {
    neg1 <- -1/sigma
    psi0 <- sum(exp(x * neg1))
    -sigma * log(psi0 / n)
  }
  c(mu, sigma, iterations = i)
}


mlgumbel_estimate_log_abomination <- function(x, ...) {
  x2      <- x^2
  reltol  <- .Machine$double.eps^0.5
  iterlim <- 100

  sigma_init <- sqrt(sum(x2) / length(x)) * 2.45/pi
  theta0     <- log(sigma_init)

  f_over_df<- function(t0) {
    s0   <- exp(t0)
    neg1 <- -1 / s0
    exps <- exp(x * neg1)
    psi0 <- sum(exps)
    psi1 <- sum(x * exps) / psi0
    f <- s0 + psi1
    df <- n * pi^2 / 6
    f / df
  }

  for (i in seq_len(iterlim)) {
    theta <- theta0 - f_over_df(theta0)
    if (abs(theta - theta0) < abs(theta0) * reltol) break
    theta0 <- theta
  }

  sigma <- exp(theta0)
  neg1 <- -1 / sigma
  psi0 <- sum(exp(x * neg1))
  mu   <- -sigma * log(psi0 / length(x))

  c(mu, sigma, iterations = i)
}



n <- 200
y <- extraDistr::rgumbel(n, 2, 200)
x <- y - mean(y)
mlgumbel_estimate(x)
mlgumbel_estimate_log_abomination(x)
mlgumbel_estimate_logsigma(x)
mlgumbel_estimate_halley_logsigma(x)
mlgumbel_estimate_newt_then_halley(x)
mlgumbel_estimate_fisherscoring(x)
mlgumbel_estimate_halley_logsigma(x)
sqrt(sum(x^2) / length(y)) * 2.45 / pi


n <- 500
a <- 100
b <- 7

mean(replicate(10000, {
  y <- extraDistr::rgumbel(n, a, b)
  x <- y - mean(y)
  res <- (mlgumbel_estimate_logsigma(x)[2] - sqrt(sum(x^2) / length(y)) * 2.45 / pi)^2
  res
})) / b^2


#set.seed(6)
y <- rnorm(n, 0, 1)
# inject two extreme outliers
out_idx <- sample(seq_along(y), 2)
y[out_idx] <- y[out_idx] * c(50, -50)
y

mlgumbel_(y)
x <- y - mean(y)
mlgumbel_estimate(x)
mlgumbel_estimate_logsigma(x)
sqrt(sum(x)^2) / length(y) * 2.45 / pi * (1 - 1 / length(y))





# gumbel_edge_case_tests.R
# -------------------------------------------------------------------------
# This script tests the Newton–Raphson Gumbel MLE (mlgumbel_) on various
# edge cases where convergence may be slow or problematic.


# 2) Scenario generators
scenario_funcs <- list(
  # 1) Light-tailed symmetric
  Normal           = function(n) rnorm(n, 0, 1),
  Logistic          = function(n) rlogis(n, 0, 1),

  # 2) Heavy but sub-exponential
  LogNormal        = function(n) rlnorm(n, meanlog = 0, sdlog = 1),

  # 3) Weibull far from 1
  Weibull_shape2   = function(n) rweibull(n, shape = 2, scale = 1),
  Weibull_shape0.5 = function(n) rweibull(n, shape = 0.5, scale = 1),

  # 4) Uniform bounded
  Uniform          = function(n) runif(n, 0, 1),

  # 5) Bimodal mixture
  MixtureNormals   = function(n) {
    idx <- rbinom(n, 1, 0.5)
    mu1 <- -2; sd1 <- 0.5
    mu2 <-  2; sd2 <- 0.5
    rnorm(n, ifelse(idx==1, mu1, mu2), ifelse(idx==1, sd1, sd2))
  },

  # 6) Small sample size
  SmallSample_n10  = function(n) rnorm(n, 0, 1),

  # 7) Discrete/tied data
  DiscreteTies     = function(n) floor(rnorm(n, 0, 1)*10)/10,

  # 8) Extreme outliers
  WithOutliers     = function(n) {
    x <- rnorm(n, 0, 1)
    # inject two extreme outliers
    out_idx <- sample(seq_along(x), 2)
    x[out_idx] <- x[out_idx] * c(50, -50)
    x
  },

  # 9) Very small or large true beta
  GumbelScaleSmall = function(n) {
    u    <- runif(n)
    mu   <- 0; beta <- 0.01
    mu - beta * log(-log(u))
  },
  GumbelScaleLarge = function(n) {
    u    <- runif(n)
    mu   <- 0; beta <- 100
    mu - beta * log(-log(u))
  }
)

# 3) Run tests
n_default <- 100
results <- data.frame(
  scenario   = character(),
  n          = integer(),
  mu_hat     = numeric(),
  sigma_hat  = numeric(),
  iterations = integer(),
  logLik     = numeric(),
  error_msg  = character(),
  stringsAsFactors = FALSE
)

for (name in names(scenario_funcs)) {
  cat("Testing scenario:", name, "\n")
  n <- if (name == "SmallSample_n10") 10 else n_default
  x <- scenario_funcs[[name]](n)

  res <- tryCatch({
    est <- mlgumbel_(x)
    data.frame(
      scenario   = name,
      n          = n,
      mu_hat     = est$estimates["mu"],
      sigma_hat  = est$estimates["sigma"],
      iterations = est$iterations,
      logLik     = est$logLik,
      error_msg  = NA_character_,
      stringsAsFactors = FALSE
    )
  }, error = function(e) {
    data.frame(
      scenario   = name,
      n          = n,
      mu_hat     = NA_real_,
      sigma_hat  = NA_real_,
      iterations = NA_integer_,
      logLik     = NA_real_,
      error_msg  = e$message,
      stringsAsFactors = FALSE
    )
  })

  results <- rbind(results, res)
}

# 4) Display summary
print(results)
# Optionally write to CSV:
# write.csv(results, "gumbel_edge_case_results.csv", row.names = FALSE)
