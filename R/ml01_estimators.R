# This file is auto-generated from r-src via scripts/gen_estimators_from_rsrc.R
# Do not edit by hand.

mlbeta_ <- function(x, ...) {
  n <- length(x)
  s <- sum(log(x)) / n
  r <- sum(log1p(-x)) / n

  reltol <- .Machine$double.eps^0.5
  iterlim <- 100

  m <- sum(x) / n
  v <- sum(x^2) / n - m^2
  nu <- m * (1 - m) / v - 1
  b <- log((1 - m) * nu)

  for (i in seq(iterlim)) {
    exp_b <- exp(b)
    digamma_b_r <- digamma(exp_b) - r
    trigamma_b <- trigamma(exp_b)

    # Inverts digamma(s-r+digamma(exp(b)))
    y <- s + digamma_b_r
    a <- 1 / log(1 + exp(-y))
    a <- a + (y - digamma(a)) / trigamma(a)
    a <- a + (y - digamma(a)) / trigamma(a)
    a <- a + (y - digamma(a)) / trigamma(a)

    # Construct derivative and hessian
    f <- exp_b * (digamma_b_r - digamma(a + exp_b))
    df <- f + exp_b^2 * (trigamma_b - trigamma(a + exp_b) * (trigamma_b / trigamma(a) + 1))

    b0 <- b - f / df
    if (abs((b0 - b) / b0) < reltol) break
    b <- b0
  }

  list(
    estimates = c(a, exp_b),
    logLik = n * ((a - 1) * s + (exp_b - 1) * r - lbeta(a, exp_b)),
    i = i
  )
}


mlbetapr_ <- function(x, ...) {
  estimates <- mlbeta(x / (x + 1), na.rm = FALSE, ...)
  alpha <- estimates[1]
  beta <- estimates[2]
  logLik <- unname(-length(x) * (lbeta(alpha, beta) -
    (alpha - 1) * mean(log(x)) +
    (alpha + beta) * mean(log(1 + x))))
  list(estimates = estimates, logLik = logLik)
}

mlbinom_ <- function(x, ...) {
  dots <- list(...)

  n <- length(x)
  x_bar <- mean(x)
  n <- length(x)
  counts <- Rfast::Table(x)
  uniques <- as.numeric(names(counts))

  log_likelihood <- function(size) {
    n * x_bar * log(x_bar) +
      n * (size - x_bar) * log(size - x_bar) -
      n * size * log(size) +
      sum(counts * lchoose(size, uniques))
  }

  if (!is.null(dots$size)) {
    size <- dots$size
    if (size < max(x)) stop("`size` is smaller than `max(x)`.")
    prob <- x_bar / size
    return(list(estimates = c(size, prob), logLik = log_likelihood(size)))
  }

  if (mean(x) / (stats::var(x) * (n - 1) / n) <= 1) {
    stop("The maximum likelihood estimator does not exist. Use `mlpois` to fit a Poisson or supply a `size` argument.")
  }

  f_over_df <- function(size) {
    size_m_x <- size - x_bar
    f <- n * (log(size_m_x) - log(size)) +
      n * digamma(size + 1) -
      sum(counts * digamma(size - uniques + 1))

    df <- n * x_bar / (size * size_m_x) +
      n * trigamma(size + 1) -
      sum(counts * trigamma(size - uniques + 1))

    f / df
  }

  size0 <- max(x)
  size_cands <- size0
  if (size0 != 1) {
    if (f_over_df(size0) > 0) {
      size_cand <- newton_raphson_1d(f_over_df, size0)
      size_cands <- c(size0, floor(size_cand), ceiling(size_cand))
    }
  }

  log_liks <- sapply(size_cands, log_likelihood)
  size <- size_cands[which.max(log_liks)]

  list(
    estimates = c(size = size, prob = x_bar / size),
    logLik = max(log_liks)
  )
}

mlburr_ <- function(x, ...) {
  if (all(x >= 1)) {
    stop("The Burr maximum likelihood estimator does not exist when all observations >=1. Use `mlpareto` to fit a Pareto distribution.")
  }

  n <- length(x)
  log_x <- log(x)
  lx_sum <- sum(log_x)
  f_over_df <- function(c) {
    x_c <- x^c
    x_c_div <- x_c / (x_c + 1)
    g <- sum(log(1 + x_c))
    dg <- sum(log_x * x_c_div)
    d2g <- sum(log_x^2 * x_c_div) - sum((log_x * x_c_div)^2)
    f <- n / c - n * dg / g + lx_sum - dg
    df <- -n / c^2 - n * (g * d2g - dg^2) / g^2 - d2g
    f / df
  }

  mu <- lx_sum / n
  sigma2 <- sum(log_x^2) / n - mu^2
  c0 <- burr_lmom(mu, sigma2)[2]
  c <- newton_raphson_1d(f_over_df, c0, ...)
  k <- n / sum(log(1 + x^c))

  list(
    estimates = c(k, c),
    logLik = sum(actuar::dburr(x, k, c, log = TRUE))
  )
}

mlcauchy_ <- function(x, ...) {
  m <- stats::median(x)
  mad <- stats::median(abs(x - m))
  start <- c(m, mad)

  f <- function(p) -sum(stats::dcauchy(x, p[1], exp(p[2]), log = TRUE))
  values <- suppressWarnings(stats::nlm(f = f, p = start))

  list(
    estimates = c(values$estimate[1], exp(values$estimate[2])),
    logLik = -values$minimum
  )
}

mldunif_ <- function(x, ...) {
  n <- length(x)
  estimates <- c(min(x), max(x))
  list(
    estimates = estimates,
    logLik = -n * log(estimates[2] - estimates[1] + 1)
  )
}

mlexp_ <- function(x, ...) {
  estimates <- 1 / mean(x)
  logLik <- length(x) * (log(estimates) - 1)
  list(estimates = estimates, logLik = logLik)
}

mlfatigue_ <- function(x, ...) {
  n <- length(x)
  x_bar <- sum(x) / n
  x_inv_bar <- n / sum(1 / x)

  f_over_df <- function(beta) {
    xi <- n / sum(1 / (x + beta))
    dxi <- sum(1 / (x + beta)^2) * xi^2 / n
    f <- beta^2 - beta * (2 * x_inv_bar + xi) + x_inv_bar * (x_bar + xi)
    df <- 2 * beta - (2 * x_inv_bar + xi) - beta * dxi + x_inv_bar * dxi
    f / df
  }

  # Starting parameter: Method of moments solution.
  sqrt_x <- sqrt(x)
  beta0 <- sum(sqrt_x) / sum(1 / sqrt_x)
  beta <- newton_raphson_1d(f_over_df, beta0, ...)

  alpha <- sqrt(x_bar / beta + beta / x_inv_bar)

  estimates <- c(alpha = alpha, beta = beta, mu = 0)
  logLik <- sum(extraDistr::dfatigue(x, alpha, beta, 0, log = TRUE))
  list(estimates = estimates, logLik = logLik)
}

mlgamma_ <- function(x, ...) {
  n <- length(x)
  mean_hat <- sum(x) / n
  lx_bar <- sum(log(x)) / n
  s <- log(mean_hat) - lx_bar

  f_over_df <- function(shape0) {
    (log(shape0) - digamma(shape0) - s) / (1 / shape0 - trigamma(shape0))
  }

  shape0 <- 1 / (12 * s) * (3 - s + sqrt((s - 3)^2 + 24 * s)) * 0.9989 + 0.0102
  shape <- newton_raphson_1d(f_over_df, shape0, ...)
  rate <- shape / mean_hat

  estimates <- c(shape = shape, rate = rate)
  logLik <- n * (shape * log(rate) - lgamma(shape) + (shape - 1) * lx_bar - rate * mean_hat)

  list(estimates = estimates, logLik = logLik)
}

mlged_ <- function(x, ...) {
  fit <- suppressWarnings(fGarch::gedFit(x))
  list(estimates = fit[["par"]], logLik = -fit$objective)
}

mlgeom_ <- function(x, ...) {
  n <- length(x)
  prob <- 1 / (mean(x) + 1)
  logLik <- sum(x * log(1 - prob)) + n * log(prob)
  list(estimates = prob, logLik = logLik)
}

mlgompertz_ <- function(x, ...) {
  n <- length(x)
  x_sum <- sum(x)
  x_var <- stats::var(x) * (n - 1) / n
  if (x_sum / n < sqrt(x_var)) {
    stop("The maximum likelihood estimator of the b parameter Gompertz distribution does not exist since the data is overdispersed. The exponential distribution will yield a better fit.")
  }

  f_over_df <- function(b) {
    exp_bx <- exp(b * x)
    sum_bx <- sum(exp_bx)
    sum_bx_x <- sum(x * exp_bx)
    sum_bx_x2 <- sum(x^2 * exp_bx)
    s <- sum_bx - n
    r <- b * sum_bx_x - s

    a <- b * n / s
    da <- -n * r / s^2
    d2a <- n / s^2 * (b * (2 * sum_bx_x^2 / s - sum_bx_x2) - 2 * sum_bx_x)

    f <- n * da / a + x_sum
    df <- n * (a * d2a - da^2) / a^2
    f / df
  }

  b <- newton_raphson_1d(f_over_df, 10e-2, ...)

  a <- b * n / (sum(exp(b * x)) - n)
  a <- max(a, 1e-09)

  list(
    estimates = c(a, b),
    logLik = sum(extraDistr::dgompertz(x, a, b, log = TRUE))
  )
}

mlgumbel_ <- function(x, ...) {
  x_bar <- sum(x) / length(x)
  estimates <- mlgumbel_estimate(x - x_bar, ...)
  mu <- estimates[1] + x_bar
  sigma <- estimates[2]
  logLik <- sum(extraDistr::dgumbel(x, mu, sigma, log = TRUE))
  list(estimates = c(mu = mu, sigma = sigma), logLik = logLik, iterations = estimates[3])
}

mlgumbel_estimate <- function(x, ...) {
  x2 <- x^2
  dots <- list(...)
  reltol <- if (!is.null(dots$reltol)) dots$reltol else .Machine$double.eps^0.25
  iterlim <- if (!is.null(dots$iterlim)) dots$iterlim else 100

  f_over_df <- function(sigma0) {
    neg_sigma_inv <- -1 / sigma0
    exps <- exp(x * neg_sigma_inv)
    psi0 <- sum(exps)
    psi1 <- sum(x * exps) / psi0
    psi2 <- sum(x2 * exps) / psi0 - psi1^2
    f <- sigma0 + psi1
    df <- 1 + neg_sigma_inv^2 * psi2
    f / df
  }

  sigma0 <- sqrt(sum(x2) / length(x)) * 2.45 / pi
  sigma <- sigma0

  for (i in seq_len(iterlim)) {
    sigma_new <- sigma - f_over_df(sigma)
    if (is.nan(sigma_new)) stop("NaN in Newton--Raphson iteration.")
    if (abs((sigma_new - sigma) / sigma_new) < reltol) {
      sigma <- sigma_new
      break
    }
    sigma <- sigma_new
  }

  if (i == iterlim) {
    warning(paste0(
      "The iteration limit (iterlim = ", iterlim, ") was reached",
      " before the relative tolerance requirement (reltol = ",
      reltol, ")."
    ))
  }

  neg_sigma_inv <- -1 / sigma
  psi0 <- sum(exp(x * neg_sigma_inv))
  mu <- -sigma * log(psi0 / length(x))
  c(mu, sigma, iterations = i)
}

mlinvburr_ <- function(x, ...) {
  params <- mlburr_(1/x)$estimates
  list(
    estimates = params,
    logLik = sum(actuar::dinvburr(x, params[1], params[2], log = TRUE))
  )
}

mlinvgamma_ <- function(x, ...) {
  gamma_ml <- mlgamma_(1 / x, ...)
  alpha <- gamma_ml$estimates[1]
  beta <- gamma_ml$estimates[2]

  L <- mean(log(x))
  M <- mean(1 / x)
  logLik <- length(x) * (alpha * log(beta) - log(gamma(alpha)) - (alpha + 1) * L - beta * M)

  list(estimates = gamma_ml$estimates, logLik = unname(logLik))
}

mlinvgauss_ <- function(x, ...) {
  n <- length(x)
  mu <- mean(x)
  lambda <- 1 / (mean(1 / x) - 1 / mu)
  estimates <- c(mu, lambda)

  L <- mean(log(x))
  S <- mean((x - mean(x))^2 / x)

  logLik <- -n / 2 * (3 * L - log(lambda) + log(2 * pi) + lambda / mu^2 * S)
  list(estimates = estimates, logLik = logLik)
}

mlinvweibull_ <- function(x, ...) {

  log_x <- log(x)
  n <- length(x)
  s <- -sum(log_x) / n

  estimates <- mlgumbel_estimate(log_x + s, ...)
  rate_log <- -estimates[1] + s

  rate <- exp(rate_log)
  shape <- 1 / estimates[2]

  #r <- sum(exp(-log_x * shape)) / n
  #scale <- 1/rate
  #r <- (scale^shape)^2
  #r <- scale^shape * r
  # r <- 1

  logLik <- n * (log(shape) + shape * (-rate_log + s) - 1 + s)
  list(estimates = c(shape, rate), logLik = logLik)
}

kumar_lmom <- function(mu, sigma2) {
  neuler <- digamma(1) # Negative Euler-Mascheroni constant
  basel <- pi^2 / 6 # trigamma(1)

  f_over_df <- function(a) {
    b <- inverse_digamma(neuler - a * mu)
    tri <- trigamma(b)
    tetra <- psigamma(b, deriv = 2)

    f <- a * tri * (basel - tri - a^2 * sigma2)
    df <- a * tetra * mu - 2 * tri * (basel + tri)
    f / df
  }

  reltol <- .Machine$double.eps^0.5
  iterlim <- 100
  a0 <-  sqrt(basel / sigma2)
  for (i in seq(iterlim)) {
    a <- a0 - f_over_df(a0)
    if (abs(a0 - a) < abs(a0) * reltol) break
    a0 <- a
  }

  a <- a0
  # b is inverse_digamma(neuler - a * mu) - 1
  a
}

mlkumar_ <- function(x, ...) {
  n <- length(x)
  log_x <- log(x)
  n_div <- 1 / n
  mu <- sum(log_x) * n_div
  sigma2 <- sum(log_x^2) * n_div - mu^2

  f_over_df <- function(a) {
    xa <- exp(a * log_x)
    z <- log_x / (1 - xa)

    alpha <- sum(log1p(-xa)) * n_div # Corresponds to s1a
    beta <- sum(xa * z) * n_div
    gamma <- sum(xa * z^2) * n_div

    f <- 1/a + mu + (1/alpha + 1) * beta
    df <- -1/(a^2) + (gamma * alpha + beta^2) / (alpha^2) + gamma

    f / df
  }

  a0 <- kumar_lmom(mu, sigma2)
  a0 <- if (a0 > 0) a0 else sqrt(pi^2 / (6 * sigma2))

  reltol <- .Machine$double.eps^0.5
  iterlim <- 100
  for (i in seq(iterlim)) {
    a <- a0 - f_over_df(a0)
    if (abs(a0 - a) < abs(a0) * reltol) break
    a0 <- a
  }

  a <- a0
  b <- -n / sum(log1p(-x^a))

  list(
    estimates = c(a = a, b = b),
    logLik = n * (log(a) + log(b) + (a - 1) * mu + 1 / b - 1),
    iterations = i
  )
}

mllaplace_ <- function(x, ...) {
  mu <- stats::median(x)
  sigma <- mean(abs(x - mu))
  estimates <- c(mu = mu, sigma = sigma)
  logLik <- -length(x) * (1 + log(2 * sigma))
  list(estimates = estimates, logLik = logLik)
}

mllgamma_ <- function(x, ...) {
  y <- log(x)
  n <- length(x)
  estimates <- mlgamma_(y, ...)$estimates
  shapelog <- estimates[1]
  ratelog <- estimates[2]
  L <- mean(y)
  K <- mean(log(y))
  logLik <-
    unname(n * (shapelog * log(ratelog) - log(gamma(shapelog)) +
      (shapelog - 1) * K - (ratelog + 1) * L))

  list(estimates = estimates, logLik = logLik)
}

mllgser_ <- function(x, ...) {
  if (sum(x) == length(x)) {
    stop("All observations equals 1; the maximum likelihood estimator does not exist.")
  }
  x_bar <- mean(x)
  xi <- -x_bar * pracma::lambertWn(-exp(-1 / x_bar) / x_bar) - 1
  theta <- xi / (xi + 1)
  logLik <- sum(extraDistr::dlgser(x, theta, log = TRUE))

  list(estimates = theta, logLik = logLik)
}

mlllogis_ <- function(x, ...) {
  y <- log(x)
  estimates <- mllogis_(y)$estimates
  estimates[1] <- exp(-estimates[1])
  estimates[2] <- 1 / estimates[2]
  estimates <- rev(estimates)
  logLik <- sum(actuar::dllogis(x, estimates[1], estimates[2], log = TRUE))
  list(estimates = estimates, logLik = logLik)
}

mllnorm_ <- function(x, ...) {
  y <- log(x)
  n <- length(x)
  meanlog <- mean(y)
  sdlog <- sqrt(stats::var(y) * (n - 1) / n)
  estimates <- c(meanlog = meanlog, sdlog = sdlog)
  logLik <-
    -n / 2 * (1 + log(2 * pi) + 2 * log(sdlog) + 2 * meanlog)
  list(estimates = estimates, logLik = logLik)
}

mllogis_ <- function(x, ...) {
  m <- stats::median(x)
  mad <- stats::median(abs(x - m))
  f <- function(p) -sum(stats::dlogis(x, p[1], exp(p[2]), log = TRUE))
  values <- suppressWarnings(stats::nlm(f = f, p = c(m, log(mad))))
  list(
    estimates = c(values$estimate[1], exp(values$estimate[2])),
    logLik = -values$minimum,
    i = values$iterations
  )
}

mllogitnorm_ <- function(x, ...) {
  n <- length(x)
  y <- stats::qlogis(x)
  mu <- mean(y)
  sigma <- sqrt(stats::var(y) * (n - 1) / n)

  H <- mean(log(x))
  G <- mean(log(1 - x))

  estimates <- c(mu = mu, sigma = sigma)
  logLik <-
    -n / 2 * (1 + log(2 * pi) + 2 * log(sigma) + 2 * H + 2 * G)
  list(estimates = estimates, logLik = logLik)
}

mllomax_ <- function(x, ...) {
  n <- length(x)
  m <- sum(x) / n
  s <- sum(x^2) / n
  m2 <- m^2

  if (s < 2 * m2) {
    stop("The Lomax maximum likelihood estimator does not exist for data with `mean(x^2) < 2*mean(x)^2`. Use `mlexp` to fit an exponential distribution.")
  }
  f_over_df <- function(lambda0) {
    lambda0_x1p <- 1 + lambda0 * x
    x_div <- x / lambda0_x1p
    S <- sum(log(lambda0_x1p)) / n
    S1 <- sum(x_div) / n
    S2 <- sum(x_div^2) / n

    psi <- S1 / S
    f <- 1 / lambda0 - psi - S1
    df <- psi^2 + S2 / S + S2 - 1 / lambda0^2
    f / df
  }

  alpha0 <- 2 * (s - m2) / (s - 2 * m2)
  lambda0 <- 1 / (m * (alpha0 - 1))

  lambda <- newton_raphson_1d(f_over_df, lambda0, ...)
  kappa <- n / sum(log1p(lambda * x))
  loglik <- n * (log(kappa) + log(lambda) - 1 - 1/kappa)

  list(estimates = c(lambda, kappa), logLik = loglik)
}

mlnaka_ <- function(x, ...) {
  estimates <- mlgamma_(x^2, ...)$estimates
  estimates["rate"] <- 1 / estimates["rate"] * estimates["shape"]

  n <- length(x)
  shape <- estimates[1]
  scale <- estimates[2]

  logLik <-
    unname(n * (shape * log(shape) + log(2) -
      lgamma(shape) - shape * log(scale)) +
      (2 * shape - 1) * sum(log(x)) - shape / scale * sum(x^2))

  list(estimates = estimates, logLik = logLik)
}

mlnbinom_ <- function(x, ...) {
  dots <- list(...)
  n <- length(x)
  x_bar <- mean(x)

  if (is.null(dots$size)) {
    if (x_bar == 0) {
      warning("All observations are 0; the maximum likelihood estimator is not unique.")
      size <- 1
    } else {
      x_var <- stats::var(x)

      if (x_var * (n - 1) / n < x_bar) {
        stop("The maximum likelihood estimator does not exists for underdispersed data, but converges to a Poisson. Use `mlpois` instead.")
      }

      f_over_df <- function(size) {
        prob <- size / (x_bar + size)
        dprob <- x_bar / (size^2 + x_bar * size)
        f <- sum(digamma(x + size)) - n * digamma(size) + n * log(prob)
        df <- sum(trigamma(x + size)) - n * trigamma(size) + n * dprob
        f / df
      }

      size0 <- x_bar^2 / (x_var - x_bar)
      size <- newton_raphson_1d(f_over_df, size0, ...)
    }
  } else {
    size <- dots$size
  }

  prob <- size / (x_bar + size)
  logLik <- sum(lgamma(x + size)) - sum(lfactorial(x)) - n * lgamma(size) +
    n * size * log(prob) + n * log(1 - prob) * x_bar

  list(estimates = c(size, prob), logLik = logLik)
}

mlnorm_ <- function(x, ...) {
  n <- length(x)

  mu <- mean(x)
  sigma <- sqrt(stats::var(x) * (n - 1) / n)

  estimates <- c(mu, sigma)
  logLik <- -n / 2 * (1 + log(2 * pi) + 2 * log(sigma))
  list(estimates = estimates, logLik = logLik)
}

mlparalogis_ <- function(x, ...) {
  n <- length(x)
  log_x <- log(x)
  log_x2 <- log_x^2
  s <- sum(log_x) / n
  n <- length(x)

  f_over_df <- function(shape) {
    x_shape <- x^shape
    x_div <- x_shape / (1 + x_shape)
    r <- sum(log(1 + x_shape)) / n
    r_grad <- sum(x_div * log_x) / n
    r_hessian <- sum(x_div * log_x2) / n - sum(x_div * x_div * log_x2) / n

    f <- 2 / shape + s - r - (shape + 1) * r_grad
    df <- -2 / shape^2 - 2 * r_grad - (shape + 1) * r_hessian
    f / df
  }

  # Approximate method of moments solution
  shape0 <- max(3 / (1 - mean(x)), 0.1)
  dots <- list(...)
  if (!is.null(dots$shape0)) shape0 <- dots$shape0
  shape <- newton_raphson_1d(f_over_df, shape0)
  rn <- function(shape) sum(log(1 + x^shape))
  loglik <- 2 * n * log(shape) + (shape - 1) * n * s - (shape + 1) * rn(shape)

  list(estimates = c(shape), logLik = loglik)
}

mlpareto_ <- function(x, ...) {
  m <- mean(log(x))
  b <- min(x)
  a <- 1 / (m - log(b))

  estimates <- c(a, b)
  logLik <- length(x) * (log(a) + a * log(b) - (a + 1) * m)
  list(estimates = estimates, logLik = logLik)
}

mlpois_ <- function(x, ...) {
  n <- length(x)
  lambda <- mean(x)
  logLik <- -n * lambda + sum(x) * log(lambda) - sum(lgamma(x + 1))
  list(estimates = lambda, logLik = logLik)
}

mlpower_ <- function(x, ...) {
  dots <- list(...)
  epsilon <- if (!is.null(dots$epsilon)) {
    dots$epsilon
  } else {
    .Machine$double.eps^0.5
  }

  m <- mean(log(x))
  alpha <- max(x) + epsilon
  beta <- 1 / (log(alpha) - m)

  estimates <- c(alpha, beta)
  logLik <-
    length(x) * (log(beta) - beta * log(alpha) + (beta - 1) * m)
  list(estimates = estimates, logLik = logLik)
}

mlrayleigh_ <- function(x, ...) {
  sigma <- sqrt(1 / 2 * mean(x^2))
  logLik <- length(x) * (mean(log(x) - 2 * log(sigma) - 1))
  list(estimates = sigma, logLik = logLik)
}

mlsged_ <- function(x, ...) {
  fit <- suppressWarnings(fGarch::sgedFit(x))
  list(estimates = fit[["par"]], logLik = -fit$objective)
}

mlsnorm_ <- function(x, ...) {
  fit <- suppressWarnings(fGarch::snormFit(x))
  list(estimates = fit[["par"]], logLik = -fit$objective)
}

mlsstd_ <- function(x, ...) {
  fit <- suppressWarnings(fGarch::sstdFit(x))
  list(estimates = fit[["estimate"]], logLik = -fit$minimum)
}

mlstd_ <- function(x, ...) {
  fit <- suppressWarnings(fGarch::stdFit(x))
  list(estimates = fit[["par"]], logLik = -fit$objective)
}

mlunif_ <- function(x, ...) {
  max_ <- max(x)
  min_ <- min(x)
  list(estimates = c(min_, max_), logLik = -length(x) * log(max_ - min_))
}

mlweibull_ <- function(x, ...) {
  log_x <- -log(x)
  s <- -sum(log_x) / length(x)
  estimates <- mlgumbel_estimate(log_x + s, ...)
  scale <- exp(-estimates[1] + s)[1]
  shape <- 1 / estimates[2]
  logLik <- length(x) * (log(shape / scale) + (shape - 1) * (s - log(scale)) - 1)
  list(estimates = c(shape, scale), logLik = logLik)
}

mlzip_ <- function(x, ...) {
  r_0 <- mean(x == 0)
  x_bar <- mean(x)

  if (r_0 == 1) {
    pi <- 1
    lambda <- 1
    warning("All observations equal to 0; the maximum likelihood estimator is not unique.")
  } else if (r_0 > exp(-x_bar)) {
    x_bar <- mean(x)
    gamma <- x_bar / (1 - r_0)
    lambda <- pracma::lambertWp(-gamma * exp(-gamma)) + gamma
    pi <- (r_0 - exp(-lambda)) / (1 - exp(-lambda))
  } else {
    pi <- 0
    lambda <- x_bar
  }

  logLik <- sum(extraDistr::dzip(x, lambda, pi, log = TRUE))

  list(estimates = c(lambda, pi), logLik = logLik)
}

mlzipf_ <- function(x, ...) {
  N <- max(x)
  n <- length(x)
  sum_lx <- sum(log(x))
  log_seq <- log(seq(N))
  log_seq2 <- log(seq(N))^2
  div_seq <- 1 / seq(1, N)

  if (sum_lx == 0) {
    warning("All observations are equal to 1. The maximum likelihood estimator is not unique.")
    return(list(estimates = c(N, 1), logLik = 0))
  }

  f_over_df <- function(shape0) {
    div_seq_shape0 <- div_seq^shape0
    sum_shape <- sum(div_seq_shape0)
    sum_log_hs <- sum(div_seq_shape0 * log_seq)
    sum_log2_hs <- sum(div_seq_shape0 * log_seq2)
    top <- sum_log_hs / sum_shape
    bottom <- -n * sum_log2_hs / sum_shape + n * top^2
    (n * top - sum_lx) / bottom
  }

  shape0 <- 1
  shape <- newton_raphson_1d(f_over_df, shape0, ...)

  if (shape < 0) {
    stop("Optimal shape parameter is less than 0. The maximum likelihood estimator does not exist.")
  }

  logLik <- -shape * sum_lx - n * log(sum(1 / seq(1, N)^shape))
  list(estimates = c(N, shape), logLik = logLik)
}

