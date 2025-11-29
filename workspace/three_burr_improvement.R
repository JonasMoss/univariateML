library(actuar) # for dburr()

#' Super simple NLM impl.
burr3_nlm <- function(x, start = c(1,1, median(x))) {
  nll_burr3 <- function(par, x) {
    if (any(par <= 0)) return(Inf)
    -sum(dburr(x,
               shape1 = par[1],
               shape2 = par[2],
               scale  = par[3],
               log    = TRUE))
  }
  res <- nlm(nll_burr3, p = start, x = x, hessian = TRUE)
  est <- setNames(res$estimate, c("shape1","shape2","scale"))
  list(est = est, loglik = - res$minimum, iter = res$iterations)
}

#' ML for 2-param Burr; c0 is starting param.
mlburr2 <- function(x, c0 = 0.385 / (sum(log_x * log_x) / n)^(1 / 3)) {
  # if (all(x >= 1)) {
  #   stop("The Burr maximum likelihood estimator does not exist when all observations >=1. Use `mlburr` to fit a Pareto distribution.")
  # }

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

  c0 <- 0.385 / (sum(log_x * log_x) / n)^(1 / 3)
  c <- univariateML:::newton_raphson_1d(f_over_df, c0)
  k <- n / sum(log(1 + x^c))

  c("shape1" = k, "shape2" = c)
}

x <- c(1,0.9,1.1,1.1)

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

cc <- seq(0.0000000000001, 20, length.out = 1000)
plot(cc, sapply(cc, f_over_df), type = "l")




# — One‑dimensional Newton–Raphson on σ
mlburr3 <- function(x, scale0 = median(x), tol = 1e-8, maxit = 100) {
  mlburr2 <- function(x, c0 = NULL) {
    if (all(x >= 1)) {
      stop("The Burr maximum likelihood estimator does not exist when all observations >=1. Use `mlburr` to fit a Pareto distribution.")
    }

    n <- length(x)
    log_x <- log(x)
    lx_sum <- sum(log_x)
    mu <- lx_sum / n
    if(is.null(c0)) {
      sigma2 <- sum(log_x^2) / n - mu^2
      c0 <- burr_lmom(mu, sigma2)[2]
    }
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

    #c0 <- burr_lmom(mu, sigma2)[2]
    c <- univariateML:::newton_raphson_1d(f_over_df, c0)
    k <- n / sum(log(1 + x^c))

    c("shape1" = k, "shape2" = c)
  }
  scale <- scale0
  shapes <- mlburr2(x / scale)
  params <- c(shapes, "scale" = scale)
  log_x <- log(x)
  f_over_df <- function(x, params) {
    k <- params[1]
    c <- params[2]
    sigma <- params[3]

    n <- length(x)
    #z <- x / sigma
    #log_x <- log(z)
    #x_c <- z^c

    x_c <- x^c

    div_sigma_c <- 1/sigma^c
    z_c <- x_c*div_sigma_c
    log_z <- log_x - log(sigma)


    dz_cp1 <- 1/(1 + z_c)
    z_c_div <- z_c * dz_cp1
    z_c_div2 <- z_c_div * dz_cp1

    A <- sum(z_c_div)
    B <- sum(z_c_div * log_z)
    C <- sum(z_c_div2 * log_z^2)
    D <- sum(z_c_div2)
    E <- D + c*sum(z_c_div2 * log_z) + sum(z_c_div^2)

    Hcc <- -n / c^2 - (k + 1) * C
    Hkk <- -n / k^2
    Haa <- c / sigma^2 * (n - (k + 1) * A - c * (k + 1) * D)
    Hck <- -B
    Hkc <- Hck
    Hka <- c / sigma * A
    Hak <- Hka
    Hca <- -n / sigma + (k + 1) / sigma * E
    Hac <- Hca

    H3 <- matrix(c(
      Hkk, Hck, Hka,
      Hkc, Hcc, Hca,
      Hak, Hac, Haa
    ), 3, 3, byrow = TRUE)

    Hσσ <- H3[3, 3] # now that order is (c,k,σ)
    Hσφ <- H3[3, 1:2]
    Hφσ <- H3[1:2, 3]
    Hφφ <- H3[1:2, 1:2]

    hessian <- Hσσ - as.numeric(Hσφ %*% solve(Hφφ, Hφσ))

    gradient <- (c / sigma) * (-n + (k + 1) * A)

    gradient / hessian
  }

  for (i in seq_len(maxit)) {
    delta <- f_over_df(x, params)
    scale_new <- scale - delta
    if (abs(delta) < tol) {
      scale <- unname(scale_new)
      break
    }
    scale <- unname(scale_new)
    #params <- c(mlburr2(x / scale, c0 = shapes[2]), "scale" = scale)
    params <- c(mlburr_rcpp(x / scale), "scale" = scale)
  }

  #params <- c(mlburr2(x / scale, c0 = shapes[2]), "scale" = scale)
  params <- c(mlburr_rcpp(x / scale), "scale" = scale)
  list(
    est = params,
    iter = i
  )
}

# — Example
set.seed(1)
x <- rburr(100, shape1 = 1, shape2 = 2, scale = 3)

#burr3_nlm(x)
#mlburr3(x)
#ml_burr_newton(x)
#microbenchmark::microbenchmark(burr3_nlm(x), mlburr3(x), mlburr_rcpp(x),  ml_burr_newton(x))

x <- rpareto(10, 1, 1)
eps <- 1e-1
#aa <- seq(min(x)+eps, 10, by = 0.01)
#plot(aa, sapply(aa, \(a) c(logLik(mlburr(x/a)))))

x <- rburr(100, shape1 = 1, shape2 = 2, scale = 3)
aa <- seq(min(x)+eps, 100, by = 0.01)
i <- 1
y <- rep(NA, length(aa))
z <- rep(NA, length(aa))
for (a in aa) {
  try({
    y[i] <- c(logLik(mlburr(x/a)))
    z[i] <- a
    i = i +1
  })
}


#' Calculate Profile Log-Likelihood for 3-Parameter Burr XII
#'
#' For a given scale parameter, finds the conditional MLEs for the shape
#' parameters using univariateML::mlburr on scaled data, then computes
#' the full 3-parameter log-likelihood using actuar::dburr.
#'
#' @param scale_param The value of the scale parameter (ap or sigma) to profile.
#' @param x The numeric vector of original data.
#' @return The profile log-likelihood value, or -Inf if calculation fails.
#' @details Requires the 'univariateML' and 'actuar' packages.
#'
#' @examples
#' \dontrun{
#' library(actuar)
#' library(univariateML)
#'
#' set.seed(1)
#' x_data <- rburr(100, shape1 = 2, shape2 = 3, scale = 5) # shape1=c, shape2=k
#'
#' # Evaluate at a specific scale
#' profile_loglik_burr3(scale_param = 5.5, x = x_data)
#'
#' # Vectorize for plotting
#' scales_to_try <- seq(3, 8, length.out = 100)
#' profile_vals <- sapply(scales_to_try, profile_loglik_burr3, x = x_data)
#' plot(scales_to_try, profile_vals, type = 'l',
#'      xlab = "Scale Parameter (ap)", ylab = "Profile Log-Likelihood")
#'
#' # Find maximum using optimize
#' opt_result <- optimize(profile_loglik_burr3, interval = range(scales_to_try),
#'                        x = x_data, maximum = TRUE)
#' print(opt_result)
#' abline(v = opt_result$maximum, col = "red", lty = 2)
#' }
profile_loglik_burr3 <- function(scale_param, x) {

  # Ensure required packages are available
  # (Could add explicit library calls or require() if needed outside interactive use)

  # Check for invalid scale parameter
  if (!is.numeric(scale_param) || length(scale_param) != 1 || scale_param <= 0 || !is.finite(scale_param)) {
    warning("Invalid scale_param provided.")
    return(-Inf)
  }

  # Scale the data
  y <- x / scale_param

  # Estimate shapes using mlburr on scaled data
  # Use tryCatch for robustness (e.g., mlburr fails if all y >= 1)
  mle_2p_result <- try(mlburr_rcpp(y), silent = TRUE)

  if (inherits(mle_2p_result, "try-error")) {
    # Likely Pareto case or other issue where 2p MLE doesn't exist
    return(-Inf)
  }

  # Extract estimated shapes
  # IMPORTANT: univariateML::mlburr returns shape1=k, shape2=c
  #            actuar::dburr expects shape1=c, shape2=k
  # Need to assign correctly!
  kp_hat <- mle_2p_result[1]
  cp_hat <- mle_2p_result[2]

  # Check if shapes are valid
  if (!is.finite(kp_hat) || kp_hat <= 0 || !is.finite(cp_hat) || cp_hat <= 0) {
    return(-Inf) # Invalid shape parameters estimated
  }

  # Calculate the full 3-parameter log-likelihood using original data
  # Use tryCatch again just in case dburr has issues with extreme values
  logLik_val <- try(sum(actuar::dburr(x,
                                      shape1 = kp_hat,
                                      shape2 = cp_hat,
                                      scale  = scale_param,
                                      log    = TRUE)),
                    silent = TRUE)

  if (inherits(logLik_val, "try-error") || !is.finite(logLik_val)) {
    return(-Inf) # Return -Inf if dburr fails or gives non-finite result
  }

  return(logLik_val)
}


eps = 0.000000000001
x <- rburr(10, shape1 = 1, shape2 = 2, scale = 1)
#x <- rweibull(10000, 1, 1)
#x <- extraDistr::rpareto(100, 1, 1)
aa <- seq(min(x)+eps, 1, length.out = 1000)


eps = 0.0000000000000001
x <- extraDistr::rpareto(10, 1, 1)
aa <- seq(min(x)+eps, max(x), length.out = 1000)
y <- sapply(aa, \(a) profile_loglik_burr3(a, x))
plot(aa, y, type = "l")


eps = 0.000000000000000001
x <- rweibull(10, 1, 1)
x <- rburr(10, shape1 = 1, shape2 = 2, scale = 2)


i = 109

i = i + 1
set.seed(i)
x <- extraDistr::rpareto(10, 1, 1)
aa <- seq(min(x)+eps, max(x)*20, length.out = 1000)
y <- sapply(aa, \(a) profile_loglik_burr3(a, x))
plot(aa, y, type = "l")


aa <- seq(min(x)+eps, max(x), length.out = 1000)
y <- sapply(aa, \(a) profile_loglik_burr3(a, x))
plot(aa, y, type = "l")
cc <- as.data.frame(t(sapply(aa, \(a) calcer(x/a))))
plot(aa, cc$cv, type = "l")
lines(aa, cc$mu, col = "red")
abline(h = 0)
abline(h = 1)
plot(aa, cc$c)

