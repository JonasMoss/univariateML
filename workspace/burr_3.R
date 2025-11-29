library(univariateML)   # for mlburr()
library(actuar)         # for dburr()

#— 1) Profile‐ML for 3‑param Burr using univariateML::mlburr
profile_mlburr3 <- function(x, tol = 1e-8) {
  # profile log‐lik in log‐scale‐space
  prof_ll <- function(log_lambda) {
    λ <- exp(log_lambda)
    # scale data
    y <- x/λ
    # 2‑param shapes (shape1=c, shape2=k)
    shp <- mlburr(y)
    c <- shp["shape1"]
    k <- shp["shape2"]
    # full log‑lik at (c,k,λ)
    sum(dburr(x, shape1=c, shape2=k, scale=λ, log=TRUE))
  }
  # bracket over a reasonable range
  # e.g. λ in [min(x)/10, max(x)*10] in log‐space
  rng <- log(range(x) * c(0.1, 10))
  opt <- optimize(prof_ll, interval = rng, maximum = TRUE, tol = tol)

  λ_hat <- exp(opt$maximum)
  shp   <- mlburr(x/λ_hat)
  c_hat <- shp["shape1"]
  k_hat <- shp["shape2"]

  list(
    scale   = λ_hat,
    shape1  = c_hat,
    shape2  = k_hat,
    logLik  = opt$objective
  )
}

# Example:
# set.seed(42)
# x <- rburr(200, shape1=2, shape2=3, scale=5)  # or your data
# profile_mlburr3(x)


burr3_nlm <- function(x, start = c(1,1, median(x))) {
  #— 2) Direct 3‑param Burr via nlm + actuar::dburr
  nll_burr3 <- function(par, x) {
    # par = c(shape1, shape2, scale)
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
  #list(est, res)
  # vc  <- solve(res$hessian)
  # se  <- sqrt(diag(vc))
  # list(
  #   estimate  = est,
  #   se        = setNames(se, names(est)),
  #   logLik    = -res$minimum,
  #   convergence = res$code
  # )
}


library(univariateML)  # for mlburr()
library(actuar)        # for dburr()
profile_newton_sigma <- function(x,
                                 sigma_init = 1,
                                 tol        = 1e-8,
                                 maxit      = 20000,
                                 trace      = FALSE) {
  library(univariateML)  # for mlburr()
  library(actuar)        # for dburr()

  n     <- length(x)
  sigma <- sigma_init

  for (i in seq_len(maxit)) {
    # 1) inner 2‑param fit at current sigma
    shp <- mlburr(x / sigma)
    c   <- shp["shape1"]
    k   <- shp["shape2"]

    # 2) compute A_i and helper sums
    A     <- (x / sigma)^c
    sum_z1 <- sum(A / (1 + A))                 # for p'

    # 3) envelope gradient p'(σ)
    grad  <- (c / sigma) * ( -n + (k + 1) * sum_z1 )

    # 4) envelope Hessian p''(σ)
    #   term1 = n * c / σ^2
    #   term2 = (k+1) * c / σ^2 * sum[ A_i*(c+1+A_i)/(1+A_i)^2 ]
    term1 <- n * c / sigma^2
    term2 <- (k + 1) * c / sigma^2 *
      sum( A * (c + 1 + A) / (1 + A)^2 )
    hess  <- term1 - term2

    # 5) Newton update
    delta <- grad / hess
    sigma_new <- sigma - delta

    if (trace) {
      cat(sprintf("iter %2d: σ=%.5g  Δ=%.2e  logLik=%.5g\n",
                  i, sigma_new, delta,
                  sum(dburr(x,
                            shape1 = c,
                            shape2 = k,
                            scale  = sigma_new,
                            log    = TRUE))))
    }

    if (abs(delta) < tol) {
      sigma <- sigma_new
      break
    }
    sigma <- sigma_new
  }
  if (i == maxit) warning("profile Newton did not converge in ", maxit, " iters")

  # final shapes at converged sigma
  shp_final <- mlburr(x / sigma)
  c_hat     <- shp_final["shape1"]
  k_hat     <- shp_final["shape2"]
  logLik    <- sum(dburr(x,
                         shape1 = c_hat,
                         shape2 = k_hat,
                         scale  = sigma,
                         log    = TRUE))

  list(scale   = sigma,
       shape1  = c_hat,
       shape2  = k_hat,
       logLik  = logLik,
       iter    = i)
}


x <- actuar::rburr(100, 1, 6, scale = 2)
profile_newton_sigma(x, maxit = 10, trace = TRUE)
burr3_nlm(x)

#microbenchmark::microbenchmark(burr3_nlm(x),
#profile_mlburr3(x))


# Install if needed:
# install.packages(c("univariateML","actuar","numDeriv"))

library(univariateML)  # for mlburr()
library(actuar)        # for dburr()
library(numDeriv)      # for grad() & hessian()

#— 1) Profile log‑likelihood in σ
prof_loglik <- function(sigma, x) {
  shp <- mlburr(x / sigma)
  c   <- shp["shape1"]
  k   <- shp["shape2"]
  sum(dburr(x,
            shape1 = c,
            shape2 = k,
            scale  = sigma,
            log    = TRUE))
}

#— 2) Analytical gradient & Hessian via envelope theorem
prof_derivs <- function(sigma, x) {
  n   <- length(x)
  shp <- mlburr(x / sigma)
  c   <- shp["shape1"]
  k   <- shp["shape2"]

  A   <- (x / sigma)^c
  sum_z1 <- sum(A / (1 + A))
  sum_term <- sum(A * (c + 1 + A) / (1 + A)^2)

  # gradient p'(σ)
  grad <- (c / sigma) * ( -n + (k + 1) * sum_z1 )

  # Hessian p''(σ)
  hess <- (n * c / sigma^2) - ((k + 1) * c / sigma^2) * sum_term

  list(grad = grad, hess = hess)
}

#— 3) Wrapper for numDeriv
prof_grad_num  <- function(sigma, x) grad(function(s) prof_loglik(s, x), sigma)
prof_hess_num <- function(sigma, x) hessian(function(s) prof_loglik(s, x), sigma)

#— 4) Quick check on simulated data
set.seed(123)
x    <- actuar::rburr(500, shape1 = 1.5, shape2 = 2.5, scale = 4)
sigma0 <- 3.5  # test point

# Compute analytic
an  <- prof_derivs(sigma0, x)
# Compute numeric
gn  <- prof_grad_num(sigma0, x)
hn  <- prof_hess_num(sigma0, x)

# Show comparison
cat(sprintf("Profile LL at σ=%.3f: %.5f\n", sigma0, prof_loglik(sigma0, x)))
cat(sprintf("Gradient:   analytic = %+ .6f  numeric = %+ .6f\n", an$grad, gn))
cat(sprintf("Hessian:    analytic = %+ .6f  numeric = %+ .6f\n", an$hess, hn))


# install.packages(c("univariateML","actuar","numDeriv"))
library(univariateML)  # for mlburr()
library(actuar)        # for dburr()
library(numDeriv)      # for grad() & hessian()

# 1) Profile log‑likelihood in sigma
prof_loglik <- function(sigma, x) {
  shp <- mlburr(x / sigma)
  k   <- shp["shape1"]
  c   <- shp["shape2"]
  sum(dburr(x,
            shape1 = k,
            shape2 = c,
            scale  = sigma,
            log    = TRUE))
}

# 2) Analytic gradient & Hessian (envelope theorem)
prof_derivs <- function(sigma, x) {
  n   <- length(x)
  shp <- mlburr(x / sigma)
  k   <- shp["shape1"]
  c   <- shp["shape2"]

  A      <- (x / sigma)^c
  sum_u  <- sum(A / (1 + A))        # Σ A/(1+A)
  sum_v  <- sum(A / (1 + A)^2)      # Σ A/(1+A)^2

  # p'(σ)
  grad <- (c / sigma) * ( -n + (k + 1) * sum_u )

  # p''(σ)
  hess <- (n * c / sigma^2)
  - ((k + 1) * c / sigma^2) * sum_u
  - ((k + 1) * c^2 / sigma^2) * sum_v

  list(grad = grad, hess = hess)
}


# 3) Numeric versions via numDeriv
prof_grad_num  <- function(sigma, x) grad( function(s) prof_loglik(s, x), sigma )
prof_hess_num  <- function(sigma, x) hessian( function(s) prof_loglik(s, x), sigma )

# 4) Quick test on simulated data
set.seed(121)
x      <- rburr(500, shape1 = 1.5, shape2 = 2.5, scale = 4)
sigma0 <- 4

# analytic
ad <- prof_derivs(sigma0, x)
# numeric
gn <- prof_grad_num(sigma0, x)
hn <- prof_hess_num(sigma0, x)

cat(sprintf("Profile LL   at σ=%.3f: %.6f\n", sigma0, prof_loglik(sigma0, x)))
cat(sprintf("Gradient     analytic = %+ .6f  numeric = %+ .6f\n", ad$grad, gn))
cat(sprintf("Hessian      analytic = %+ .6f  numeric = %+ .6f\n", ad$hess, hn))


