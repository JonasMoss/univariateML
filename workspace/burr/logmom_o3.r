## ===============================================================
##  Finv()  --  quantile of
##
##    F(u) = ( 2 – [ ψʺ(1) – ψʺ(eᵘ) ] / ( ψʹ(1)+ψʹ(eᵘ) )^(3/2) )
##           -----------------------------------------------
##                 2 – ψʺ(1) / ψʹ(1)^(3/2)
##
##  Accurate to machine precision on the whole open unit interval.
## ===============================================================

Finv <- local({
  ## ---- 1. constants -------------------------------------------
  C <- trigamma(1) #  ψʹ(1)
  A <- psigamma(1, deriv = 2) #  ψʺ(1)   (negative)
  K <- 2 - A / C^(1.5) #  > 0

  ## left‑ & right‑tail coefficients (derived in the analysis msg)
  c.left <- K / (3 * C) #  p  ≈  c.left * t^3
  c.right <- -3 * A / (2 * K * C^(2.5)) #  t ≈  c.right / (1‑p)

  ## ---- 2. helpers ---------------------------------------------
  FofU <- function(u) {
    t <- exp(u)
    D <- psigamma(t, 1) # ψʹ(t)
    B <- psigamma(t, 2) # ψʺ(t)
    (2 - (A - B) / (C + D)^(1.5)) / K
  }

  dFdu <- function(u) { # analytic 1st derivative (corrected)
    t <- exp(u)
    D <- psigamma(t, 1) # ψʹ(t)
    B <- psigamma(t, 2) # ψʺ(t)
    B3 <- psigamma(t, 3) # ψ‴(t)
    P <- C + D
    num <- P * B3 + 1.5 * (A - B) * B
    t * num / (K * P^2.5)
  }
  const_left <- K / (6 * C) # Constant for left tail F(u) ~ exp(2u) / const_left

  ## ---- 3.  start‑ups ------------------------------------------
  # u0_left  <- function(p)  log( (p / c.left)^(1/3) )

  u0_left <- function(p) {
    # Add small epsilon to p to avoid log(0) if p is exactly 0 (though input check prevents this)
    # log(p + .Machine$double.xmin) + log(2 * K)
    p_safe <- pmax(p, .Machine$double.xmin)
    #0.5 * log(p_safe) + 0.5 * log(const_left)
    0.5 * log(p_safe * const_left)
  }
  u0_right <- function(p) log(c.right / (1 - p))

  ## minimax [3/3] rational on the logit scale, max rel err < 5e‑4
  u0_mid <- function(p) {
    s <- log(p / (1 - p))
    s2 <- s * s
    s4 <- s2 * s2
    num <- s * (0.160 + 0.0150 * s2 + 0.00042 * s4)
    den <- 1 + 0.098 * s2 + 0.00160 * s4
    num / den
  }

  ## ---- 4.  main vectorised quantile ---------------------------
  function(p, tol = 8 * .Machine$double.eps) {
    if (any(!is.finite(p)) || any(p <= 0 | p >= 1)) {
      stop("p must be strictly between 0 and 1")
    }

    ## region‑wise initial guess
    u <- numeric(length(p))
    left <- p < 0.05
    right <- p > 0.95
    mid <- !(left | right)
    u[left] <- u0_left(p[left])
    u[right] <- u0_right(p[right])
    u[mid] <- u0_mid(p[mid])

    ## safeguarded Newton (usually 2 iterations do it)
    for (k in 1:6) {
      f <- FofU(u) - p
      ok <- abs(f) > tol * pmax(p, 1 - p)
      if (!any(ok)) break
      u[ok] <- u[ok] - f[ok] / dFdu(u[ok])
    }

    # ---- 5. adaptive bracket + uniroot (rare) ------------------
    still <- abs(FofU(u) - p) > tol * pmax(p, 1 - p)
    if (any(still)) {
      print(paste0("Using uniroot for ", p, " (iter:", k))
      for (i in which(still)) {
        step <- 2 # start with ±2
        lo <- u[i] - step
        hi <- u[i] + step
        fl <- FofU(lo) - p[i]
        fh <- FofU(hi) - p[i]
        it <- 0L
        while (fl * fh > 0 && it < 25L) { # expand until signs differ
          step <- step * 2
          lo <- u[i] - step
          hi <- u[i] + step
          fl <- FofU(lo) - p[i]
          fh <- FofU(hi) - p[i]
          it <- it + 1L
        }
        u[i] <- uniroot(function(x) FofU(x) - p[i],
          interval = c(lo, hi), tol = tol
        )$root
      }
    }
    u
  }
})


kappa <- \(u) (psigamma(1, 2) - psigamma(exp(u), 2)) / (trigamma(exp(u)) + trigamma(1))^(3 / 2)
F <- \(u) (lim_neg_inf - kappa(u)) / (lim_neg_inf - lim_inf)

lim_inf <- psigamma(1, 2) / (trigamma(1))^(3 / 2)
lim_neg_inf <- 2
# y <- exp(seq(log(1e-9), log1p(-1e-9), length.out = 1000))
y <- seq(1e-15, 1 - 1e-15, length.out = 100000)
res <- sapply(y, \(y) as.numeric(try(Finv(y))))
plot(y, res, type = "l")

max(abs(F(res) - y))
max(abs((F(res) - y) / y))
max(abs((F(res) - y) / (1-y)))

# y <- seq(0.7, 1-1e-8, length.out = 100)
# res <- sapply(y, \(y) as.numeric(try(Finv(y))))
# cbind(y, F(res))


p_deep_left <- exp(seq(log(1e-16), log(.Machine$double.xmin), length.out = 20000))
p_deep_right <- 1 - exp(seq(log(1e-16), log(.Machine$double.xmin), length.out = 20000))
p_eps_left <- exp(seq(log(1e-20), log(1e-14), length.out = 20000))
p_eps_right <- 1 - exp(seq(log(1e-14), log(1e-20), length.out = 20000))
p_linear <- seq(1e-14, 1 - 1e-14, length.out = 20000)
p_specific <- c(
  .Machine$double.xmin,
  10 * .Machine$double.xmin,
  sqrt(.Machine$double.xmin),
  .Machine$double.eps / 2,
  .Machine$double.eps,
  2 * .Machine$double.eps,
  sqrt(.Machine$double.eps),
  0.001, 0.5, 0.999,
  1 - sqrt(.Machine$double.eps),
  1 - 2 * .Machine$double.eps,
  1 - .Machine$double.eps,
  1 - .Machine$double.eps / 2,
  1 - sqrt(.Machine$double.xmin),
  1 - 10 * .Machine$double.xmin,
  1 - .Machine$double.xmin
)

y_test <- sort(unique(c(p_deep_left, p_eps_left, p_linear, p_eps_right, p_deep_right, p_specific)))
# Remove exact 0 or 1 if they sneak in (shouldn't with xmin)
y_test <- y_test[y_test > 0 & y_test < 1]

print(paste(
  "Testing", length(y_test), "unique points between",
  format(min(y_test), scientific = TRUE, digits = 3), "and",
  format(max(y_test), scientific = FALSE, digits = 16)
))

# --- Run the Test ---
# Make sure Finv is loaded, F is defined (likely FofU_analytical from Finv3)
res_test <- sapply(y_test, \(p) tryCatch(Finv(p), error = function(e) NA))
# Check for NAs in res_test
if (any(is.na(res_test))) {
  warning("Finv returned NA for some inputs!")
  print("Problematic p values:")
  print(y_test[is.na(res_test)])
}
y_valid <- y_test[!is.na(res_test)]
res_test <- res_test[!is.na(res_test)] # Remove NAs for error calculation

# --- Calculate Error ---
# Make sure F (the original CDF function) is available and accurate
# Use the most precise version, FofU_analytical
F_res <- sapply(res_test, F)
max_abs_err <- max(abs((F_res - y_valid) / y_valid))

print(paste(
  "Maximum absolute error |F(Finv(p)) - p|:",
  format(max_abs_err, scientific = TRUE, digits = 4)
))

# Optional: Plot results, maybe log scale
# plot(y_valid, res_test, type="p", pch=".", log="x", main="Full Range Test")
