#!/usr/bin/env Rscript
# Independent numerical checks for the 11 not-implemented free-size hold
# subsets of bbinom, cmb, tbinom (TASK: P0b). Self-contained: densities are
# re-derived from each README's log_density / its stated reference (dbbinom,
# dtbinom are each family's own documented r.density; cmb has no package
# reference so its density is the direct combinatorial formula). No
# univariateML code is loaded. Extends notes/evaluations/2026-10-29-p0-binomial/
# size-scan.R's profile-over-size approach with a longer grid and an explicit
# comparison against the analytically computed limiting ("named law") face,
# to tell a genuine size -> infinity escape from a slow-converging finite
# optimum.
suppressPackageStartupMessages({ library(extraDistr) })
set.seed(20261030)

# ---- cmb's own density (no package reference exists) ----------------------
cmb_ll <- function(x, N, prob, nu) {
  k <- 0:N
  lc <- lchoose(N, k)
  z <- nu * lc + k * qlogis(prob)
  mx <- max(z)
  logZ <- mx + log(sum(exp(z - mx)))
  sum(nu * lchoose(N, x) + x * qlogis(prob) - logZ)
}

# ---- profile over free shapes at a FIXED size, for each family -----------
profile_bbinom <- function(x, N, alpha_fixed = NA, beta_fixed = NA) {
  if (any(x > N)) return(-Inf)
  if (!is.na(alpha_fixed) && !is.na(beta_fixed))
    return(sum(dbbinom(x, N, alpha_fixed, beta_fixed, log = TRUE)))
  free <- c(if (is.na(alpha_fixed)) "alpha", if (is.na(beta_fixed)) "beta")
  # extraDistr::dbbinom loses accuracy for shapes beyond ~1e10 (KNOWN_ISSUES.md);
  # box log(shape) in [-16, 16] (shape up to ~9e6) so BFGS cannot wander into
  # the region where the reference itself underflows to a spurious density 1.
  nll <- function(t) {
    a <- if (is.na(alpha_fixed)) exp(t[match("alpha", free)]) else alpha_fixed
    b <- if (is.na(beta_fixed)) exp(t[match("beta", free)]) else beta_fixed
    v <- -sum(dbbinom(x, N, a, b, log = TRUE))
    if (!is.finite(v)) 1e12 else v
  }
  best <- Inf
  lower <- rep(-16, length(free)); upper <- rep(16, length(free))
  for (s0 in list(rep(0, length(free)), rep(1, length(free)), rep(-1, length(free)), rep(3, length(free)))) {
    fit <- tryCatch(optim(s0, nll, method = "L-BFGS-B", lower = lower, upper = upper,
                           control = list(maxit = 300)), error = function(e) NULL)
    if (!is.null(fit) && is.finite(fit$value)) best <- min(best, fit$value)
  }
  if (length(free) == 1) {
    opt <- tryCatch(optimize(nll, interval = c(-16, 16), tol = 1e-10), error = function(e) NULL)
    if (!is.null(opt) && is.finite(opt$objective)) best <- min(best, opt$objective)
  }
  -best
}
profile_cmb <- function(x, N, prob_fixed = NA, nu_fixed = NA) {
  if (any(x > N)) return(-Inf)
  free <- c(if (is.na(prob_fixed)) "prob", if (is.na(nu_fixed)) "nu")
  if (length(free) == 0) return(cmb_ll(x, N, prob_fixed, nu_fixed))
  nll <- function(t) {
    p <- if (is.na(prob_fixed)) plogis(t[match("prob", free)]) else prob_fixed
    nu <- if (is.na(nu_fixed)) t[match("nu", free)] else nu_fixed
    v <- tryCatch(-cmb_ll(x, N, p, nu), error = function(e) 1e12)
    if (!is.finite(v)) 1e12 else v
  }
  lower <- ifelse(free == "nu", -60, -16); upper <- ifelse(free == "nu", 60, 16)
  best <- Inf
  starts3 <- list(rep(0, length(free)),
                   if ("nu" %in% free) { z <- rep(0, length(free)); z[match("nu", free)] <- 30; z } else rep(2, length(free)),
                   if ("nu" %in% free) { z <- rep(0, length(free)); z[match("nu", free)] <- -30; z } else rep(-2, length(free)))
  for (s0 in starts3) {
    fit <- tryCatch(optim(s0, nll, method = "L-BFGS-B", lower = lower, upper = upper,
                           control = list(maxit = 150)), error = function(e) NULL)
    if (!is.null(fit) && is.finite(fit$value)) best <- min(best, fit$value)
  }
  -best
}
profile_tbinom <- function(x, N, a, b, prob_fixed = NA) {
  lo <- max(0, floor(a) + 1); hi <- min(N, floor(b))
  if (lo > hi || any(x < lo) || any(x > hi)) return(-Inf)
  ll <- function(p) {
    if (p <= 0 || p >= 1) return(-Inf)
    k <- lo:hi
    logden <- dbinom(k, N, p, log = TRUE)
    mx <- max(logden); logZ <- mx + log(sum(exp(logden - mx)))
    sum(dbinom(x, N, p, log = TRUE)) - length(x) * logZ
  }
  if (!is.na(prob_fixed)) return(ll(prob_fixed))
  opt <- optimize(ll, interval = c(1e-10, 1 - 1e-10), maximum = TRUE, tol = 1e-10)
  opt$objective
}

# ---- analytic limiting faces -----------------------------------------------
poisson_face <- function(x) sum(dpois(x, mean(x), log = TRUE))
nb_face_fixed_r <- function(x, r) { p <- r / (r + mean(x)); sum(dnbinom(x, size = r, prob = p, log = TRUE)) }
nb_face_free_r <- function(x) {
  f <- function(logr) -nb_face_fixed_r(x, exp(logr))
  best <- Inf
  for (s0 in c(-2, 0, 2, 5)) {
    fit <- tryCatch(optimize(f, interval = c(-5, 12)), error = function(e) NULL)
    if (!is.null(fit) && is.finite(fit$objective)) best <- min(best, fit$objective)
  }
  -best
}
binom_face <- function(x, Nmax = 20000) {
  mu <- mean(x); n <- length(x)
  V <- sum((x - mu)^2) / n
  if (V >= mu) return(poisson_face(x))  # DeRiggi 1983: no finite interior beats Poisson
  best <- -Inf
  N <- max(x)
  while (N <= Nmax) {
    p <- mu / N
    if (p > 0 && p < 1) {
      ll <- sum(dbinom(x, N, p, log = TRUE))
      if (ll > best + 1e-10) best <- ll else if (N > max(x) + 200) break
    }
    N <- N + max(1, floor(N * 0.05))
  }
  max(best, poisson_face(x))
}
two_point_face <- function(x) {  # saturated empirical two-point law (adjacent/endpoint escapes)
  tab <- table(x); p <- tab / sum(tab)
  sum(tab * log(p))
}
trunc_poisson_face <- function(x, a, b) {  # tbinom size -> infinity, prob fixed: Poisson truncated to (a,b]
  lo <- max(0, floor(a) + 1); hi <- floor(b)
  f <- function(loglam) {
    lam <- exp(loglam)
    k <- lo:min(hi, lo + 5000)
    logden <- dpois(k, lam, log = TRUE)
    mx <- max(logden); logZ <- mx + log(sum(exp(logden - mx)))
    sum(dpois(x, lam, log = TRUE)) - length(x) * logZ
  }
  opt <- optimize(f, interval = c(log(1e-6), log(1e6)), maximum = TRUE)
  opt$objective
}

grid_profile_max <- function(profile_fn, Nmax, step_frac = 0.08) {
  xs <- c(); ys <- c()
  N <- attr(profile_fn, "Nmin")
  best <- -Inf; best_N <- N
  while (N <= Nmax) {
    v <- profile_fn(N)
    xs <- c(xs, N); ys <- c(ys, v)
    if (is.finite(v) && v > best) { best <- v; best_N <- N }
    N <- N + max(1, floor(N * step_frac))
  }
  list(best = best, best_N = best_N, last = tail(ys, 1), last_N = tail(xs, 1), trace = data.frame(N = xs, ll = ys))
}

rows <- list(); j <- 0L
log_row <- function(...) { j <<- j + 1L; rows[[j]] <<- data.frame(...) }

samples_bbinom <- list(
  default_20 = rbbinom(20, 10, 2, 3), default_100 = rbbinom(100, 10, 2, 3),
  default_1000 = rbbinom(1000, 10, 2, 3), small_5 = rbbinom(5, 10, 2, 3),
  tied = c(2, 2, 2, 2, 5, 6, 7), negbinom_limit = rnbinom(100, size = 2, prob = 0.4),
  binom_like = rbinom(100, 10, 0.4))

samples_cmb <- list(
  default_20 = { N <- 10; k <- 0:N; z <- 1.5 * lchoose(N, k) + k * qlogis(0.3); p <- exp(z - max(z)); p <- p / sum(p); sample(k, 20, TRUE, p) },
  default_100 = { N <- 10; k <- 0:N; z <- 1.5 * lchoose(N, k) + k * qlogis(0.3); p <- exp(z - max(z)); p <- p / sum(p); sample(k, 100, TRUE, p) },
  tied = c(0, 0, 1, 1, 2, 2), adjacent = rep(c(4, 5), 25), endpoint = rep(c(0, 10), 25))

samples_tbinom <- list(
  default_20 = rtbinom(20, 12, 0.4, 0, 10), default_100 = rtbinom(100, 12, 0.4, 0, 10),
  default_1000 = rtbinom(1000, 12, 0.4, 0, 10), small_5 = rtbinom(5, 12, 0.4, 0, 10),
  tied = c(1, 1, 1, 2, 3), tpois_like = { lam <- 3; p <- dpois(0:10, lam); p <- p / sum(p[1:11]); sample(0:10, 100, TRUE, p[1:11]) })

cat("=== bbinom: free size, held subsets {}, {alpha}, {beta} ===\n")
for (sn in names(samples_bbinom)) {
  x <- samples_bbinom[[sn]]
  for (held in list(c(), "alpha", "beta")) {
    af <- if ("alpha" %in% held) 2 else NA; bf <- if ("beta" %in% held) 3 else NA
    pf <- function(N) profile_bbinom(x, N, af, bf)
    attr(pf, "Nmin") <- max(x)
    res <- grid_profile_max(pf, Nmax = 4000, step_frac = 0.15)
    B_bin <- binom_face(x); B_nb <- if (!is.na(af)) nb_face_fixed_r(x, af) else nb_face_free_r(x)
    B <- max(B_bin, B_nb, poisson_face(x))
    log_row(family = "bbinom", held = paste(held, collapse = "+"), sample = sn, n = length(x),
            grid_best = res$best, grid_best_N = res$best_N, grid_last = res$last, grid_last_N = res$last_N,
            face_B = B, interior_minus_face = res$best - B)
  }
}

cat("=== cmb: free size, held subsets {}, {prob}, {nu}, {prob,nu} ===\n")
for (sn in names(samples_cmb)) {
  x <- samples_cmb[[sn]]
  for (held in list(c(), "prob", "nu", c("prob", "nu"))) {
    pf_fixed <- if ("prob" %in% held) 0.3 else NA; nf_fixed <- if ("nu" %in% held) 1.5 else NA
    pf <- function(N) profile_cmb(x, N, pf_fixed, nf_fixed)
    attr(pf, "Nmin") <- max(x)
    res <- grid_profile_max(pf, Nmax = 1500, step_frac = 0.25)
    B <- two_point_face(x)
    log_row(family = "cmb", held = paste(held, collapse = "+"), sample = sn, n = length(x),
            grid_best = res$best, grid_best_N = res$best_N, grid_last = res$last, grid_last_N = res$last_N,
            face_B = B, interior_minus_face = res$best - B)
  }
}

cat("=== tbinom: free size & prob, held subsets {}, {a}, {b}, {a,b} ===\n")
for (sn in names(samples_tbinom)) {
  x <- samples_tbinom[[sn]]
  for (held in list(c(), "a", "b", c("a", "b"))) {
    a <- if ("a" %in% held) 0 else min(x) - 1; b <- if ("b" %in% held) 10 else max(x)
    pf <- function(N) profile_tbinom(x, N, a, b)
    attr(pf, "Nmin") <- max(x)
    res <- grid_profile_max(pf, Nmax = 20000, step_frac = 0.15)
    B <- tryCatch(trunc_poisson_face(x, a, b), error = function(e) NA_real_)
    log_row(family = "tbinom", held = paste(held, collapse = "+"), sample = sn, n = length(x),
            grid_best = res$best, grid_best_N = res$best_N, grid_last = res$last, grid_last_N = res$last_N,
            face_B = B, interior_minus_face = res$best - B)
  }
}

out <- do.call(rbind, rows)
write.csv(out, "notes/evaluations/2026-10-29-p0b/binomial-results.csv", row.names = FALSE)
print(out, row.names = FALSE)
