#!/usr/bin/env Rscript
# Independent numerical checks for the 12 not-implemented sstd/sged hold
# subsets (TASK: P0b). Densities are re-derived from the README log_density
# formulas (via the existing notes/evaluations/2026-10-29-p0-skewreal/density.R
# helper), independent of the package. No package code is loaded or changed.
#
# Mechanisms probed (see notes/evaluations/2026-10-29-p0b.md for the proofs):
#  (1) sstd mode-at-tie + scale collapse: unbounded iff k > nu*(n-k) for the
#      achievable nu (nu held -> that value; nu free -> worst case nu->2+,
#      threshold k > 2(n-k)).
#  (2) sged: exponential tails beat any such tie blow-up unless the whole
#      sample is tied (k=n); SEPARATELY, nu->0 is a genuine spike needing the
#      existing nu>=1 restriction (A3: no exemption, checked here for sd held).
#  (3) xi free alone (mean,sd,nu held): bounded 1-d profile; escape only if
#      data is feasible for the FORCED half-family endpoint.
#  (4) nu free alone (mean,sd,xi held): bounded; escape only via exact
#      coincidence with the forced nu-edge point (non-generic).
set.seed(20261029)
root <- "notes/evaluations/2026-10-29-p0-skewreal"
source(file.path(root, "density.R"))  # provides logref(id, x, p)

# ---- samplers (two-piece Fernandez-Steel construction) --------------------
m1_sstd <- function(nu) exp(lgamma((nu - 1) / 2) - lgamma(nu / 2)) * sqrt((nu - 2) / pi)
m1_sged <- function(nu) {
  lambda <- sqrt(2^(-2 / nu) * gamma(1 / nu) / gamma(3 / nu))
  2^(1 / nu) * lambda * gamma(2 / nu) / gamma(1 / nu)
}
mode_scale <- function(id, mean, sd, nu, xi) {
  m1 <- if (id == "sstd") m1_sstd(nu) else m1_sged(nu)
  sigma_u <- sqrt((1 - m1^2) * (xi^2 + 1 / xi^2) + 2 * m1^2 - 1)
  mu_u <- m1 * (xi - 1 / xi)
  s <- sd / sigma_u
  list(mode = mean - s * mu_u, a = s / xi, b = s * xi, sigma_u = sigma_u, mu_u = mu_u, s = s)
}
rtwopiece <- function(n, mode, a, b, rabs) {
  left <- runif(n) < a / (a + b)
  z <- rabs(n)
  ifelse(left, mode - a * z, mode + b * z)
}
rsstd2 <- function(n, mean = 0, sd = 1, nu = 3, xi = 1) {
  ms <- mode_scale("sstd", mean, sd, nu, xi)
  rtwopiece(n, ms$mode, ms$a, ms$b, function(n) abs(rt(n, nu)) * sqrt((nu - 2) / nu))
}
rsged2 <- function(n, mean = 0, sd = 1, nu = 3, xi = 1) {
  ms <- mode_scale("sged", mean, sd, nu, xi)
  lambda <- sqrt(2^(-2 / nu) * gamma(1 / nu) / gamma(3 / nu))
  # f(z) ~ exp(-0.5*|z/lambda|^nu) (the log_dged_unit convention, note the 1/2
  # inside the exponent): |Z| = lambda*(2G)^(1/nu), G ~ Gamma(1/nu, rate=1).
  rtwopiece(n, ms$mode, ms$a, ms$b, function(n) lambda * (2 * rgamma(n, shape = 1 / nu, rate = 1))^(1 / nu))
}

held_sets <- list(c("mean"), c("sd"), c("nu"), c("xi"),
                   c("mean", "sd"), c("mean", "nu"), c("mean", "xi"),
                   c("sd", "nu"), c("sd", "xi"), c("nu", "xi"),
                   c("mean", "sd", "nu"), c("mean", "sd", "xi"))
all_params <- c("mean", "sd", "nu", "xi")

encode1 <- function(id, nm, v) {
  if (nm == "sd" || nm == "xi") log(v)
  else if (nm == "nu") { if (id == "sstd") log(v - 2) else log(v) }
  else v
}
decode1 <- function(id, nm, t) {
  if (nm == "sd" || nm == "xi") exp(t)
  else if (nm == "nu") { if (id == "sstd") 2 + exp(t) else exp(t) }
  else t
}

make_nll <- function(id, x, fixed, free) {
  function(theta) {
    theta <- unname(theta)  # named theta[i] would poison unlist(p)'s names below
    p <- fixed
    for (i in seq_along(free)) p[[free[i]]] <- decode1(id, free[i], theta[i])
    pv <- unlist(p)[all_params]
    if (!is.finite(pv["sd"]) || pv["sd"] <= 0) return(1e12)
    if (id == "sstd" && (!is.finite(pv["nu"]) || pv["nu"] <= 2)) return(1e12)
    if (id == "sged" && (!is.finite(pv["nu"]) || pv["nu"] <= 0)) return(1e12)
    if (!is.finite(pv["xi"]) || pv["xi"] <= 0) return(1e12)
    v <- tryCatch(logref(id, x, pv), error = function(e) rep(-Inf, length(x)))
    if (any(!is.finite(v))) return(1e12)
    -sum(v)
  }
}

multistart <- function(id, x, fixed, free, starts) {
  nll <- make_nll(id, x, fixed, free)
  best <- list(value = Inf, par = NULL)
  for (s0 in starts) {
    for (meth in c("BFGS", "Nelder-Mead")) {
      fit <- tryCatch(optim(s0, nll, method = meth, control = list(maxit = 600, reltol = 1e-12)),
                       error = function(e) NULL)
      if (!is.null(fit) && is.finite(fit$value) && fit$value < best$value) best <- fit
    }
  }
  best
}

default_starts <- function(id, fixed, free) {
  base <- list(mean = 0, sd = 1, nu = if (id == "sstd") 5 else 2, xi = 1)
  for (nm in names(fixed)) base[[nm]] <- fixed[[nm]]
  s0 <- sapply(free, function(nm) encode1(id, nm, base[[nm]]))
  grid <- list(s0)
  for (d in c(-1.5, 1.5)) grid[[length(grid) + 1]] <- s0 + d
  if ("xi" %in% free) {
    for (x0 in c(0.3, 3)) {
      s1 <- s0; s1[match("xi", free)] <- encode1(id, "xi", x0); grid[[length(grid) + 1]] <- s1
    }
  }
  if ("nu" %in% free) {
    for (n0 in if (id == "sstd") c(2.5, 50) else c(0.3, 8)) {
      s1 <- s0; s1[match("nu", free)] <- encode1(id, "nu", n0); grid[[length(grid) + 1]] <- s1
    }
  }
  grid
}

rows <- list(); j <- 0L
log_row <- function(...) { j <<- j + 1L; rows[[j]] <<- data.frame(...) }

# ============================================================================
# Experiment 0: broad coverage -- for every one of the 12 not-implemented
# hold subsets, per family, multistart-fit the free coordinates on
# default-parameter samples (n=20,100,1000), small n=5, and a tied sample,
# using the family's own README `simulate:` defaults as the held values and
# as the (encoded) start point. Flags a fit as "boundary-ish" when the best
# point sits near the edge of the wide search box (log|.| > 10), a cheap
# proxy for an escaping path worth checking by hand (used to steer Exp 1-4).
# ============================================================================
sim_defaults <- list(sstd = list(mean = 0, sd = 1, nu = 3, xi = 2),
                      sged = list(mean = 0, sd = 1, nu = 3, xi = 3))
for (id in c("sstd", "sged")) {
  sim <- sim_defaults[[id]]
  samp <- list(n20 = do.call(if (id == "sstd") rsstd2 else rsged2, c(list(n = 20), sim)))
  samp$n100 <- do.call(if (id == "sstd") rsstd2 else rsged2, c(list(n = 100), sim))
  samp$n1000 <- do.call(if (id == "sstd") rsstd2 else rsged2, c(list(n = 1000), sim))
  samp$n5 <- samp$n20[1:5]
  x_t <- samp$n20; x_t[2:5] <- x_t[1]; samp$ties <- x_t  # k=4 tie, n=20 (below threshold)
  for (sname in names(samp)) {
    x <- samp[[sname]]
    for (hs in held_sets) {
      held <- hs; free <- setdiff(all_params, held)
      fixed <- sim[held]
      starts <- default_starts(id, fixed, free)
      fit <- multistart(id, x, fixed, free, starts)
      par_decoded <- if (!is.null(fit$par)) sapply(seq_along(free), function(i) decode1(id, free[i], fit$par[i])) else NA
      boundary <- if (!is.null(fit$par)) any(abs(fit$par) > 10) else NA
      log_row(id = paste0(id, "_cover"), held = paste(hs, collapse = "+"), sample = sname,
              best_nll = fit$value, diverging = boundary,
              path_01 = NA, path_001 = NA, path_0001 = NA, path_00001 = NA)
    }
  }
}

# ============================================================================
# Experiment 1: mode-at-tie + scale collapse (sstd) vs no-collapse (sged).
# x has k=7 ties at 0, n-k=2 at {1,2}: threshold k > 2(n-k) <=> 7 > 4 (TRUE).
# ============================================================================
x_heavy <- c(rep(0, 7), 1, 2)          # k=7, n=9, n-k=2: 7 > 2*2  => sstd collapse
x_safe  <- c(rep(0, 2), 1:7)           # k=2, n=9, n-k=7: 2 < 2*7 => no sstd collapse

for (id in c("sstd", "sged")) for (xdesc in c("heavy", "safe")) {
  x <- if (xdesc == "heavy") x_heavy else x_safe
  for (hs in held_sets) {
    held <- hs; free <- setdiff(all_params, held)
    if (!("mean" %in% held)) {   # mode must be free to "chase" the tie at 0
      fixed <- list(sd = 1, nu = if (id == "sstd") 3 else 3, xi = if (id == "sstd") 2 else 3)
      fixed <- fixed[intersect(names(fixed), held)]
      starts <- default_starts(id, fixed, free)
      fit <- multistart(id, x, fixed, free, starts)
      # explicit divergence path: mode -> 0 (the tie), sd -> 0 along the free coords.
      fixed2 <- fixed
      path_ll <- function(eps) {
        p <- fixed2
        if ("mean" %in% free) p$mean <- 0
        if ("sd" %in% free) p$sd <- eps
        if ("nu" %in% free) p$nu <- if (id == "sstd") 2 + eps else eps
        if ("xi" %in% free) p$xi <- 1
        pv <- unlist(p)[all_params]
        sum(logref(id, x, pv))
      }
      can_path <- all(c("mean") %in% c(free, "mean")) &&
        (("sd" %in% free) || ("nu" %in% free))
      path_vals <- if (can_path) sapply(c(0.3, 0.03, 0.003, 0.0003), path_ll) else rep(NA, 4)
      log_row(id = id, held = paste(hs, collapse = "+"), sample = xdesc,
              best_nll = fit$value, diverging = is.infinite(fit$value) || fit$value < -1e6,
              path_01 = path_vals[1], path_001 = path_vals[2],
              path_0001 = path_vals[3], path_00001 = path_vals[4])
    }
  }
}

# ============================================================================
# Experiment 2: sged nu -> 0 spike with sd (and xi) HELD -- does holding sd
# exempt the existing nu>=1 restriction (roadmap: "not sd", i.e. NOT exempt)?
# mean held AT the tied value; vary the tie fraction around 13.79%.
# ============================================================================
make_tied_sample <- function(k, m) c(rep(0, k), seq(1, m))  # k ties at 0, m spread out
for (frac_desc in list(list(k = 1, m = 10, note = "9pct_below"),
                        list(k = 2, m = 10, note = "18pct_above"))) {
  x <- make_tied_sample(frac_desc$k, frac_desc$m)
  n <- length(x); k <- frac_desc$k
  for (sd_held in c(TRUE, FALSE)) {
    nus <- c(2, 1, 0.5, 0.2, 0.1, 0.05, 0.02, 0.01)
    lls <- sapply(nus, function(nu) {
      fixed <- list(mean = 0, xi = 1)
      if (sd_held) fixed$sd <- 3
      free <- setdiff(all_params, c(names(fixed), "nu"))
      p <- fixed; p$nu <- nu
      if (!sd_held) {
        # profile sd (and mean already fixed at the tied value) at this nu.
        nllf <- make_nll("sged", x, c(fixed, list(nu = nu)), "sd")
        opt <- optimize(function(t) nllf(t), interval = c(-20, 20))
        -opt$objective
      } else {
        pv <- unlist(p)[all_params]
        sum(logref("sged", x, pv))
      }
    })
    log_row(id = "sged_spike", held = paste0("sd_held=", sd_held), sample = frac_desc$note,
            best_nll = NA, diverging = NA,
            path_01 = lls[1], path_001 = lls[4], path_0001 = lls[6], path_00001 = lls[8])
  }
}

# ============================================================================
# Experiment 3: xi free alone (mean,sd,nu held) -- bounded 1-d profile; the
# forced half-family endpoint/scale determine a feasibility condition.
# ============================================================================
for (id in c("sstd", "sged")) {
  mean0 <- 0; sd0 <- 1; nu0 <- if (id == "sstd") 5 else 2
  f <- function(xi) {
    p <- list(mean = mean0, sd = sd0, nu = nu0, xi = xi)
    pv <- unlist(p)[all_params]
  }
  ll_xi <- function(xi, x) sum(logref(id, x, unlist(list(mean = mean0, sd = sd0, nu = nu0, xi = xi))[all_params]))
  # feasible (straddling) sample vs one-sided (all above mean0) sample.
  x_strad <- c(-3, -1, 0.5, 1, 2, 3)
  x_oneside <- c(0.5, 1, 1.5, 2, 2.5, 3, 10)
  for (sname in c("straddle", "oneside")) {
    x <- if (sname == "straddle") x_strad else x_oneside
    xis <- c(0.1, 0.5, 1, 2, 5, 20, 100, 1000)
    lls <- sapply(xis, ll_xi, x = x)
    best_i <- which.max(lls)
    log_row(id = paste0(id, "_xialone"), held = "mean+sd+nu", sample = sname,
            best_nll = -lls[best_i], diverging = best_i %in% c(1, length(xis)),
            path_01 = lls[1], path_001 = lls[3], path_0001 = lls[6], path_00001 = lls[8])
  }
}

# ============================================================================
# Experiment 4: nu free alone (mean,sd,xi held) -- generic (non-coincident)
# data should give an interior maximum away from domain edges.
# ============================================================================
for (id in c("sstd", "sged")) {
  mean0 <- 0; sd0 <- 1; xi0 <- 2
  x <- rnorm(20) + 3  # generic, no exact coincidence with any mean0-implied point
  ll_nu <- function(nu) sum(logref(id, x, unlist(list(mean = mean0, sd = sd0, nu = nu, xi = xi0))[all_params]))
  nus <- if (id == "sstd") c(2.01, 2.1, 2.5, 5, 20, 100, 1000) else c(0.05, 0.2, 1, 3, 10, 100, 1000)
  lls <- sapply(nus, ll_nu)
  best_i <- which.max(lls)
  log_row(id = paste0(id, "_nualone"), held = "mean+sd+xi", sample = "generic",
          best_nll = -lls[best_i], diverging = best_i %in% c(1, length(nus)),
          path_01 = lls[1], path_001 = lls[3], path_0001 = lls[5], path_00001 = lls[7])
}

out <- do.call(rbind, rows)
write.csv(out, "notes/evaluations/2026-10-29-p0b/skewreal-results.csv", row.names = FALSE)
print(out, row.names = FALSE)
