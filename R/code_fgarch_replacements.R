# Lightweight replacements for fGarch fitters used by mlged, mlsged, mlsnorm,
# mlsstd, and mlstd. These are simple stats::nlminb wrappers around the package's
# dpqr implementations to avoid taking a hard dependency on fGarch.

safe_obj <- function(fn, penalty = 1e9) {
  force(fn)
  function(par, y) {
    val <- tryCatch(fn(par, y), error = identity)
    if (inherits(val, "error") || !is.finite(val)) penalty else val
  }
}

gedFit <- function(x, ...) {
  start <- c(mean = mean(x), sd = sqrt(stats::var(x)), nu = 2)
  objective <- safe_obj(function(par, y) {
    m <- par[1]
    s <- par[2]
    nu <- par[3]
    if (s <= 0 || nu <= 0) {
      return(Inf)
    }
    ll <- dged(y, m, s, nu, log = TRUE)
    if (any(!is.finite(ll))) {
      return(Inf)
    }
    -sum(ll)
  })
  fit <- stats::nlminb(
    start = start,
    objective = objective,
    lower = c(-Inf, 1e-8, 1e-8),
    upper = c(Inf, Inf, Inf),
    y = x,
    ...
  )
  names(fit$par) <- c("mean", "sd", "nu")
  fit
}

sgedFit <- function(x, ...) {
  start <- c(mean = mean(x), sd = sqrt(stats::var(x)), nu = 2, xi = 1.5)
  objective <- safe_obj(function(par, y) {
    m <- par[1]
    s <- par[2]
    nu <- par[3]
    xi <- par[4]
    if (s <= 0 || nu <= 0 || xi <= 0) {
      return(Inf)
    }
    ll <- dsged(y, m, s, nu, xi, log = TRUE)
    if (any(!is.finite(ll))) {
      return(Inf)
    }
    -sum(ll)
  })
  fit <- stats::nlminb(
    start = start,
    objective = objective,
    lower = c(-Inf, 1e-8, 1e-8, 1e-8),
    upper = c(Inf, Inf, Inf, Inf),
    y = x,
    ...
  )
  names(fit$par) <- c("mean", "sd", "nu", "xi")
  fit
}

snormFit <- function(x, ...) {
  start <- c(mean = mean(x), sd = sqrt(stats::var(x)), xi = 1.5)
  objective <- safe_obj(function(par, y) {
    m <- par[1]
    s <- par[2]
    xi <- par[3]
    if (s <= 0 || xi <= 0) {
      return(Inf)
    }
    ll <- dsnorm(y, m, s, xi, log = TRUE)
    if (any(!is.finite(ll))) {
      return(Inf)
    }
    -sum(ll)
  })
  fit <- stats::nlminb(
    start = start,
    objective = objective,
    lower = c(-Inf, 1e-8, 1e-8),
    upper = c(Inf, Inf, Inf),
    y = x,
    ...
  )
  names(fit$par) <- c("mean", "sd", "xi")
  fit
}

stdFit <- function(x, ...) {
  start <- c(mean = mean(x), sd = sqrt(stats::var(x)), nu = 5)
  objective <- safe_obj(function(par, y) {
    m <- par[1]
    s <- par[2]
    nu <- par[3]
    if (s <= 0 || nu <= 2) {
      return(Inf)
    }
    ll <- dstd(y, m, s, nu, log = TRUE)
    if (any(!is.finite(ll))) {
      return(Inf)
    }
    -sum(ll)
  })
  fit <- stats::nlminb(
    start = start,
    objective = objective,
    lower = c(-Inf, 1e-8, 2 + 1e-8),
    upper = c(Inf, Inf, Inf),
    y = x,
    ...
  )
  names(fit$par) <- c("mean", "sd", "nu")
  fit
}

sstdFit <- function(x, ...) {
  start <- c(mean = mean(x), sd = sqrt(stats::var(x)), nu = 5, xi = 1.5)
  objective <- safe_obj(function(par, y) {
    m <- par[1]
    s <- par[2]
    nu <- par[3]
    xi <- par[4]
    if (s <= 0 || nu <= 2 || xi <= 0) {
      return(Inf)
    }
    ll <- dsstd(y, m, s, nu, xi, log = TRUE)
    if (any(!is.finite(ll))) {
      return(Inf)
    }
    -sum(ll)
  })
  fit <- stats::nlminb(
    start = start,
    objective = objective,
    lower = c(-Inf, 1e-8, 2 + 1e-8, 1e-8),
    upper = c(Inf, Inf, Inf, Inf),
    y = x,
    ...
  )
  names(fit$par) <- c("mean", "sd", "nu", "xi")
  fit
}
