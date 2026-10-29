#' assertthat
#'
#' Light replacement for assert_that.
#'
#' @param ... conditions to be tested.
#' @param env optional environment
#' @param msg custom error message.

assert_that <- function(..., env = parent.frame(), msg = NULL) {
  conds <- as.list(substitute(list(...)))[-1L]
  for (cond in conds) {
    ok <- tryCatch(eval(cond, env), error = function(e) e)
    if (!isTRUE(ok)) {
      m <- if (!is.null(msg)) {
        msg
      } else if (inherits(ok, "error")) {
        ok$message
      } else {
        paste("Assertion failed:", deparse(cond))
      }
      stop(m, call. = FALSE)
    }
  }
  TRUE
}


#' Decorate estimator
#'
#' The decorator constructs an `ml***`. It first checks the input arguments,
#'    then call the defined estimator (with elements `estimates` and `logLik`),
#'    and the optional parameter `support`, then adds the required attributes.
#'
#' @param name Name of the `ml***` function.
#' @return The proper `ml***` function.
#' @keywords internal
decorator <- function(name) {
  force(name)
  fun <- function(x, na.rm = FALSE, ...) {
    x <- ml_check_modify(x, na.rm = na.rm, name = name)
    n <- length(x)
    out <- do.call(paste0(name, "_"), c(list(x = x), list(...)))

    params <- list(logLik = unname(out$logLik), call = deparse(match.call()), n = n)
    univariateML_construct(out$estimates, name = name, params = params, fixed = out$fixed)
  }
  meta <- univariateML_metadata[[name]]
  meta$parameters <- meta$names
  meta$names <- NULL
  attributes(fun) <- c(meta, attributes(fun))
  fun
}

#' Construct `univariateML` object.
#'
#' @param estimates The estimated parameters
#' @param name Name of the `ml***` function.
#' @param params List of `loglik`, `call`, and `n`.
#' @param fixed Names of the parameters held fixed rather than estimated.
#' @return Object of class `univariateML`

univariateML_construct <- function(estimates, name, params, fixed = NULL) {
  estimates <- unname(estimates)
  class <- "univariateML"
  args <- c(.Data = list(estimates), params, univariateML_metadata[[name]], class = class)
  object <- do.call(structure, args)
  attr(object, "call") <- if (length(attr(object, "call")) == 1) {
    str2lang(attr(object, "call"))
  } else {
    NULL
  }

  attr(object, "continuous") <- (univariateML_metadata[[name]]$support$type == "R")
  attr(object, "support") <- univariateML_metadata[[name]]$support$bounds
  attr(object, "fixed") <- as.character(fixed)
  object
}

#' Names of the parameters a univariateML object held fixed.
#'
#' @param obj A univariateML object.
#' @return A character vector, empty when every parameter was estimated.
#' @keywords internal
univariateML_fixed <- function(obj) {
  fixed <- attr(obj, "fixed")
  if (is.null(fixed)) character(0) else fixed
}

#' Number of estimated (free) parameters of a univariateML object.
#'
#' @param obj A univariateML object.
#' @return An integer.
#' @keywords internal
univariateML_df <- function(obj) {
  length(obj) - length(univariateML_fixed(obj))
}


ml_check_modify <- function(x, na.rm, name) {
  assert_that(is.numeric(x))
  msg <- paste0("x is not a numeric vector (NCOL(x) = ", NCOL(x), ")")
  assert_that(NCOL(x) == 1, msg = msg)
  msg <- "NA in input when na.rm = FALSE"
  if (na.rm) x <- x[!is.na(x)] else assert_that(!anyNA(x), msg = msg)
  # Without this, min/max of an empty vector pass the support checks and
  # the fitters return NaN estimates.
  if (!length(x)) stop("x must have positive length")

  support <- univariateML_metadata[[name]]$support

  msg <- "x not in the support of the data"
  if (support$closed[1]) {
    assert_that(min(x) >= support$bounds[[1]], msg = msg)
  } else {
    assert_that(min(x) > support$bounds[[1]], msg = msg)
  }

  if (support$closed[2]) {
    assert_that(max(x) <= support$bounds[[2]], msg = msg)
  } else {
    assert_that(max(x) < support$bounds[[2]], msg = msg)
  }

  if (support$type == "Z") {
    if (sum(abs(x - floor(x)) != 0)) {
      stop("Non-integral inputs for discrete density.")
    }
    # The count fitters take integer vectors; larger values would become
    # NA_integer_ (the most negative int) and give silent nonsense.
    if (max(abs(x)) > .Machine$integer.max) {
      stop("Counts must not exceed .Machine$integer.max in absolute value.")
    }
  }

  x
}

#' Name of the `ml***` function that produced a univariateML object.
#'
#' @param obj A univariateML object.
#' @return A string such as `"mlnorm"`, or `NULL` if the model is unknown.
#' @keywords internal

univariateML_name <- function(obj) {
  model <- attr(obj, "model")
  models <- vapply(univariateML_metadata, function(m) m$model, character(1))
  hit <- names(models)[models == model]
  if (length(hit) == 1L) hit else NULL
}

#' Map estimates onto the arguments of the reference d/p/q/r functions.
#'
#' Most models pass their coefficients by name. A model whose reference
#' functions use other argument names declares `r.args` in its `README.md`
#' (argument name to coefficient name, collected in `univariateML_args`),
#' e.g. the translated half-normal uses `extraDistr::dtnorm` with
#' `mean = mu`, `sd = sigma`, `a = mu`.
#'
#' @param estimates Named numeric vector of coefficients.
#' @param name Name of the `ml***` function, or `NULL`.
#' @return A named list of argument values.
#' @keywords internal

univariateML_dpqr_args <- function(estimates, name) {
  map <- if (is.null(name)) NULL else univariateML_args[[name]]
  values <- stats::setNames(as.list(as.numeric(estimates)), names(estimates))
  if (is.null(map)) return(values)
  stats::setNames(lapply(unname(map), function(coef) values[[coef]]), names(map))
}

#' Workarounds for defects in reference functions
#'
#' Keyed by the reference function (`pkg::fn`). `broken()` tests for the
#' defect on every call, so a fixed upstream release switches the
#' workaround off; `replacement` takes the same arguments. Defects are
#' listed in `KNOWN_ISSUES.md` at the repository root.
#'
#' `extraDistr::pdweibull` (extraDistr 1.10.0.5) does not floor its
#' argument, e.g. `extraDistr::pdweibull(0.5, 0.7, 1)` is 0.414, not
#' `pdweibull(0, 0.7, 1)` = 0.3. The replacement floors `q` and calls
#' `extraDistr::pdweibull`.
#' @keywords internal

univariateML_reference_fixes <- list(
  "extraDistr::ptriang" = list(
    broken = function() {
      is.nan(suppressWarnings(extraDistr::ptriang(0, 0, 1, 0)))
    },
    replacement = function(q, a = -1, b = 1, c = (a + b)/2,
                           lower.tail = TRUE, log.p = FALSE) {
      out <- suppressWarnings(extraDistr::ptriang(q, a, b, c,
                                 lower.tail = lower.tail, log.p = log.p))
      n <- length(out)
      if (!n) return(out)
      aa <- rep_len(a, n); bb <- rep_len(b, n); cc <- rep_len(c, n)
      qq <- rep_len(q, n)
      fix <- !is.na(qq) & is.finite(aa) & is.finite(bb) &
        is.finite(cc) & aa < bb & aa <= cc & cc <= bb & qq <= aa
      out[which(fix)] <- if (log.p) { if (lower.tail) -Inf else 0 } else {
        if (lower.tail) 0 else 1
      }
      out
    }
  ),
  "extraDistr::pdweibull" = list(
    broken = function() {
      tryCatch(
        extraDistr::pdweibull(0.5, 0.7, 1) != extraDistr::pdweibull(0, 0.7, 1),
        error = function(e) TRUE
      )
    },
    replacement = function(q, shape1, shape2, lower.tail = TRUE, log.p = FALSE) {
      extraDistr::pdweibull(floor(q), shape1, shape2,
        lower.tail = lower.tail, log.p = log.p
      )
    }
  ),
  "actuar::dtrgamma" = list(
    broken = function() {
      abs(actuar::dtrgamma(1, 1e10, 1, scale = 1e-10, log = TRUE) -
          stats::dgamma(1, 1e10, scale = 1e-10, log = TRUE)) > 1e-7
    },
    replacement = function(x, shape1, shape2, rate = 1, scale = 1/rate, log = FALSE) {
      # actuar uses R's gamma backend too. Compose that backend's stable
      # log density with the power Jacobian; keep actuar's endpoint rules.
      ans <- actuar::dtrgamma(x, shape1, shape2, scale = scale, log = TRUE)
      logu <- shape2 * (log(x) - log(scale))
      u <- exp(logu)
      stable <- stats::dgamma(u, shape1, scale = 1, log = TRUE) +
        log(shape2) + logu - log(x)
      good <- is.finite(stable)
      ans[good] <- stable[good]
      if (log) ans else exp(ans)
    }
  ),
  "actuar::qtrgamma" = list(
    broken = function() {
      !isTRUE(all.equal(actuar::qtrgamma(.99, 100, .0067, scale = exp(-log(100)/.0067)),
                       exp(log(exp(-log(100)/.0067)) + log(stats::qgamma(.99, 100, lower.tail = TRUE))/.0067),
                       tolerance = 1e-12)) ||
        actuar::qtrgamma(-1000, 2, 1, log.p = TRUE) == 0 ||
        !is.finite(actuar::qtrgamma(.5, 1e-6, 1e6)) ||
        actuar::qtrgamma(.5, 1e-6, 1e6) == 0
    },
    replacement = function(p, shape1, shape2, rate = 1, scale = 1/rate,
                           lower.tail = TRUE, log.p = FALSE) {
      g <- stats::qgamma(p, shape1, scale = 1, lower.tail = lower.tail, log.p = log.p)
      logg <- log(g)
      lost <- !is.na(g) & g < exp(-700)
      if (any(lost)) {
        lp <- if (log.p) { if (lower.tail) p else log(-expm1(p)) } else {
          if (lower.tail) log(p) else log1p(-p)
        }
        anchor <- actuar::ptrgamma(exp(-700), shape1, 1, scale = 1, log.p = TRUE)
        recovered <- -700 + (lp - anchor)/shape1
        logg[lost] <- recovered[lost]
      }
      exp(log(scale) + logg/shape2)
    }
  ),
  "actuar::rtrgamma" = list(
    broken = function() {
      had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
      if (had_seed) saved_seed <- get(".Random.seed", envir = .GlobalEnv)
      on.exit(if (had_seed) assign(".Random.seed", saved_seed, envir = .GlobalEnv)
              else rm(".Random.seed", envir = .GlobalEnv))
      set.seed(1)
      z <- actuar::rtrgamma(1000, 100, .0067, scale = exp(-log(100)/.0067))
      near_power <- actuar::rtrgamma(10, 1e-6, 1e6)
      any(!is.finite(z) | z == 0) || any(!is.finite(near_power) | near_power == 0)
    },
    replacement = function(n, shape1, shape2, rate = 1, scale = 1/rate) {
      if (any(is.finite(shape1) & shape1 > 0 & shape1 < .01))
        return(univariateML_reference_fixes[["actuar::qtrgamma"]]$replacement(
          stats::runif(n), shape1, shape2, scale = scale))
      exp(log(scale) + log(stats::rgamma(n, shape1, scale = 1))/shape2)
    }
  ),
  "actuar::dinvtrgamma" = list(
    broken = function() {
      abs(actuar::dinvtrgamma(1, 1e10, 1, scale = 1e10, log = TRUE) -
          stats::dgamma(1, 1e10, scale = 1e-10, log = TRUE)) > 1e-7
    },
    replacement = function(x, shape1, shape2, rate = 1, scale = 1/rate, log = FALSE) {
      # actuar uses R's gamma backend too. Compose that backend's stable
      # log density with the power Jacobian; keep actuar's endpoint rules.
      ans <- actuar::dinvtrgamma(x, shape1, shape2, scale = scale, log = TRUE)
      logu <- shape2 * (log(scale) - log(x))
      u <- exp(logu)
      stable <- stats::dgamma(u, shape1, scale = 1, log = TRUE) +
        log(shape2) + logu - log(x)
      good <- is.finite(stable)
      ans[good] <- stable[good]
      if (log) ans else exp(ans)
    }
  ),
  "actuar::qinvtrgamma" = list(
    broken = function() {
      !isTRUE(all.equal(actuar::qinvtrgamma(1e-8, 100, .0065, scale = exp(log(100)/.0065)),
                       exp(log(exp(log(100)/.0065)) - log(stats::qgamma(1e-8, 100, lower.tail = FALSE))/.0065),
                       tolerance = 1e-12)) ||
        actuar::qinvtrgamma(-1000, 2, 1, log.p = TRUE) == 0 ||
        !is.finite(actuar::qinvtrgamma(.5, 1e-6, 1e6)) ||
        actuar::qinvtrgamma(.5, 1e-6, 1e6) == 0
    },
    replacement = function(p, shape1, shape2, rate = 1, scale = 1/rate,
                           lower.tail = TRUE, log.p = FALSE) {
      g <- stats::qgamma(p, shape1, scale = 1, lower.tail = !lower.tail, log.p = log.p)
      logg <- log(g)
      lost <- !is.na(g) & g < exp(-700)
      if (any(lost)) {
        lp <- if (log.p) { if (!lower.tail) p else log(-expm1(p)) } else {
          if (!lower.tail) log(p) else log1p(-p)
        }
        anchor <- actuar::ptrgamma(exp(-700), shape1, 1, scale = 1, log.p = TRUE)
        recovered <- -700 + (lp - anchor)/shape1
        logg[lost] <- recovered[lost]
      }
      exp(log(scale) - logg/shape2)
    }
  ),
  "actuar::rinvtrgamma" = list(
    broken = function() {
      had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
      if (had_seed) saved_seed <- get(".Random.seed", envir = .GlobalEnv)
      on.exit(if (had_seed) assign(".Random.seed", saved_seed, envir = .GlobalEnv)
              else rm(".Random.seed", envir = .GlobalEnv))
      set.seed(1)
      z <- actuar::rinvtrgamma(1000, 100, .0065, scale = exp(log(100)/.0065))
      near_power <- actuar::rinvtrgamma(10, 1e-6, 1e6)
      any(!is.finite(z) | z == 0) || any(!is.finite(near_power) | near_power == 0)
    },
    replacement = function(n, shape1, shape2, rate = 1, scale = 1/rate) {
      if (any(is.finite(shape1) & shape1 > 0 & shape1 < .01))
        return(univariateML_reference_fixes[["actuar::qinvtrgamma"]]$replacement(
          stats::runif(n), shape1, shape2, scale = scale))
      exp(log(scale) + log(actuar::rinvgamma(n, shape1, scale = 1))/shape2)
    }
  ),
  "actuar::ptrgamma" = list(
    broken = function() {
      actuar::ptrgamma(.9, 1e-6, 1e6) == 0
    },
    replacement = function(q, shape1, shape2, rate = 1, scale = 1/rate,
                           lower.tail = TRUE, log.p = FALSE) {
      ans <- actuar::ptrgamma(q, shape1, shape2, scale = scale,
                                lower.tail = lower.tail, log.p = log.p)
      logu <- shape2 * (log(q) - log(scale))
      lost <- !is.na(logu) & logu < -700 & q > 0
      if (any(lost)) {
        anchor <- actuar::ptrgamma(exp(-700), shape1, 1, scale = 1, log.p = TRUE)
        lp <- anchor + shape1 * (logu + 700)
        value <- if (lower.tail) { if (log.p) lp else exp(lp) } else {
          if (log.p) log(-expm1(lp)) else -expm1(lp)
        }
        ans[lost] <- value[lost]
      }
      ans
    }
  ),
  "actuar::pinvtrgamma" = list(
    broken = function() {
      actuar::pinvtrgamma(1/.9, 1e-6, 1e6) == 1
    },
    replacement = function(q, shape1, shape2, rate = 1, scale = 1/rate,
                           lower.tail = TRUE, log.p = FALSE) {
      ans <- actuar::pinvtrgamma(q, shape1, shape2, scale = scale,
                                lower.tail = lower.tail, log.p = log.p)
      logu <- shape2 * (log(scale) - log(q))
      lost <- !is.na(logu) & logu < -700 & q > 0
      if (any(lost)) {
        anchor <- actuar::ptrgamma(exp(-700), shape1, 1, scale = 1, log.p = TRUE)
        lp <- anchor + shape1 * (logu + 700)
        value <- if (!lower.tail) { if (log.p) lp else exp(lp) } else {
          if (log.p) log(-expm1(lp)) else -expm1(lp)
        }
        ans[lost] <- value[lost]
      }
      ans
    }
  )
)

#' Quantile functions for reference packages that have none
#'
#' Keyed by the missing reference quantile function (`pkg::qfn`). For a
#' discrete model whose reference package has d/p/r but no q, `qml()` (and
#' `plot()`, which calls it for unbounded support) uses the generalised
#' inverse of the reference's own CDF, the smallest support point `q` with
#' `F(q) >= u`, found by bisection on the integers
#' (`univariateML_discrete_quantile()`). This list is the single switch:
#' delete an entry and `qml()` for that model stops with an error naming the
#' missing function. An entry is used only while the reference package
#' lacks the function, so an upstream quantile function takes over without
#' changes here.
#'
#' Each entry has `p`, the reference CDF (`pkg::pfn`, taking `lower.tail`
#' and `log.p`), and `support`, a function of the coefficients (with `...`)
#' returning the integer support bounds `c(lower, upper)`; `upper` may be
#' `Inf`.
#'
#' `extraDistr::qbbinom` does not exist (extraDistr 1.10.0.5); the
#' beta-binomial (`mlbbinom`) quantile inverts `extraDistr::pbbinom` on
#' `0:size`.
#' @keywords internal

univariateML_reference_quantiles <- list(
  "extraDistr::qbbinom" = list(
    p = "extraDistr::pbbinom",
    support = function(size, ...) c(0, size)
  ),
  "extraDistr::qbnbinom" = list(
    p = "extraDistr::pbnbinom",
    support = function(...) c(0, Inf),
    work_budget = 1e6
  )
)

#' Generalised inverse of a reference CDF on the integers
#'
#' @param entry An entry of `univariateML_reference_quantiles`.
#' @return A quantile function with the CDF's arguments, `q` renamed `p`.
#'   For lower-tail probabilities it returns the smallest support point `q`
#'   with `F(q) >= p`; for upper-tail ones the smallest `q` with
#'   `P(X > q) <= p`, comparing in the scale the CDF reports (`log.p`).
#' @keywords internal

univariateML_discrete_quantile <- function(entry) {
  cdf <- eval(parse(text = entry$p))
  fix <- univariateML_reference_fixes[[entry$p]]
  if (!is.null(fix) && fix$broken()) cdf <- fix$replacement
  support <- entry$support
  fun <- function() {
    args <- as.list(environment())
    params <- args[setdiff(names(args), c("p", "lower.tail", "log.p"))]
    univariateML_invert_cdf(args$p, cdf, params, support, args$lower.tail, args$log.p, entry$work_budget)
  }
  arguments <- formals(cdf)
  names(arguments)[1L] <- "p"
  formals(fun) <- arguments
  fun
}

#' Integer bisection for `univariateML_discrete_quantile()`
#'
#' @param p Probabilities, on the scale given by `log.p`.
#' @param cdf The reference CDF.
#' @param params Named list of single coefficient values.
#' @param support Function of the coefficients giving `c(lower, upper)`.
#' @param lower.tail,log.p As in the reference CDF.
#' @return Quantiles; `NaN` (with a warning) for probabilities out of range.
#' @keywords internal

univariateML_invert_cdf <- function(p, cdf, params, support, lower.tail, log.p, work_budget = NULL) {
  if (any(lengths(params) != 1L)) stop("coefficients must be single numbers", call. = FALSE)
  bounds <- do.call(support, params)
  lower <- bounds[[1L]]
  upper <- bounds[[2L]]
  work <- 0
  at <- function(q) {
    if (!is.null(work_budget)) {
      work <<- work + sum(pmax(0, q + 1))
      if (!is.finite(work) || work > work_budget)
        stop("Discrete quantile CDF work budget exceeded; the requested tail is too large.", call. = FALSE)
    }
    do.call(cdf, c(list(q), params, list(lower.tail = lower.tail, log.p = log.p)))
  }
  reached <- function(q, u) if (lower.tail) at(q) >= u else at(q) <= u
  out <- rep(NaN, length(p))
  out[is.na(p)] <- p[is.na(p)]
  inside <- !is.na(p) & (if (log.p) p <= 0 else p >= 0 & p <= 1)
  if (any(!inside & !is.na(p))) warning("NaNs produced", call. = FALSE)
  # R quantile endpoint convention, including unbounded supports.
  bottom <- inside & if (log.p) (if (lower.tail) p == -Inf else p == 0) else (if (lower.tail) p == 0 else p == 1)
  top <- inside & if (log.p) (if (lower.tail) p == 0 else p == -Inf) else (if (lower.tail) p == 1 else p == 0)
  out[bottom] <- lower
  out[top] <- upper
  inside <- inside & !bottom & !top
  u <- p[inside]
  if (!length(u)) return(out)
  # Invariant: the quantile lies in (a, b]. The CDF has not reached u at a
  # (a point below the support counts as not reached) and has at b (the
  # upper end of a finite support counts as reached).
  a <- rep(lower - 1, length(u))
  b <- rep(upper, length(u))
  if (is.infinite(lower)) {
    a[] <- -1; b[] <- 0
    step <- 1
    todo <- reached(a, u)
    while (any(todo)) {
      b[todo] <- a[todo]
      step <- step * 2
      if (step > 2^52) stop("Quantile exceeds exact integer range.")
      a[todo] <- -step
      todo[todo] <- reached(a[todo], u[todo])
    }
  }
  if (is.infinite(upper)) {
    if (is.finite(lower)) b[] <- lower
    step <- 1
    todo <- !reached(b, u)
    while (any(todo)) {
      if (step > 2^52) {
        b[todo] <- Inf
        break
      }
      a[todo] <- b[todo]
      b[todo] <- (if (is.finite(lower)) lower else 0) + step
      step <- 2 * step
      todo[todo] <- !reached(b[todo], u[todo])
    }
  }
  todo <- is.finite(b) & b - a > 1
  while (any(todo)) {
    mid <- floor((a[todo] + b[todo]) / 2)
    hit <- reached(mid, u[todo])
    b[todo] <- ifelse(hit, mid, b[todo])
    a[todo] <- ifelse(hit, a[todo], mid)
    todo <- is.finite(b) & b - a > 1
  }
  out[inside] <- b
  out
}

#' Does a reference function exist?
#'
#' `FALSE` only when the package is installed and does not export the
#' function; any other failure is left to the caller's evaluation.
#' @param string `pkg::fn` or a bare function name.
#' @keywords internal

univariateML_reference_exists <- function(string) {
  parts <- strsplit(string, "::", fixed = TRUE)[[1]]
  if (length(parts) != 2L || !requireNamespace(parts[1], quietly = TRUE)) return(TRUE)
  parts[2] %in% getNamespaceExports(parts[1])
}

#' Transform a univariateML object to a string specifying quantile, CDF,
#'     density or random variate generation.
#'
#' @param obj A univariateML object.
#' @param type A type.
#' @return A string
#' @keywords internal

univariateML_to_string <- function(obj, type = c("d", "p", "q", "r", "ml")) {
  type <- match.arg(type)
  density <- attr(obj, "density")
  parts <- strsplit(density, "::", fixed = TRUE)[[1]]
  # Density may be either "pkg::dfoo" (external function) or just "dfoo"
  # (lives in our namespace).
  if (length(parts) == 2L) {
    fname <- parts[2]
    pkg <- parts[1]
  } else {
    fname <- parts[1]
    pkg <- NULL
  }
  if (type %in% c("d", "p", "q", "r")) {
    substring(fname, first = 1, last = 1) <- type
    if (is.null(pkg)) fname else paste0(pkg, "::", fname)
  } else {
    name <- univariateML_name(obj)
    if (is.null(name)) paste0(type, substring(fname, 2)) else name
  }
}

#' Transform a univariateML object to a quantile, CDF, density or random
#'    variate generation.
#'
#' @param obj A univariateML object.
#' @param type A type.
#' @return A string
#' @keywords internal

univariateML_to_function <- function(obj, type = c("d", "p", "q", "r", "ml")) {
  type <- match.arg(type)
  # A held half-normal endpoint is closed, unlike the translated dtnorm
  # convention. Compose the reference half-normal with the known translation.
  if (identical(univariateML_name(obj), "mlhnorm") &&
      "mu" %in% univariateML_fixed(obj) && type %in% c("d", "p", "q", "r")) {
    key <- paste0("extraDistr::", type, "hnorm")
    fun <- getExportedValue("extraDistr", paste0(type, "hnorm"))
    repair <- univariateML_reference_fixes[[key]]
    if (!is.null(repair) && repair$broken()) fun <- repair$replacement
    mu <- unname(obj["mu"]); sigma <- unname(obj["sigma"])
    return(switch(type,
      d = function(x, log = FALSE) fun(x - mu, sigma = sigma, log = log),
      p = function(q, lower.tail = TRUE, log.p = FALSE)
        fun(q - mu, sigma = sigma, lower.tail = lower.tail, log.p = log.p),
      q = function(p, lower.tail = TRUE, log.p = FALSE)
        mu + fun(p, sigma = sigma, lower.tail = lower.tail, log.p = log.p),
      r = function(n) mu + fun(n, sigma = sigma)))
  }
  string <- univariateML_to_string(obj, type = type)
  if (type == "q" && !univariateML_reference_exists(string)) {
    # A reference without a quantile function; see univariateML_reference_quantiles.
    entry <- univariateML_reference_quantiles[[string]]
    if (is.null(entry)) {
      stop("qml() is not available for the ", attr(obj, "model"), " model: the reference ",
           "package has no quantile function `", string, "`.", call. = FALSE)
    }
    fun <- univariateML_discrete_quantile(entry)
  } else {
    fun <- eval(parse(text = string))
  }
  fix <- univariateML_reference_fixes[[string]]
  if (!is.null(fix) && fix$broken()) fun <- fix$replacement
  if (type %in% c("d", "p", "q", "r")) {
    values <- univariateML_dpqr_args(obj, univariateML_name(obj))
    for (arg in names(values)) formals(fun)[[arg]] <- values[[arg]]
  }
  fun
}

#' Data and function to 'univariateML'
#' @param y Data to transform.
#' @param obj Function to apply.
#' @keywords internal

to_univariateML <- function(y, obj) {
  if (inherits(obj, "univariateML")) {
    obj <- obj
  } else {
    msg <- paste0(
      "obj must be either a function returning a univariateML ",
      "object or an univariateML object."
    )
    tryCatch(
      {
        obj <- obj(y)
      },
      error = function(cond) stop(msg)
    )
    stopifnot(inherits(obj, "univariateML"))
  }

  obj
}


# extraDistr 1.10.0.5 subtracts rounded/underflowed normal probabilities.
# Reconstruct conditional probabilities by integrating its untruncated normal
# density relative to the interval mode. No independent R density is supplied.
# The normal scaling identity keeps reference log densities away from underflow.
univariateML_reference_fixes <- c(univariateML_reference_fixes, local({
  lognormal <- function(z) {
    k <- pmax(1, abs(z) / 8)
    c0 <- extraDistr::dtnorm(0, log = TRUE)
    c0 + k^2 * (extraDistr::dtnorm(z / k, log = TRUE) - c0)
  }
  coordinates <- function(mean, sd, a, b) {
    centre <- min(max(mean, a), b); scale <- min(sd, b-a)
    list(centre = centre, scale = scale, t1 = ((mean-centre)/sd)*(scale/sd),
         t2 = -.5*(scale/sd)^2, lo = (a-centre)/scale, hi = (b-centre)/scale)
  }
  mass <- function(a, b, t1, t2) {
    if (a >= b) return(-Inf)
    mode <- min(max(-t1/(2*t2), a), b)
    g <- t1 + 2*t2*mode
    root <- sqrt(g*g - 320*t2)
    if (g >= 0) {
      right <- (-g-root)/(2*t2); left <- 80/(t2*right)
    } else {
      left <- (-g+root)/(2*t2); right <- 80/(t2*left)
    }
    lo <- max(a-mode, left); hi <- min(b-mode, right); w <- hi-lo
    kernel <- function(u) {
      d <- lo + w*u
      exp(lognormal(sqrt(-2*t2)*d) - lognormal(0) + g*d)
    }
    mode*(t1+t2*mode) + log(w) + log(stats::integrate(kernel, 0, 1,
      rel.tol = 2e-12, subdivisions = 200L)$value)
  }
  broken <- function() {
    z <- suppressWarnings(extraDistr::dtnorm(10.05, a = 10, log = TRUE))
    !is.finite(z) || abs(z - 1.81109661730779) > 1e-9 ||
      !is.finite(suppressWarnings(extraDistr::dtnorm(40.01, a = 40, b = 41, log = TRUE))) ||
      !is.finite(suppressWarnings(extraDistr::dtnorm(-39.9, a = -40, log = TRUE)))
  }
  density <- function(x, mean = 0, sd = 1, a = -Inf, b = Inf, log = FALSE) {
    c <- coordinates(mean, sd, a, b)
    total <- mass(c$lo, c$hi, c$t1, c$t2)
    u <- (x-c$centre)/c$scale
    ans <- lognormal(sqrt(-2*c$t2)*u) - lognormal(0) + c$t1*u - base::log(c$scale) - total
    ans[x <= a | x >= b] <- -Inf
    if (log) ans else exp(ans)
  }
  cdf <- function(q, mean = 0, sd = 1, a = -Inf, b = Inf,
                  lower.tail = TRUE, log.p = FALSE) {
    c <- coordinates(mean, sd, a, b)
    al <- c$lo; be <- c$hi
    total <- mass(al, be, c$t1, c$t2)
    vapply(q, function(x) {
      z <- (x - c$centre)/c$scale
      if (is.na(z)) return(z)
      if (z <= al) lp <- if (lower.tail) -Inf else 0
      else if (z >= be) lp <- if (lower.tail) 0 else -Inf
      else {
        below <- mass(al, z, c$t1, c$t2) - total; above <- mass(z, be, c$t1, c$t2) - total
        complement <- function(v) log(-expm1(min(0, v)))
        if (below > above) below <- complement(above) else above <- complement(below)
        lp <- if (lower.tail) below else above
      }
      if (log.p) lp else exp(lp)
    }, numeric(1))
  }
  quantile <- function(p, mean = 0, sd = 1, a = -Inf, b = Inf,
                       lower.tail = TRUE, log.p = FALSE) {
    vapply(p, function(v) {
      lp <- if (log.p) v else log(v)
      if (is.na(lp) || lp > 0) return(NaN)
      if (lp == -Inf) return(if (lower.tail) a else b)
      if (lp == 0) return(if (lower.tail) b else a)
      # Solve for the smaller tail in standardized coordinates.
      lower <- lower.tail
      if (lp > -log(2)) { lp <- log(-expm1(lp)); lower <- !lower }
      c <- coordinates(mean, sd, a, b)
      al <- c$lo; be <- c$hi
      centre <- 0
      f <- function(z) cdf(c$centre + c$scale*z, mean = mean, sd = sd,
                            a = a, b = b, lower.tail = lower, log.p = TRUE) - lp
      lo <- if (is.finite(al)) al else centre - 1
      hi <- if (is.finite(be)) be else centre + 1
      step <- 1
      while (!is.finite(al) && (if (lower) f(lo) > 0 else f(lo) < 0)) {
        step <- step * 2; lo <- centre - step
      }
      step <- 1
      while (!is.finite(be) && (if (lower) f(hi) < 0 else f(hi) > 0)) {
        step <- step * 2; hi <- centre + step
      }
      z <- stats::uniroot(f, c(lo, hi), tol = 1e-12)$root
      min(max(c$centre + c$scale*z, a), b)
    }, numeric(1))
  }
  list("extraDistr::dtnorm" = list(broken = broken, replacement = density),
       "extraDistr::ptnorm" = list(broken = function() {
         !isTRUE(all.equal(suppressWarnings(extraDistr::ptnorm(10.05, a = 10)),
                          0.3971837723052376, tolerance = 1e-12)) ||
           !is.finite(suppressWarnings(extraDistr::ptnorm(40.01, a = 40, b = 41)))
       }, replacement = cdf),
       "extraDistr::qtnorm" = list(broken = function() {
         !is.finite(suppressWarnings(extraDistr::qtnorm(.5, a = 40, b = 41)))
       }, replacement = quantile))
}))

# extraDistr GEV: stable transforms of its own Gumbel functions.
univariateML_reference_fixes <- c(univariateML_reference_fixes, local({
  quantile <- function(p, mu = 0, sigma = 1, xi = 0,
                       lower.tail = TRUE, log.p = FALSE) {
    z <- extraDistr::qgumbel(p, lower.tail = lower.tail, log.p = log.p)
    n <- max(length(z), length(mu), length(sigma), length(xi))
    if (!length(z) || !length(mu) || !length(sigma) || !length(xi)) return(numeric())
    z <- rep_len(z, n); xi <- rep_len(xi, n)
    y <- z
    nonzero <- which(!is.na(xi) & xi != 0)
    y[nonzero] <- expm1(xi[nonzero] * z[nonzero])/xi[nonzero]
    out <- rep_len(mu, n) + rep_len(sigma, n) * y
    out[is.na(xi) | rep_len(sigma, n) <= 0] <- NaN
    out
  }
  cdf <- function(q, mu = 0, sigma = 1, xi = 0,
                  lower.tail = TRUE, log.p = FALSE) {
    # Retain the reference for all but the unstable upper-tail logarithm.
    if (lower.tail || !log.p)
      return(extraDistr::pgev(q, mu, sigma, xi, lower.tail, log.p))
    n <- max(length(q), length(mu), length(sigma), length(xi))
    if (!length(q) || !length(mu) || !length(sigma) || !length(xi)) return(numeric())
    z <- (rep_len(q, n) - rep_len(mu, n))/rep_len(sigma, n)
    xi <- rep_len(xi, n)
    t <- -z
    j <- which(!is.na(xi) & xi != 0 & !is.na(z) & 1 + xi*z > 0)
    t[j] <- -log1p(xi[j]*z[j])/xi[j]
    # log(1-exp(-exp(t))) tends to t below the exponential underflow range.
    ans <- t
    j <- which(!is.na(t) & t > -36)
    ans[j] <- log(-expm1(-exp(t[j])))
    ans[which(!is.na(z) & !is.na(xi) & xi*z <= -1 & xi > 0)] <- 0
    ans[which(!is.na(z) & !is.na(xi) & xi*z <= -1 & xi < 0)] <- -Inf
    ans[is.na(xi) | rep_len(sigma, n) <= 0] <- NaN
    ans
  }
  list(
    "extraDistr::qgev" = list(
      broken = function() {
        !isTRUE(all.equal(extraDistr::qgev(.5, xi = 1e-16),
                         -log(log(2)), tolerance = 1e-12)) ||
          extraDistr::qgev(1, xi = -.5) != 2
      }, replacement = quantile),
    "extraDistr::pgev" = list(
      broken = function() {
        !isTRUE(all.equal(extraDistr::pgev(1000, lower.tail = FALSE, log.p = TRUE),
                         -1000, tolerance = 1e-12))
      }, replacement = cdf),
    "extraDistr::rgev" = list(
      broken = function() {
        had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
        if (had_seed) saved_seed <- get(".Random.seed", envir = .GlobalEnv)
        on.exit(if (had_seed) assign(".Random.seed", saved_seed, envir = .GlobalEnv)
                else rm(".Random.seed", envir = .GlobalEnv))
        set.seed(1)
        actual <- extraDistr::rgev(10, xi = 1e-16)
        set.seed(1)
        expected <- expm1(1e-16 * extraDistr::rgumbel(10))/1e-16
        !isTRUE(all.equal(sort(actual), sort(expected), tolerance = 1e-12))
      },
      replacement = function(n, mu = 0, sigma = 1, xi = 0) {
        z <- extraDistr::rgumbel(n)
        nout <- length(z)
        xi <- rep_len(xi, nout)
        y <- z
        j <- which(!is.na(xi) & xi != 0)
        y[j] <- expm1(xi[j]*z[j])/xi[j]
        ans <- rep_len(mu, nout) + rep_len(sigma, nout)*y
        ans[is.na(xi) | rep_len(sigma, nout) <= 0] <- NaN
        ans
      })
  )
}))

# Recycle parameter vectors while retaining scalar integration coordinates.
for (.key in paste0("extraDistr::", c("dtnorm", "ptnorm", "qtnorm"))) {
  univariateML_reference_fixes[[.key]]$replacement <- local({
    scalar <- univariateML_reference_fixes[[.key]]$replacement
    wrapper <- function() NULL
    # Preserve the reference formal names (including x/q/p and flags).
    formals(wrapper) <- formals(scalar)
    body(wrapper) <- substitute({
      x <- ARG
      if (any(lengths(list(x, mean, sd, a, b)) == 0)) return(numeric())
      mapply(function(x, mean, sd, a, b) {
        if (anyNA(c(mean, sd, a, b)) || !is.finite(mean) ||
            !is.finite(sd) || sd <= 0 || a >= b) return(NaN)
        CALL
      }, x, mean, sd, a, b, USE.NAMES = FALSE)
    }, list(ARG = as.name(names(formals(scalar))[1]),
            CALL = if ("log" %in% names(formals(scalar)))
              quote(scalar(x, mean, sd, a, b, log = log)) else
              quote(scalar(x, mean, sd, a, b, lower.tail = lower.tail, log.p = log.p))))
    wrapper
  })
}
rm(.key)

# Folded reference families: evaluate tails before folding, avoiding subtraction
# of a rounded CDF from one. Keep reference samplers (and their seeded streams).
univariateML_reference_fixes <- c(univariateML_reference_fixes, local({
  complement <- function(lp) log(-expm1(pmin(0, lp)))
  make <- function(id) {
    is_t <- id == "ht"
    mode <- function(nu) {
      if (is_t) extraDistr::dht(0, nu, log = TRUE)
      else if (id == "hcauchy") extraDistr::dhcauchy(0, log = TRUE)
      else extraDistr::dhnorm(0, log = TRUE)
    }
    density <- function(x, nu = 1, sigma = 1, log = FALSE) {
      if (any(lengths(list(x, nu, sigma)) == 0)) return(numeric())
      lz <- base::log(abs(x)) - base::log(sigma)
      z <- 2*lz - base::log(nu)
      kernel <- pmax(z, 0) + log1p(exp(-abs(z)))
      ans <- mode(nu) - base::log(sigma) - (nu+1)/2*kernel
      ans[x < 0] <- -Inf
      ans[sigma <= 0 | !is.finite(sigma) | nu <= 0 | !is.finite(nu)] <- NaN
      if (log) ans else exp(ans)
    }
    cdf <- function(q, nu = 1, sigma = 1, lower.tail = TRUE, log.p = FALSE) {
      if (any(lengths(list(q, nu, sigma)) == 0)) return(numeric())
      z <- pmax(q, 0)/sigma
      upper <- log(2) + if (is_t) stats::pt(z, nu, lower.tail = FALSE, log.p = TRUE)
        else if (id == "hcauchy") stats::pcauchy(z, lower.tail = FALSE, log.p = TRUE)
        else stats::pnorm(z, lower.tail = FALSE, log.p = TRUE)
      lp <- if (lower.tail) complement(upper) else upper
      small <- which(!is.na(z) & z < 1e-8 * pmin(1, sqrt(nu)))
      if (length(small)) {
        linear <- mode(nu) + log(pmax(q, 0)) - log(sigma)
        if (!lower.tail) linear <- complement(linear)
        lp[small] <- rep_len(linear, length(lp))[small]
      }
      lp <- pmin(0, lp)
      lp[sigma <= 0 | !is.finite(sigma) | nu <= 0 | !is.finite(nu)] <- NaN
      if (log.p) lp else exp(lp)
    }
    quantile <- function(p, nu = 1, sigma = 1, lower.tail = TRUE, log.p = FALSE) {
      if (any(lengths(list(p, nu, sigma)) == 0)) return(numeric())
      lp <- if (log.p) p else log(p)
      upper <- (if (lower.tail) complement(lp) else lp) - log(2)
      ans <- sigma * if (is_t) stats::qt(upper, nu, lower.tail = FALSE, log.p = TRUE)
        else if (id == "hcauchy") stats::qcauchy(upper, lower.tail = FALSE, log.p = TRUE)
        else stats::qnorm(upper, lower.tail = FALSE, log.p = TRUE)
      if (lower.tail) {
        small <- which(!is.na(lp) & lp < -18)
        linear <- exp(lp - mode(nu) + log(sigma))
        ans[small] <- rep_len(linear, length(ans))[small]
      }
      ans[lp > 0 | sigma <= 0 | !is.finite(sigma) | nu <= 0 | !is.finite(nu)] <- NaN
      ans
    }
    # Preserve the public reference formals, omitting nu for the other families.
    if (!is_t) {
      d <- density; p <- cdf; q <- quantile
      density <- function(x, sigma = 1, log = FALSE) d(x, 1, sigma, log)
      cdf <- function(q, sigma = 1, lower.tail = TRUE, log.p = FALSE) p(q, 1, sigma, lower.tail, log.p)
      quantile <- function(p, sigma = 1, lower.tail = TRUE, log.p = FALSE) q(p, 1, sigma, lower.tail, log.p)
    }
    out <- list()
    if (id != "hnorm") out[[paste0("extraDistr::d", id)]] <- list(
      broken = local({ family <- id; function() {
        f <- getExportedValue("extraDistr", paste0("d", family))
        args <- if (family == "ht") list(nu = 3) else list()
        !is.finite(do.call(f, c(list(x = 1e200, sigma = 1e-200, log = TRUE), args)))
      }}), replacement = density)
    out[[paste0("extraDistr::p", id)]] <- list(
      broken = local({ family <- id; function() {
        f <- getExportedValue("extraDistr", paste0("p", family))
        args <- if (family == "ht") list(nu = 3) else list()
        !is.finite(do.call(f, c(list(q = if (family == "hnorm") 40 else 1e20, lower.tail = FALSE, log.p = TRUE), args))) ||
          do.call(f, c(list(q = 1e-20), args)) == 0
      }}), replacement = cdf)
    out[[paste0("extraDistr::q", id)]] <- list(
      broken = local({ family <- id; function() {
        f <- getExportedValue("extraDistr", paste0("q", family))
        args <- if (family == "ht") list(nu = 3) else list()
        actual <- do.call(f, c(list(p = -100, lower.tail = FALSE, log.p = TRUE), args))
        expected <- if (family == "ht") stats::qt(-100-log(2), 3, lower.tail = FALSE, log.p = TRUE)
          else if (family == "hcauchy") stats::qcauchy(-100-log(2), lower.tail = FALSE, log.p = TRUE)
          else stats::qnorm(-100-log(2), lower.tail = FALSE, log.p = TRUE)
        !isTRUE(all.equal(actual, expected, tolerance = 1e-12))
      }}), replacement = quantile)
    out
  }
  c(make("hcauchy"), make("ht"), make("hnorm"))
}))
# Stable conditional probabilities from the reference law's untruncated R
# counterpart. Log-tail differences avoid rounded CDF subtraction.
univariateML_reference_fixes <- c(univariateML_reference_fixes, local({
  difference <- function(x, y) x + log(-expm1(pmin(0, y-x)))
  entries <- list()
  for (family in c("tpois", "tbinom")) local({
    id <- family
    binomial <- id == "tbinom"
    mass <- function(a, b, lambda = NULL, size = NULL, prob = NULL) {
      if (a >= b) return(-Inf)
      cdf <- function(x, lower) if (binomial) stats::pbinom(x, size, prob, lower.tail=lower, log.p=TRUE) else stats::ppois(x, lambda, lower.tail=lower, log.p=TRUE)
      centre <- if (binomial) size*prob else lambda
      if (a >= centre) difference(cdf(a,FALSE),cdf(b,FALSE))
      else difference(cdf(b,TRUE),cdf(a,TRUE))
    }
    density <- function(x, lambda, a=-Inf, b=Inf, log=FALSE) NULL
    cdf <- function(q, lambda, a=-Inf, b=Inf, lower.tail=TRUE, log.p=FALSE) NULL
    if (binomial) {
      formals(density) <- alist(x=, size=, prob=, a=-Inf, b=Inf, log=FALSE)
      formals(cdf) <- alist(q=, size=, prob=, a=-Inf, b=Inf, lower.tail=TRUE, log.p=FALSE)
    }
    body(density) <- quote({
      if(any(lengths(if(binomial)list(x,size,prob,a,b)else list(x,lambda,a,b))==0))return(numeric())
      evaluate <- function(x,a,b,...) {
        args <- list(...)
        z <- do.call(mass,c(list(a=a,b=b),args))
        lp <- if(binomial) stats::dbinom(x,args$size,args$prob,log=TRUE) else stats::dpois(x,args$lambda,log=TRUE)
        ans <- lp-z; ans[x<=a|x>b] <- -Inf
        if(log) ans else exp(ans)
      }
      if(binomial) mapply(evaluate,x,a,b,size=size,prob=prob,USE.NAMES=FALSE)
      else mapply(evaluate,x,a,b,lambda=lambda,USE.NAMES=FALSE)
    })
    body(cdf) <- quote({
      if(any(lengths(if(binomial)list(q,size,prob,a,b)else list(q,lambda,a,b))==0))return(numeric())
      evaluate <- function(q,a,b,...) {
        if(is.na(q))return(q)
        args<-list(...); total<-do.call(mass,c(list(a=a,b=b),args))
        lo<-max(a,min(b,floor(q)))
        below<-do.call(mass,c(list(a=a,b=lo),args))-total
        above<-do.call(mass,c(list(a=lo,b=b),args))-total
        if(below>above) below<-log(-expm1(min(0,above))) else above<-log(-expm1(min(0,below)))
        ans<-if(lower.tail)below else above
        if(log.p)ans else exp(ans)
      }
      if(binomial)mapply(evaluate,q,a,b,size=size,prob=prob,USE.NAMES=FALSE)
      else mapply(evaluate,q,a,b,lambda=lambda,USE.NAMES=FALSE)
    })
    bounds <- if(binomial) function(size,prob,a=-Inf,b=Inf,...) c(max(0,floor(a)+1),min(size,floor(b))) else function(lambda,a=-Inf,b=Inf,...) c(max(0,floor(a)+1),if(lambda==0 && a<0 && b>=0)0 else floor(b))
    quantile <- function()NULL
    formals(quantile)<-formals(cdf);names(formals(quantile))[1]<-"p"
    body(quantile)<-quote({
      params<-if(binomial)list(size=size,prob=prob,a=a,b=b)else list(lambda=lambda,a=a,b=b)
      n<-max(c(length(p),lengths(params)))
      if(!length(p)||any(lengths(params)==0))return(numeric())
      vapply(seq_len(n),function(i)univariateML_invert_cdf(rep_len(p,n)[i],cdf,lapply(params,function(z)rep_len(z,n)[i]),bounds,lower.tail,log.p),0.)
    })
    random <- function()NULL
    formals(random)<-formals(density);names(formals(random))[1]<-"n";formals(random)$log<-NULL
    body(random)<-quote({
      if(length(n)>1)n<-length(n)
      if(binomial)quantile(stats::runif(n),size,prob,a,b)else quantile(stats::runif(n),lambda,a,b)
    })
    for(prefix in c("d","p","q","r")) local({
      pre <- prefix
      replacement <- switch(pre,d=density,p=cdf,q=quantile,r=random)
      broken <- function() {
        args <- if(binomial)list(size=100,prob=.1,a=90,b=100) else list(lambda=2,a=100,b=110)
        value <- switch(pre,d=if(binomial)91 else 101,p=if(binomial)99 else 109,q=c(0,.5,1),r=3)
        flags <- switch(pre,d=list(log=TRUE),p=list(lower.tail=FALSE,log.p=TRUE),list())
        callargs <- c(list(value),args,flags)
        reference <- getExportedValue("extraDistr",paste0(pre,id))
        if(pre=="r") {
          had_seed<-exists(".Random.seed",envir=.GlobalEnv,inherits=FALSE)
          if(had_seed)saved_seed<-get(".Random.seed",envir=.GlobalEnv)
          on.exit(if(had_seed)assign(".Random.seed",saved_seed,envir=.GlobalEnv) else rm(".Random.seed",envir=.GlobalEnv))
          set.seed(8); actual<-suppressWarnings(do.call(reference,callargs))
          set.seed(8); expected<-do.call(replacement,callargs)
        } else {
          actual<-suppressWarnings(do.call(reference,callargs));expected<-do.call(replacement,callargs)
        }
        !isTRUE(all.equal(actual,expected,tolerance=1e-10))
      }
      entries[[paste0("extraDistr::",pre,id)]] <<- list(broken=broken,replacement=replacement)
    })
  })
  entries
}))
univariateML_reference_fixes[["extraDistr::pdlaplace"]] <- list(
  broken=function() !is.finite(extraDistr::pdlaplace(1000,0,.5,FALSE,TRUE)) ||
    abs(extraDistr::pdlaplace(3,5,.5)-1/6)>1e-12,
  replacement=function(q,location,scale,lower.tail=TRUE,log.p=FALSE) {
    z<-floor(q-location)
    left<-(-z)*log(scale)-log1p(scale)
    right<-(z+1)*log(scale)-log1p(scale)
    lp<-ifelse(z<0,left,right)
    complement<-which(!is.na(z) & ((z<0) != lower.tail))
    lp[complement]<-log(-expm1(lp[complement]))
    if(log.p)lp else exp(lp)
  })
univariateML_reference_quantiles[["extraDistr::qdlaplace"]] <- list(
  p="extraDistr::pdlaplace",support=function(...)c(-Inf,Inf))
