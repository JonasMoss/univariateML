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
#' `sads::qzipf` (sads 0.6.5) returns the quantile minus one for
#' probabilities strictly between two CDF values, e.g.
#' `sads::qzipf(0.5, N = 10, s = 1)` is 1, not 2. The replacement inverts
#' the CDF built from `sads::dzipf`.
#' @keywords internal

univariateML_reference_fixes <- list(
  "sads::qzipf" = list(
    broken = function() {
      tryCatch(sads::qzipf(0.5, N = 10, s = 1) != 2, error = function(e) TRUE)
    },
    replacement = function(p, N, s, lower.tail = TRUE, log.p = FALSE) {
      if (log.p) p <- exp(p)
      if (!lower.tail) p <- 1 - p
      cdf <- cumsum(sads::dzipf(seq_len(N), N = N, s = s))
      q <- pmin(findInterval(p, cdf, left.open = TRUE) + 1, N)
      q[is.na(p) | p < 0 | p > 1] <- NaN
      q
    }
  ),
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
  )
)

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
  string <- univariateML_to_string(obj, type = type)
  fun <- eval(parse(text = string))
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
