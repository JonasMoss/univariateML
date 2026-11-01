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
    univariateML_construct(out$estimates, name = name, params = params, fixed = out$fixed,
                           restricted = out$restricted, data = x)
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
#' @param restricted Named character vector of coefficients at lower/upper fitting restrictions.
#' @param data Checked observations retained for inference, or `NULL`.
#' @return Object of class `univariateML`

univariateML_construct <- function(estimates, name, params, fixed = NULL, restricted = NULL, data = NULL) {
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
  if (!is.null(data)) {
    attr(object, "inference_data") <- data
    attr(object, "inference_free") <- stats::setNames(!names(object) %in% as.character(fixed), names(object))
  }
  if (!is.null(restricted)) attr(object, "restricted") <- restricted
  if (length(restricted)) univariateML_warn_restricted(name, restricted)
  object
}

# Emit one structured warning per fit, using the README-derived metadata.
univariateML_warn_restricted <- function(name, restricted) {
  params <- univariateML_metadata[[name]]$params
  declared <- vapply(params, function(p) !is.null(p$fit_domain), logical(1))
  coefs <- intersect(names(restricted), vapply(params[declared], `[[`, "", "name"))
  if (!length(coefs)) return(invisible(NULL))
  messages <- vapply(coefs, function(coef) {
    p <- Filter(function(p) identical(p$name, coef), params)[[1L]]
    domains <- p$fit_domain
    if (!is.null(domains$restriction)) domains <- list(domains)
    side <- restricted[[coef]]
    matches <- Filter(function(d) !is.null(d[[side]]), domains)
    d <- matches[[1L]]
    sprintf("`%s` = %s (%s bound): %s", coef, format(d[[side]]), side,
            d$restriction$reason)
  }, character(1))
  warning(structure(list(message = paste0(name, " reached a fitting restriction: ",
                                         paste(messages, collapse = "; ")),
                         call = NULL, restricted = restricted),
                    class = c("univariateML_restricted", "warning", "condition")))
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
#' (argument name to coefficient expression, collected in `univariateML_args`),
#' e.g. the stats hypergeometric reference maps `n` to `N - m`.
#' The resolver also supports `r.shift` for references without a location coefficient.
#'
#' @param estimates Named numeric vector of coefficients.
#' @param name Name of the `ml***` function, or `NULL`.
#' @return A named list of argument values.
#' @keywords internal

univariateML_dpqr_args <- function(estimates, name) {
  map <- if (is.null(name)) NULL else univariateML_args[[name]]
  values <- stats::setNames(as.list(as.numeric(estimates)), names(estimates))
  if (is.null(map)) {
    shift <- if (is.null(name)) NULL else univariateML_shifts[[name]]
    if (!is.null(shift)) values[[shift]] <- NULL
    return(values)
  }
  stats::setNames(lapply(unname(map), function(expr) eval(parse(text = expr), values)), names(map))
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
  string <- univariateML_to_string(obj, type = type)
  fun <- eval(parse(text = string))
  if (type %in% c("d", "p", "q", "r")) {
    values <- univariateML_dpqr_args(obj, univariateML_name(obj))
    for (arg in names(values)) formals(fun)[[arg]] <- values[[arg]]
    model_name <- univariateML_name(obj)
    shift_name <- if (is.null(model_name)) NULL else univariateML_shifts[[model_name]]
    if (!is.null(shift_name)) {
      shift <- unname(obj[[shift_name]])
      base_fun <- fun
      # Retain reference formals so dml/pml/qml recognize log/tail options,
      # and endpoint closure receives their defaults and explicit overrides.
      fun <- function() {
        call_args <- as.list(environment())
        if (type %in% c("d", "p")) call_args[[1L]] <- call_args[[1L]] - shift
        value <- do.call(base_fun, call_args)
        if (type %in% c("q", "r")) value + shift else value
      }
      formals(fun) <- formals(base_fun)
    }
  }
  if (type %in% c("d", "p", "q"))
    fun <- univariateML_close_function(fun, obj, type)
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


#' Reject argument names that are not exact adapter arguments
#'
#' The generated `ml***` wrappers pass `...` to their adapters, and R
#' matches argument names partially: `mlweibull(x, shape = 2)` would match
#' the start value `shape0` and estimate the shape the user meant to hold.
#' Every named argument must therefore be an exact adapter argument.
#'
#' @param adapter The adapter function `ml***_`.
#' @param dots `list(...)` from the wrapper.
#' @param name Name of the `ml***` function.
#' @return Invisible `NULL`; stops on an unknown name.
#' @keywords internal
ml_check_args <- function(adapter, dots, name) {
  given <- names(dots)
  if (is.null(given)) return(invisible(NULL))
  given <- given[nzchar(given)]
  allowed <- setdiff(names(formals(adapter)), "x")
  bad <- setdiff(given, allowed)
  if (!length(bad)) return(invisible(NULL))
  coefs <- univariateML_metadata[[name]]$names
  held <- intersect(bad, coefs)
  why <- if (length(held)) {
    paste0("`", name, "` cannot hold ", paste0("`", held, "`", collapse = ", "),
           " (not supported yet)")
  } else {
    paste0("`", name, "` has no argument ", paste0("`", bad, "`", collapse = ", "))
  }
  stop(why, "; its arguments are ", paste0("`", allowed, "`", collapse = ", "),
       ". Argument names must match exactly.", call. = FALSE)
}

# Resolve support in original data coordinates, independently of r.args/r.shift.
# The numeric metadata bounds remain the envelope used before fitting.
univariateML_support <- function(obj) {
  name <- univariateML_name(obj)
  s <- if (is.null(name)) NULL else univariateML_metadata[[name]]$support
  if (is.null(s)) return(NULL)
  values <- stats::setNames(as.list(as.numeric(obj)), names(obj))
  expressions <- s$expressions
  if (is.null(expressions)) return(s)
  s$bounds <- vapply(expressions, function(e)
    if (is.character(e)) eval(parse(text = e), values) else e, numeric(1))
  s$closed <- s$endpoint_closed
  s
}

# Close only coefficient-dependent continuous endpoints. Discrete support
# expressions already describe actual included lattice points (tpois: a+1).
univariateML_close_function <- function(fun, obj, type) {
  force(fun)
  s <- univariateML_support(obj)
  if (is.null(s) || !any(vapply(s$expressions, is.character, logical(1)))) return(fun)
  wrapper <- function() univariateML_closed_call(fun, obj, type, as.list(environment()))
  formals(wrapper) <- formals(fun)
  wrapper
}

univariateML_closed_call <- function(fun, obj, type, args) {
  # Preserve the reference function's argument order and explicit overrides.
  # Vector coefficients retain the reference's recycling semantics.
  name <- univariateML_name(obj)
  map <- univariateML_args[[name]]
  coefficients <- intersect(names(obj), names(args))
  for (coefficient in coefficients) {
    if (length(args[[coefficient]]) != 1L) return(do.call(fun, args))
    obj[[coefficient]] <- args[[coefficient]]
  }
  if (!is.null(map)) for (arg in intersect(names(map), names(args))) {
    coefficient <- map[[arg]]
    if (coefficient %in% names(obj)) {
      if (length(args[[arg]]) != 1L) return(do.call(fun, args))
      obj[[coefficient]] <- args[[arg]]
    }
  }
  # Reference overrides are still in (m,n), while support is expressed in
  # (m,N). Reconstruct the C6 population size after either count changes.
  if (!is.null(map) && identical(unname(map["n"]), "N - m") &&
      all(c("m", "N") %in% names(obj)) && !is.null(args$n)) {
    if (length(args$n) != 1L) return(do.call(fun, args))
    obj[["N"]] <- obj[["m"]] + args$n
  }
  s <- univariateML_support(obj)
  dynamic <- vapply(s$expressions, is.character, logical(1))
  at <- function(x) {
    call_args <- args
    call_args[[1L]] <- x
    call_args$log <- TRUE
    do.call(fun, call_args)
  }
  bounds <- s$bounds
  values <- stats::setNames(as.list(as.numeric(obj)), names(obj))
  if (type == "d") {
    x <- args[[1L]]; log <- isTRUE(args$log)
    ans <- do.call(fun, args)
    outside <- which(!is.na(x) & (x < bounds[1] | x > bounds[2]))
    ans[outside] <- if (log) -Inf else 0
    if (s$type != "R") return(ans)
    for (i in which(dynamic & s$closed & is.finite(bounds))) {
      hit <- which(!is.na(x) & x == bounds[i])
      if (!length(hit)) next
      expr <- s$endpoint_log_density[[c("lower", "upper")[i]]]
      if (!is.null(expr)) {
        env <- c(values, list(x = bounds[i]))
        ld <- if (is.character(expr)) eval(parse(text = expr), env) else expr
      } else {
        ld <- at(bounds[i])
        if (is.finite(ld)) {
          ans[hit] <- if (log) ld else exp(ld)
          next
        }
        # Smooth undeclared limits: extrapolate two nearby reference log
        # densities to remove the first-order displacement error.
        scale <- min(abs(bounds[2]-bounds[1]),
                     max(1, abs(unlist(values[names(values) %in% c("sd", "sigma", "scale")]))))
        step <- max(scale * 1e-7, abs(bounds[i]) * .Machine$double.eps * 2,
                    .Machine$double.xmin)
        direction <- if (i == 1L) 1 else -1
        z <- bounds[i] + direction * step
        zz <- bounds[i] + direction * step * 2
        if (!(z > bounds[1] && z < bounds[2] && zz > bounds[1] && zz < bounds[2]))
          stop("Cannot resolve the endpoint density within double precision.")
        l1 <- at(z); l2 <- at(zz)
        ld <- if (is.finite(l1) && is.finite(l2)) 2*l1-l2 else l1
      }
      ans[hit] <- if (log) ld else exp(ld)
    }
    return(ans)
  }
  if (type == "p") {
    q <- args[[1L]]; lower.tail <- args$lower.tail; log.p <- args$log.p
    ans <- do.call(fun, args)
    # Equality at the upper endpoint always includes all mass; equality at
    # a continuous lower endpoint has zero mass.
    low <- which(!is.na(q) & (q < bounds[1] | (s$type == "R" & q == bounds[1])))
    high <- which(!is.na(q) & q >= bounds[2])
    ans[low] <- if (log.p) { if (lower.tail) -Inf else 0 } else as.numeric(!lower.tail)
    ans[high] <- if (log.p) { if (lower.tail) 0 else -Inf } else as.numeric(lower.tail)
    return(ans)
  }
  {
    p <- args[[1L]]; lower.tail <- args$lower.tail; log.p <- args$log.p
    ans <- do.call(fun, args)
    low <- which(!is.na(p) & p == if (log.p) -Inf else 0)
    high <- which(!is.na(p) & p == if (log.p) 0 else 1)
    ans[low] <- bounds[if (lower.tail) 1L else 2L]
    ans[high] <- bounds[if (lower.tail) 2L else 1L]
    ans
  }
}
