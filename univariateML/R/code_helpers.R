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
