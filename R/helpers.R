#' Decorate estimator
#'
#' The decorator constructs an `ml***`. It first checks the input arguments,
#'    then call the defined estimator (with elements `estimates` and `logLik`),
#'    and the optional parameter `support`, then adds the required attributes.
#'
#' @param name Name of the `ml***` function.
#' @return The proper `ml***` function.
#' @keywords internal
decorator <- \(name) {
  force(name)
  fun <- \(x, na.rm = FALSE, ...) {
    x <- ml_check_modify(x, na.rm = na.rm, name = name)
    n <- length(x)
    out <- rlang::exec(paste0(name, "_"), x = x, ...)

    params <- list(logLik = out$logLik, call = deparse(match.call()), n = n)
    univariateML_construct(out$estimates, name = name, params = params)
  }
  meta <- metadata[[name]]
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
#' @return Object of class `univariateML`

univariateML_construct <- \(estimates, name, params) {
  estimates <- unname(estimates)
  class <- "univariateML"
  args <- c(.Data = list(estimates), params, metadata[[name]], class = class)
  object <- do.call(structure, args)
  attr(object, "call") <- if (length(attr(object, "call")) == 1) {
    str2lang(attr(object, "call"))
  } else {
    NULL
  }

  if (metadata[[name]]$support@type == "R") {
    attr(object, "continuous") <- TRUE
  } else {
    attr(object, "continuous") <- FALSE
  }

  support_names <- names(metadata[[name]]$support)
  if (is.null(support_names)) {
    attr(object, "support") <- c(metadata[[name]]$support@.Data)
  } else {
    support <- attr(object, "support")
    values <- as.list(stats::setNames(estimates, metadata[[name]]$names))
    supp <- unname(sapply(names(support), \(x) with(values, eval(str2lang(x)))))
    attr(object, "support") <- supp
  }
  object
}


ml_check_modify <- \(x, na.rm, name) {
  assertthat::assert_that(is.numeric(x))
  msg <- paste0("x is not a numeric vector (NCOL(x) = ", NCOL(x), ")")
  assertthat::assert_that(NCOL(x) == 1, msg = msg)
  msg <- "NA in input when na.rm = FALSE"
  if (na.rm) x <- x[!is.na(x)] else assertthat::assert_that(!anyNA(x), msg = msg)

  support <- metadata[[name]]$support

  msg <- "x not in the support of the data"
  if (support@closed[1]) {
    assertthat::assert_that(min(x) >= support[[1]], msg = msg)
  } else {
    assertthat::assert_that(min(x) > support[[1]], msg = msg)
  }

  if (support@closed[2]) {
    assertthat::assert_that(max(x) <= support[[2]], msg = msg)
  } else {
    assertthat::assert_that(max(x) < support[[2]], msg = msg)
  }

  if (support@type == "Z") {
    if (sum(abs(x - floor(x)) != 0)) {
      stop("Non-integral inputs for discrete density.")
    }
  }

  x
}

#' Transform a univariateML object to a string specifying quantile, CDF,
#'     density or random variate generation.
#'
#' @param obj A univariateML object.
#' @param type A type.
#' @return A string
#' @keywords internal

univariateML_to_string <- \(obj, type = c("d", "p", "q", "r", "ml")) {
  type <- match.arg(type)
  if (type %in% c("d", "p", "q", "r")) {
    strings <- strsplit(attr(obj, "density"), "::")[[1]]
    substring(strings[2], first = 1, last = 1) <- type
    paste0(strings[1], "::", strings[2])
  } else {
    paste0(type, substring(strsplit(attr(obj, "density"), "::")[[1]][2], 2))
  }
}

#' Transform a univariateML object to a quantile, CDF, density or random
#'    variate generation.
#'
#' @param obj A univariateML object.
#' @param type A type.
#' @return A string
#' @keywords internal

univariateML_to_function <- \(obj, type = c("d", "p", "q", "r", "ml")) {
  type <- match.arg(type)
  string <- univariateML_to_string(obj, type = type)
  fun <- eval(parse(text = string))
  if (type %in% c("d", "p", "q", "r")) {
    for (i in seq_along(obj)) formals(fun)[[names(obj)[i]]] <- unname(obj[i])
  }
  fun
}

#' Data and function to 'univariateML'
#' @param y Data to transform.
#' @param obj Function to apply.
#' @keywords internal

to_univariateML <- \(y, obj) {
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
      error = \(cond) stop(msg)
    )
    stopifnot(inherits(obj, "univariateML"))
  }

  obj
}
