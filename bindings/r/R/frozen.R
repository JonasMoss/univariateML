# Shared constructor for README-defined fully specified distributions.
ml_require_data <- function(object) {
  if (isTRUE(attr(object, "frozen")))
    stop("This model was built from fixed coefficients; it has no data", call. = FALSE)
  invisible(object)
}

ml_check_frozen_call <- function(call) {
  args <- as.list(call)[-1L]
  if (length(args) && (is.null(names(args)) || any(!nzchar(names(args)))))
    stop("Without data every argument must be named", call. = FALSE)
  invisible(NULL)
}

ml_fixed_model <- function(name, supplied, call, frozen = FALSE, x = numeric()) {
  if (frozen) return(ml_build_fixed_model(name, supplied, call, TRUE, x))
  force(x)
  # Preserve established fitter diagnostics for invalid holds, observations
  # and controls. Successful models always use the density path below; the
  # legacy all-held adapter is consulted only to diagnose rejected input.
  diagnose <- function() tryCatch(
    do.call(get(paste0(name, "_"), mode = "function"), c(list(x = x), supplied)),
    error = identity)
  controls <- intersect(names(supplied), c("reltol", "abstol", "gradtol", "iterlim"))
  invalid <- vapply(supplied[controls], function(v)
    !is.numeric(v) || length(v) != 1L || !is.finite(v) || v <= 0, logical(1))
  if (any(invalid)) {
    old <- diagnose()
    if (inherits(old, "error")) stop(old)
  }
  tryCatch(ml_build_fixed_model(name, supplied, call, FALSE, x), error = function(e) {
    old <- diagnose()
    if (inherits(old, "error")) stop(old)
    stop(e)
  })
}

ml_build_fixed_model <- function(name, supplied, call, frozen, x) {
  meta <- univariateML_metadata[[name]]
  if (frozen && length(supplied) &&
      (is.null(names(supplied)) || any(!nzchar(names(supplied)))))
    stop("Without data every argument must be named", call. = FALSE)
  if (anyDuplicated(names(supplied))) stop("Duplicate coefficient arguments", call. = FALSE)
  if (frozen && length(setdiff(names(supplied), meta$names)))
    stop("Without data supply only named coefficients", call. = FALSE)
  values <- stats::setNames(as.list(meta$simulate), meta$names)
  for (p in intersect(names(supplied), meta$names)) values[p] <- supplied[p]
  for (p in meta$params) {
    v <- values[[p$name]]
    if (!is.numeric(v) || length(v) != 1L || is.na(v))
      stop("`", name, "`: held `", p$name, "` must be in ", p$domain, call. = FALSE)
  }
  env <- c(values, list(inf = Inf))
  for (p in meta$params) {
    v <- values[[p$name]]
    domain <- p$domain
    ends <- strsplit(substr(domain, 2, nchar(domain)-1), ",", fixed = TRUE)[[1]]
    bounds <- vapply(ends, function(e) eval(parse(text = e), env), numeric(1))
    valid <- (if (substr(domain, 1, 1) == "[") v >= bounds[1] else v > bounds[1]) &&
      (if (substr(domain, nchar(domain), nchar(domain)) == "]") v <= bounds[2] else v < bounds[2])
    if (!isTRUE(valid)) stop("`", name, "`: held `", p$name, "` must be in ", domain, call. = FALSE)
  }
  obj <- univariateML_construct(unlist(values), name,
    list(n = length(x), call = deparse(call)), fixed = meta$names)
  # A median evaluation also checks joint parameter constraints and lattice
  # parameters in the reference evaluator, without imposing fit_domain.
  q <- suppressWarnings(qml(0.5, obj))
  if (length(q) != 1L || is.na(q)) stop("`", name, "`: invalid held coefficients", call. = FALSE)
  if (frozen) {
    attr(obj, "frozen") <- TRUE
  } else {
    id <- substring(name, 3L)
    density <- get(paste0(if (startsWith(meta$density, "stats::")) "core_", "d", id), mode = "function")
    ld <- do.call(density, c(list(x), values, list(log = TRUE)))
    if (anyNA(ld) || any(ld == -Inf))
      stop("`", name, "`: observations incompatible with held coefficients", call. = FALSE)
    attr(obj, "logLik") <- sum(ld)
  }
  obj
}
