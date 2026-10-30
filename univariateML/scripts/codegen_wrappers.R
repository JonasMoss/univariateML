# Generate package/R/ml02_wrappers.R: one exported ml<id>() wrapper per
# distribution, with roxygen docs. Run from univariateML/ via `just gen`,
# after Rcpp::compileAttributes() (the adapters' formals are read from
# package/R/RcppExports.R to check the documented control arguments).
#
# Docs come from each distribution's README.md (front matter + prose).

dist_dir <- Sys.getenv(
  "UMVL_DOSSIER_DIR",
  "../cppunivariate/include/cppunivariate/distributions"
)
pkg_r_dir <- Sys.getenv("UMVL_PKG_R_DIR", "package/R")

source(file.path("scripts", "load_distributions.R"))
models <- load_distributions(dist_dir)

# Adapter formals, for the control-argument check.
rcpp_env <- new.env()
rcpp_exports <- file.path(pkg_r_dir, "RcppExports.R")
if (!file.exists(rcpp_exports)) {
  stop("codegen_wrappers: run Rcpp::compileAttributes() first (", rcpp_exports, " missing)")
}
sys.source(rcpp_exports, envir = rcpp_env)

nl <- function(...) paste0(..., collapse = "")

# Markdown math in prose -> Rd math macros (roxygen markdown leaves Rd
# macros alone).
rd_math <- function(x) {
  x <- gsub("\\$\\$([^$]+)\\$\\$", "\\\\deqn{\\1}", x)
  gsub("\\$([^$]+)\\$", "\\\\eqn{\\1}", x)
}

# "pkg::fn" -> roxygen link "[pkg::fn()]"; bare names -> "[fn()]".
rd_link <- function(ref) paste0("[", ref, "()]")

roxy <- function(lines) paste0("#' ", lines)

param_names <- function(model) vapply(model$params, function(p) p$name, character(1))

and_list <- function(x) {
  x <- paste0("`", x, "`")
  if (length(x) <= 1L) return(x)
  paste0(paste(x[-length(x)], collapse = ", "), " and ", x[length(x)])
}

limit_sentence <- function(lim, model) {
  # lim$model: id of a package model to point users to (D1: "use mlX");
  # without it, lim$name describes a limit the package does not fit.
  has_target <- !is.null(lim$model)
  target_name <- if (has_target && !is.null(models[[lim$model]])) models[[lim$model]]$name
                 else lim$name %||% lim$model
  # lim$fixed: the limit applies only when these parameters are held fixed.
  held <- unlist(lim$fixed)
  nl(
    if (length(held)) nl("With ", and_list(held), " held fixed: w") else "W",
    "hen ", if (!is.null(lim$detect)) nl("`", lim$detect, "`") else "the data point to this limit",
    ", the maximum likelihood estimator does not exist: the likelihood ",
    "increases toward the ", target_name, " distribution",
    if (!is.null(lim$when)) nl(" as ", rd_math(lim$when)) else "",
    ". `ml", model$id, "()` then stops with an error",
    if (has_target) nl("; use [ml", lim$model, "()] instead.") else "."
  )
}

# Parameters the user may hold fixed: an adapter argument named like the
# coefficient; these names are derived from the exact adapter formals.
holdable_names <- function(model) {
  intersect(param_names(model), names(formals(rcpp_env[[paste0("ml", model$id, "_")]])))
}

# Design quantities and thresholds the user must supply (derived from kind):
# adapter arguments without a default, always held fixed.
required_names <- function(model) {
  unlist(lapply(model$params, function(p) if (isTRUE(p$required)) p$name))
}

check_control <- function(model) {
  fn <- rcpp_env[[paste0("ml", model$id, "_")]]
  if (is.null(fn)) stop("codegen_wrappers: no Rcpp adapter ml", model$id, "_")
  formal_names <- setdiff(names(formals(fn)), "x")
  doc_names <- union(names(model$control), holdable_names(model))
  if (!setequal(formal_names, doc_names)) {
    stop("codegen_wrappers: ", model$id, ": adapter arguments (",
         paste(formal_names, collapse = ", "), ") do not match README r.control (",
         paste(doc_names, collapse = ", "), ")")
  }
  for (p in setdiff(param_names(model), formal_names)) {
    declared <- any(vapply(model$holds, function(h)
      identical(h$status, "not-implemented") && p %in% unlist(h$held), logical(1)))
    if (!declared) stop("codegen_wrappers: ", model$id, ": coefficient `", p,
                       "` is not an adapter argument; add a holds: not-implemented entry")
  }
  for (p in intersect(param_names(model), formal_names)) {
    missing_default <- identical(formals(fn)[[p]], quote(expr=))
    if (p %in% required_names(model)) {
      if (!missing_default) stop("codegen_wrappers: ", model$id, ": required coefficient `", p, "` must have no adapter default")
    } else if (missing_default || !is.null(formals(fn)[[p]])) {
      stop("codegen_wrappers: ", model$id, ": non-required coefficient `", p, "` must default to NULL")
    }
  }
  formals(fn)
}

# Stop with a clear message when a required argument is missing.
required_checks <- function(model) {
  fname <- paste0("ml", model$id)
  req <- Filter(function(p) isTRUE(p$required), model$params)
  if (!length(req)) return(character())
  checks <- c(nl("  supplied <- match.call(", fname, "_, call = as.call(c(\n",
       "    list(quote(", fname, "_), x = quote(x)), list(...))))\n"),
    vapply(req, function(p) nl(
    "  if (!\"", p$name, "\" %in% names(supplied)) {\n",
    "    stop(", encodeString(paste0("`", fname, "` requires `", p$name,
      "` (", p$kind, "). Hold a value, e.g. `", p$name, " = ", p$simulate, "`, or use `", p$name, " = NULL` to estimate it where supported.",
      if (!is.null(p$advice)) paste0(" ", p$advice) else ""),
      quote = '"'), ", call. = FALSE)\n",
    "  }\n"
  ), character(1)))
  # Use the matched names so supported positional adapter calls keep working.
  checks <- c(checks, "  dots <- as.list(supplied)[-1L]\n")
  # An exact held-set structural exception can be diagnosed before conversion.
  exceptions <- Filter(function(h) !length(setdiff(unlist(h$held), holdable_names(model))), model$holds)
  for (h in exceptions) {
    held <- unlist(h$held)
    conditions <- vapply(holdable_names(model), function(p) {
      if (p %in% held) nl("!is.null(dots[[\"",p,"\"]])") else nl("is.null(dots[[\"",p,"\"]])")
    }, "")
    checks <- c(checks, nl("  if (", paste(conditions,collapse=" && "), ") stop(",
      encodeString(h$reason, quote='"'), ", call. = FALSE)\n"))
  }
  checks
}

# " with `mean = mu`, `sd = sigma`" for models with an r.args map.
args_clause <- function(model) {
  if (is.null(model$args)) return("")
  nl(" with ", and_list(paste(names(model$args), "=", model$args)))
}

gen_doc <- function(model) {
  fname <- paste0("ml", model$id)
  defaults <- check_control(model)
  s <- model$sections
  desc <- rd_math(s$description)
  if (!length(desc)) stop("codegen_wrappers: ", model$id, ": README.md has no description")

  details <- c(
    if (startsWith(model$density, "stats::"))
      nl("For the density function of the ", model$name, " distribution see ",
         rd_link(model$density), args_clause(model), ".") else
      "Evaluate the fitted distribution with [dml()], [pml()], [qml()] and [rml()].",
    rd_math(s[["d/p/q/r"]] %||% character()),
    vapply(model$limits, limit_sentence, character(1), model = model),
    rd_math(s$details %||% character())
  )
  control_items <- vapply(names(model$control), function(arg) {
    default_txt <- if (arg %in% required_names(model)) {
      " Required; there is no default."
    } else {
      d <- defaults[[arg]]
      if (is.null(d)) "" else nl(" Defaults to `", format(eval(d)), "`.")
    }
    nl("  * `", arg, "`: ", rd_math(model$control[[arg]]), default_txt)
  }, character(1))
  fixed_items <- vapply(setdiff(holdable_names(model), names(model$control)), function(arg) {
    nl("  * `", arg, "`: Value at which to hold `", arg, "` fixed; ",
       "`NULL` (the default) estimates it.")
  }, character(1))
  control_items <- c(fixed_items, control_items)
  seealso <- unique(c(
    if (startsWith(model$density, "stats::")) nl(rd_link(model$density), " for the density.") else "[dml()] for density evaluation.",
    unlist(lapply(model$limits, function(l) if (!is.null(l$model)) nl("[ml", l$model, "()] for the limit model."))),
    rd_math(s[["see also"]] %||% character())
  ))
  examples <- if (!is.null(model$examples)) {
    strsplit(sub("\n$", "", model$examples), "\n", fixed = TRUE)[[1]]
  } else {
    nl("set.seed(1); ", fname, "(simulate_default(\"", fname, "\", 100)",
       if (length(model$fit_args)) paste0(", ", paste(vapply(names(model$fit_args), function(p)
         paste0(p, " = ", paste(deparse(model$fit_args[[p]]), collapse=" ")), ""), collapse=", ")),
       ")")
  }

  block <- c(
    nl(model$name, " distribution maximum likelihood estimation"),
    "",
    desc[1],
    if (length(desc) > 1L) c(rbind("", desc[-1])) else NULL,
    "",
    "@details",
    head(c(rbind(details, "")), -1L),
    "",
    "@param x a (non-empty) numeric vector of data values.",
    "@param na.rm logical. Should missing values be removed?",
    if (length(control_items)) c("@param ... Optional control arguments:", control_items)
    else "@param ... Not used.",
    nl("@return `", fname, "` returns an object of [class][base::class] `univariateML`. ",
       "This is a named numeric vector with maximum likelihood estimates for ",
       and_list(param_names(model)), " and the attributes `model`, `density`, ",
       "`logLik`, `support`, `n`, `call` and `fixed` (the names of the parameters ",
       "held fixed rather than estimated), plus `restricted` when supplied by an adapter."),
    "@seealso",
    seealso,
    if (length(model$references)) c("@references", head(c(rbind(model$references, "")), -1L)) else NULL,
    "@examples",
    examples,
    "@export"
  )
  paste0(roxy(block), collapse = "\n")
}

gen_wrapper <- function(model) {
  fname <- paste0("ml", model$id)
  doc <- gen_doc(model)
  paste0(
    doc, "\n",
    fname, " <- function(x, na.rm = FALSE, ...) {\n",
    "  ml_check_args(", fname, "_, list(...), \"", fname, "\")\n",
    nl(required_checks(model)),
    "  x <- ml_check_modify(x, na.rm = na.rm, name = \"", fname, "\")\n",
    "  n <- length(x)\n",
    "  out <- ", fname, "_(x = x, ...)\n",
    "  params <- list(logLik = unname(out$logLik), call = deparse(match.call()), n = n)\n",
    "  univariateML_construct(out$estimates, name = \"", fname, "\", params = params, fixed = out$fixed, restricted = out$restricted)\n",
    "}\n\n"
  )
}

header <- paste(
  "# This file is auto-generated by scripts/codegen_wrappers.R",
  "# Do not edit by hand.",
  sep = "\n"
)

wrappers <- vapply(models, gen_wrapper, character(1))

dir.create(pkg_r_dir, recursive = TRUE, showWarnings = FALSE)
cat(header, "\n\n", paste0(wrappers, collapse = ""),
    file = file.path(pkg_r_dir, "ml02_wrappers.R"), sep = "")
