# Generate package/R/ml02_wrappers.R: one exported ml<id>() wrapper per
# distribution, with roxygen docs. Run from bindings/r/ via `just gen`,
# after Rcpp::compileAttributes() (the adapters' formals are read from
# package/R/RcppExports.R to check the documented control arguments).
#
# Docs come from each distribution's README.md (front matter + prose).

dist_dir <- Sys.getenv(
  "UMVL_DOSSIER_DIR",
  "../../univariateML-cpp/include/cppunivariate/distributions"
)
pkg_r_dir <- Sys.getenv("UMVL_PKG_R_DIR", "../../univariateML-r/R")

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

# roxygen treats a backtick span beginning with "r " as inline R. Emit
# an Rd code macro for those literal spans, wherever dossier prose uses them.
roxy <- function(lines) {
  lines <- gsub("`(r[[:space:]][^`]*)`", "\\\\code{\\1}", lines)
  paste0("#' ", lines)
}

param_names <- function(model) vapply(model$params, function(p) p$name, character(1))

and_list <- function(x) {
  x <- paste0("`", x, "`")
  if (length(x) <= 1L) return(x)
  paste0(paste(x[-length(x)], collapse = ", "), " and ", x[length(x)])
}

# Dossiers contain both legacy clauses and complete site-facing sentences.
# Preserve math/code case, and never append punctuation to existing punctuation.
rd_sentence <- function(x) {
  if (is.null(x) || !nzchar(trimws(x))) return(character())
  x <- trimws(x)
  substr(x, 1L, 1L) <- toupper(substr(x, 1L, 1L))
  if (!grepl("[.!?]$", x)) x <- paste0(x, ".")
  # Legacy prose contains literal sets such as {0,1}. Protect those with
  # inline code, without touching braces inside math or existing code spans.
  spans <- gregexpr("\\$[^$]*\\$|`[^`]*`|\\{[^{}]*\\}", x, perl = TRUE)
  regmatches(x, spans) <- lapply(regmatches(x, spans), function(parts)
    ifelse(startsWith(parts, "{"), paste0("`", parts, "`"), parts))
  rd_math(x)
}

limit_sentence <- function(lim, model) {
  has_target <- !is.null(lim$model)
  target_name <- if (has_target && !is.null(models[[lim$model]])) models[[lim$model]]$name
                 else lim$name %||% lim$model
  held <- unlist(lim$fixed)
  scope <- if (length(held)) nl("This limit applies with ", and_list(held), " held fixed. ") else ""
  configured_call <- NULL
  if(has_target && !is.null(lim$configuration)) {
    setting<-lim$configuration
    argument<-if(!is.null(setting$zero))paste0('zero = "',setting$zero,'"') else
      paste0("truncate = c(",paste(unlist(setting$truncate),collapse=", "),")")
    configured_call<-paste0("ml",lim$model,"(x, ",argument,")")
  }
  stop_text <- nl("`ml", model$id, "()` stops with an error",
    if(!is.null(configured_call))nl("; use `",configured_call,"` to fit the limit model; see [ml",lim$model,"()].") else
    if (has_target) nl("; use [ml", lim$model, "()] to fit the limit model.") else ".")
  lead <- if (identical(lim$kind, "nonexistence")) {
    "The likelihood is unbounded and no finite maximum likelihood estimator exists. "
  } else if (isTRUE(lim$numerical)) {
    nl("The numerical search does not establish a finite maximum above its tolerance",
       if (!is.null(target_name)) nl(" relative to the limit (", target_name, ")") else "", ". ")
  } else {
    nl("The maximum likelihood estimator does not exist: the likelihood ",
       if (has_target) nl("increases toward the ", target_name, " distribution")
       else nl("approaches its supremum only in a limit (", target_name, ")"), ". ")
  }
  paste(c(nl(scope, lead, stop_text), rd_sentence(lim$when),
          rd_sentence(lim$detect)), collapse = " ")
}

# "Notation: \eqn{\alpha} is `alpha`, ..." so prose and limit sentences that
# use the README symbols can be read against the argument names.
notation_sentence <- function(model) {
  syms <- Filter(function(p) !is.null(p$symbol), model$params)
  if (!length(syms)) return(character())
  nl("Notation: ", paste(vapply(syms, function(p)
    nl(rd_math(nl("$", p$symbol, "$")), " is `", p$name, "`"), ""), collapse = ", "), ".")
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
    "  supplied_values <- list(...)\n",
    '  names(supplied_values) <- setdiff(names(supplied), c("", "x"))\n',
    "  supplied_holds <- names(Filter(function(v) !is.null(v), supplied_values))\n",
    vapply(req, function(p) {
      # With this coefficient free, the remaining actual holds determine
      # whether a structural exception prevents offering NULL.
      exceptions <- Filter(function(h) !p$name %in% unlist(h$held), model$holds)
      blocked <- vapply(exceptions, function(h) {
        held <- unlist(h$held)
        other <- setdiff(holdable_names(model), p$name)
        conditions <- vapply(other, function(q) {
          nl(if (!q %in% held) "!" else "", "\"", q, "\" %in% supplied_holds")
        }, "")
        if (length(conditions)) paste(conditions, collapse=" && ") else "TRUE"
      }, "")
      offer <- paste0(", or use `", p$name, " = NULL` to estimate it where supported")
      message <- paste0("`", fname, "` requires `", p$name,
        "` (", p$kind, "). Hold a value, e.g. `", p$name, " = ", p$simulate, "`")
      nl("  if (!\"", p$name, "\" %in% names(supplied)) {\n",
         "    stop(paste0(", encodeString(message, quote='"'), ", ",
         if (length(blocked)) nl("if (", paste(paste0("(",blocked,")"),collapse=" || "),
           ") \"\" else ") else "",
         encodeString(offer,quote='"'), ", ",
         encodeString(paste0(".", if (!is.null(p$advice)) paste0(" ",p$advice) else ""),quote='"'),
         "), call. = FALSE)\n  }\n")
    }, character(1)))
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
    nl("With `x` omitted, builds a fixed model without data. Supply coefficients by name; unspecified coefficients default to ",
       paste(vapply(model$params, function(p) paste0("`", p$name, " = ", p$simulate, "`"), ""), collapse = ", "),
       ". Design and threshold coefficients also use these defaults. Domains apply without fitting restrictions. Evaluate or simulate with [dml()], [pml()], [qml()] and [rml()], or draw its density with [graphics::plot()]. Methods requiring data stop. With data and every coefficient held, the log likelihood is evaluated without estimation (zero degrees of freedom)."),
    notation_sentence(model),
    rd_math(s[["d/p/q/r"]] %||% character()),
    vapply(model$limits, limit_sentence, character(1), model = model),
    rd_math(c(model$existence$statement, model$existence$reason)),
    rd_math(s$details %||% character()),
    rd_math(s[["r notes"]] %||% character())
  )
  control_items <- vapply(names(model$control), function(arg) {
    default_txt <- if (arg %in% required_names(model)) {
      " Required with data; without data uses the fixed-model default above."
    } else {
      d <- defaults[[arg]]
      if (is.null(d)) "" else nl(" Defaults to `", format(eval(d)), "`.")
    }
    nl("  * `", arg, "`: ", rd_math(model$control[[arg]]), default_txt)
  }, character(1))
  fixed_items <- vapply(setdiff(holdable_names(model), names(model$control)), function(arg) {
    nl("  * `", arg, "`: Value at which to hold `", arg, "` fixed; ",
       "`NULL` estimates it with data; without data uses the fixed-model default above.")
  }, character(1))
  control_items <- c(fixed_items, control_items,
    if(model$modifier_eligibility$zero) '  * `zero`: One of "none", "truncated", "modified", "inflated".',
    '  * `truncate`: Closed retained window c(lower, upper); NA estimates a bound.')
  seealso <- unique(c(
    if (startsWith(model$density, "stats::")) nl(rd_link(model$density), " for the density.") else "[dml()] for density evaluation.",
    unlist(lapply(model$limits, function(l) if (!is.null(l$model)) nl("[ml", l$model, "()] for the limit model."))),
    rd_math(s[["see also"]] %||% character())
  ))
  if (is.null(model$examples)) stop("codegen_wrappers: ", model$id, ": missing examples:")
  examples <- strsplit(sub("\n$", "", model$examples), "\n", fixed = TRUE)[[1]]

  block <- c(
    nl(model$name, " distribution maximum likelihood estimation"),
    "",
    desc[1],
    if (length(desc) > 1L) c(rbind("", desc[-1])) else NULL,
    "",
    "@details",
    head(c(rbind(details, "")), -1L),
    "",
    "@param x a (non-empty) numeric vector of data values. Omit to build a fixed model without data.",
    "@param na.rm logical. Should missing values be removed?",
    if (length(control_items)) c("@param ... Optional control arguments:", control_items)
    else "@param ... Not used.",
    nl("@return `", fname, "` returns an object of [class][base::class] `univariateML`. ",
       "This is a named numeric vector with coefficients (maximum likelihood estimates when data are supplied) for ",
       and_list(param_names(model)), " and the attributes `model`, `density`, ",
       "`logLik`, `support`, `n`, `call` and `fixed` (the names of the parameters ",
       "held fixed rather than estimated), plus `restricted` when supplied by an adapter and `frozen = TRUE` for models built without data."),
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
    "  modifier_args <- list(...)\n",
    "  if (", if(model$modifier_eligibility$zero) '\"zero\" %in% names(modifier_args) || ' else '',
    '\"truncate\" %in% names(modifier_args) || (!missing(x) && ((is.matrix(x) && NCOL(x)==2L) || inherits(x, \"Surv\")))) {\n',
    "    zero <- ", if(model$modifier_eligibility$zero) 'if (\"zero\" %in% names(modifier_args)) modifier_args$zero else \"none\"' else '\"none\"', "\n",
    "    truncate <- modifier_args$truncate; modifier_args$truncate <- NULL\n",
    if(model$modifier_eligibility$zero) "    modifier_args$zero <- NULL\n" else "",
    "    return(ml_modified_call(\"", fname, "\", if(missing(x)) NULL else x, missing(x), na.rm, modifier_args, zero, truncate, match.call()))\n  }\n",
    "  ml_check_args(", fname, "_, list(...), \"", fname, "\")\n",
    "  if (missing(x)) {\n",
    "    ml_check_frozen_call(sys.call())\n",
    "    return(ml_fixed_model(\"", fname, "\", list(...), match.call(), frozen = TRUE))\n  }\n",
    "  supplied <- match.call(", fname, "_, call = as.call(c(list(quote(", fname, "_), x = quote(x)), list(...))))\n",
    "  held <- as.list(supplied)[-1L]; held$x <- NULL\n",
    "  if (all(vapply(univariateML_metadata[[\"", fname, "\"]]$names, function(p) !is.null(held[[p]]), logical(1)))) {\n",
    "    return(ml_fixed_model(\"", fname, "\", held, match.call(), x = ml_check_modify(x, na.rm, \"", fname, "\")))\n  }\n",
    nl(required_checks(model)),
    "  x <- ml_check_modify(x, na.rm = na.rm, name = \"", fname, "\")\n",
    "  n <- length(x)\n",
    "  out <- ", fname, "_(x = x, ...)\n",
    "  if (inherits(out, \"univariateML_stop\")) stop(out)\n",
    "  params <- list(logLik = unname(out$logLik), call = deparse(match.call()), n = n)\n",
    "  univariateML_construct(out$estimates, name = \"", fname, "\", params = params, fixed = out$fixed, restricted = out$restricted, data = x)\n",
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
