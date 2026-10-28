# Generate package/R/ml02_wrappers.R: one exported ml<id>() wrapper per
# distribution, with roxygen docs. Run from univariateML/ via `just gen`,
# after Rcpp::compileAttributes() (the adapters' formals are read from
# package/R/RcppExports.R to check the documented control arguments).
#
# Migrated distributions (distributions/<id>/README.md) get full docs from
# the front matter and prose; legacy ones get placeholder docs until they
# are migrated.

dist_dir <- Sys.getenv(
  "UMVL_DOSSIER_DIR",
  "../cppunivariate/include/cppunivariate/distributions"
)
r_metadata_dir <- Sys.getenv("UMVL_R_METADATA_DIR", "metadata")
pkg_r_dir <- Sys.getenv("UMVL_PKG_R_DIR", "package/R")

source(file.path("scripts", "load_distributions.R"))
models <- load_distributions(dist_dir, r_metadata_dir)

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
  nl(
    "When ", if (!is.null(lim$detect)) nl("`", lim$detect, "`") else "the data point to this limit",
    ", the maximum likelihood estimator does not exist: the likelihood ",
    "increases toward the ", target_name, " distribution",
    if (!is.null(lim$when)) nl(" as ", rd_math(lim$when)) else "",
    ". `ml", model$id, "()` then stops with an error",
    if (has_target) nl("; use [ml", lim$model, "()] instead.") else "."
  )
}

check_control <- function(model) {
  fn <- rcpp_env[[paste0("ml", model$id, "_")]]
  if (is.null(fn)) stop("codegen_wrappers: no Rcpp adapter ml", model$id, "_")
  formal_names <- setdiff(names(formals(fn)), "x")
  doc_names <- names(model$control)
  if (!setequal(formal_names, doc_names)) {
    stop("codegen_wrappers: ", model$id, ": adapter arguments (",
         paste(formal_names, collapse = ", "), ") do not match README r.control (",
         paste(doc_names, collapse = ", "), ")")
  }
  formals(fn)
}

gen_doc_readme <- function(model) {
  fname <- paste0("ml", model$id)
  defaults <- check_control(model)
  s <- model$sections
  desc <- rd_math(s$description)
  if (!length(desc)) stop("codegen_wrappers: ", model$id, ": README.md has no description")

  details <- c(
    nl("For the density function of the ", model$name, " distribution see ",
       rd_link(model$density), "."),
    vapply(model$limits, limit_sentence, character(1), model = model),
    rd_math(s$details %||% character())
  )
  control_items <- vapply(names(model$control), function(arg) {
    d <- defaults[[arg]]
    default_txt <- if (is.null(d)) "" else nl(" Defaults to `", format(eval(d)), "`.")
    nl("  * `", arg, "`: ", rd_math(model$control[[arg]]), default_txt)
  }, character(1))
  seealso <- unique(c(
    nl(rd_link(model$density), " for the density."),
    unlist(lapply(model$limits, function(l) if (!is.null(l$model)) nl("[ml", l$model, "()] for the limit model."))),
    rd_math(s[["see also"]] %||% character())
  ))
  examples <- if (!is.null(model$examples)) {
    strsplit(sub("\n$", "", model$examples), "\n", fixed = TRUE)[[1]]
  } else {
    nl("set.seed(1); ", fname, "(simulate_default(\"", fname, "\", 100))")
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
       "`logLik`, `support`, `n` and `call`."),
    "@seealso",
    seealso,
    if (length(model$references)) c("@references", head(c(rbind(model$references, "")), -1L)) else NULL,
    "@examples",
    examples,
    "@export"
  )
  paste0(roxy(block), collapse = "\n")
}

gen_doc_legacy <- function(model) {
  lower <- if (is.null(model$support$lower)) "-Inf" else model$support$lower
  upper <- if (is.null(model$support$upper)) "Inf" else model$support$upper
  block <- c(
    nl(model$name, " maximum likelihood estimation"),
    "",
    "Maximum likelihood estimation for this distribution.",
    "",
    nl("Density: ", model$density, "; support: [", lower, ", ", upper, ")."),
    "",
    "@param x a (non-empty) numeric vector of data values.",
    "@param na.rm logical. Should missing values be removed?",
    "@param ... Optional control arguments passed to the optimizer/estimator.",
    "@return An object of class `univariateML` with estimates and attributes.",
    "@seealso",
    nl("  ", model$density),
    "@examples",
    nl("set.seed(1); ml", model$id, "(simulate_default(\"ml", model$id, "\", 100))"),
    "@export"
  )
  paste0(roxy(block), collapse = "\n")
}

gen_wrapper <- function(model) {
  fname <- paste0("ml", model$id)
  doc <- if (identical(model$source, "readme")) gen_doc_readme(model) else gen_doc_legacy(model)
  paste0(
    doc, "\n",
    fname, " <- function(x, na.rm = FALSE, ...) {\n",
    "  x <- ml_check_modify(x, na.rm = na.rm, name = \"", fname, "\")\n",
    "  n <- length(x)\n",
    "  out <- ", fname, "_(x = x, ...)\n",
    "  params <- list(logLik = unname(out$logLik), call = deparse(match.call()), n = n)\n",
    "  univariateML_construct(out$estimates, name = \"", fname, "\", params = params)\n",
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
