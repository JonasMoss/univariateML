# Load per-distribution metadata for the R-side codegen and goldens from
# distributions/<id>/README.md (YAML front matter + prose; schema:
# distributions/README_SCHEMA.md). Returns a list of models sorted by id,
# each with: id, name, support, params, density, sampler, args, fit_args,
# control, limits, references, examples, sections.

if (!requireNamespace("yaml", quietly = TRUE)) {
  stop("the yaml package is required to read distributions/<id>/README.md")
}

`%||%` <- function(a, b) if (is.null(a)) b else a
.umvl_as_bound <- function(v) {
  if (is.numeric(v)) return(v)
  s <- tolower(trimws(as.character(v)))
  if (s %in% c("inf", ".inf", "+inf")) return(Inf)
  if (s %in% c("-inf", "-.inf")) return(-Inf)
  out <- suppressWarnings(as.numeric(s))
  if (is.na(out)) stop("load_distributions: cannot parse bound '", v, "'")
  out
}

# Support expressions are R expressions in coefficients, with a small pure
# arithmetic vocabulary. Keep numeric envelopes for pre-fit input checks.
.umvl_support_expr <- function(v, pn, endpoint = FALSE) {
  if (is.numeric(v)) return(v)
  numeric <- suppressWarnings(tryCatch(.umvl_as_bound(v), error = function(e) NA_real_))
  if (!is.na(numeric)) return(numeric)
  parsed <- tryCatch(parse(text = v), error = function(e) NULL)
  allowed <- c("+", "-", "*", "/", "^", "(", "if", "<", ">", "<=", ">=", "==",
               "!=", "&&", "||", "min", "max", "floor", "ceiling", "log", "sqrt",
               "lgamma", "lbeta", "dnorm", "dt", "exp", "expit")
  calls <- function(x) {
    if (!is.call(x)) return(character())
    c(as.character(x[[1]]), unlist(lapply(as.list(x)[-1], calls)))
  }
  if (is.null(parsed) || length(parsed) != 1L ||
      length(setdiff(all.vars(parsed), c(pn, "Inf", "pi", if(endpoint) "x"))) ||
      length(setdiff(calls(parsed[[1]]), allowed)))
    stop("load_distributions: invalid support expression '", v, "'")
  as.character(v)
}

# Claims are explicit and never inferred from DQM. Legacy/minimal metadata
# may omit this block while the inventory is filled.
.umvl_validate_numeric_inference <- function(scopes, id) {
  scalar_text <- function(x) is.character(x) && length(x) == 1L && !is.na(x) && nzchar(x)
  if (!is.null(scopes)) {
    if (!is.list(scopes) || !length(scopes))
      stop("load_distributions: invalid numeric inference scopes for ", id)
    names_seen <- character()
    for (scope in scopes) {
      if (!is.list(scope) || !scalar_text(scope$regime) ||
          !is.logical(scope$available) || length(scope$available) != 1L ||
          is.na(scope$available) || !scalar_text(scope$condition) ||
          !scalar_text(scope$classification) ||
          !grepl("^[^#]+#[^#]+$", scope$classification) ||
          (!scope$available && !scalar_text(scope$reason)) ||
          (!is.null(scope$reason) && !scalar_text(scope$reason)) ||
          length(setdiff(names(scope), c("regime", "available", "condition", "reason", "classification"))))
        stop("load_distributions: invalid numeric inference scope for ", id)
      names_seen <- c(names_seen, scope$regime)
    }
    if (anyDuplicated(names_seen))
      stop("load_distributions: duplicate numeric inference regime for ", id)
  }
  invisible(NULL)
}

# Split "---\n<yaml>\n---\n<body>" into list(front = <list>, body = <chr>).
.umvl_read_front_matter <- function(path) {
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  marks <- which(trimws(lines) == "---")
  if (length(marks) < 2L || marks[1] != 1L) {
    stop("load_distributions: ", path, " must start with YAML front matter (---)")
  }
  front <- yaml::yaml.load(paste(lines[2:(marks[2] - 1L)], collapse = "\n"))
  body <- if (marks[2] < length(lines)) lines[(marks[2] + 1L):length(lines)] else character()
  list(front = front, body = body)
}

# Body prose before the first "## " heading is the description; each
# "## Heading" section becomes a named entry (lower-case heading).
.umvl_body_sections <- function(body) {
  heads <- grep("^## ", body)
  pre <- if (length(heads)) body[seq_len(heads[1] - 1L)] else body
  sections <- list(description = .umvl_paragraphs(pre))
  for (i in seq_along(heads)) {
    start <- heads[i] + 1L
    end <- if (i < length(heads)) heads[i + 1L] - 1L else length(body)
    name <- tolower(trimws(sub("^## ", "", body[heads[i]])))
    chunk <- if (start <= end) body[start:end] else character()
    sections[[name]] <- .umvl_paragraphs(chunk)
  }
  sections
}

.umvl_paragraphs <- function(lines) {
  if (!length(lines)) return(character())
  txt <- paste(lines, collapse = "\n")
  paras <- trimws(strsplit(txt, "\n[[:space:]]*\n")[[1]])
  paras <- paras[nzchar(paras)]
  gsub("[[:space:]]*\n[[:space:]]*", " ", paras)
}

.umvl_from_readme <- function(path, id) {
  fm <- .umvl_read_front_matter(path)
  f <- fm$front
  if (!identical(f$id, id)) {
    stop("load_distributions: ", path, " has id '", f$id, "', folder is '", id, "'")
  }
  required <- c("id", "name", "support", "params", "r")
  missing <- setdiff(required, names(f))
  if (length(missing)) {
    stop("load_distributions: ", path, " lacks ", paste(missing, collapse = ", "))
  }
  scalar_text <- function(x) is.character(x) && length(x) == 1L && !is.na(x) && nzchar(x)
  for (key in c("summary", "classification_reason"))
    if (!is.null(f[[key]]) && !scalar_text(f[[key]])) stop("load_distributions: invalid ", key, " for ", id)
  if (!is.null(f$classification) &&
      (!scalar_text(f$classification) || !f$classification %in% c("classical", "specialist", "uncommon") ||
       !scalar_text(f$classification_reason))) stop("load_distributions: invalid classification for ", id)
  .umvl_validate_numeric_inference(f$inference_numeric, id)
  if (!is.null(f$aliases) && length(f$aliases) && !all(vapply(f$aliases, scalar_text, TRUE)))
    stop("load_distributions: invalid aliases for ", id)
  if (length(f$references) && !all(vapply(f$references, scalar_text, TRUE)))
    stop("load_distributions: references must remain strings for ", id)
  keys <- unlist(f$key_references)
  if (length(keys) && (length(keys) > 3L || !is.numeric(keys) || anyNA(keys) ||
      any(keys != floor(keys) | keys < 1 | keys > length(f$references)) || anyDuplicated(keys)))
    stop("load_distributions: invalid key_references for ", id)
  for (lim in f$limits %||% list()) {
    if (!is.null(lim$firmness) && (!scalar_text(lim$firmness) ||
        !lim$firmness %in% c("proved", "argued", "numerical", "conjecture")))
      stop("load_distributions: invalid limit firmness for ", id)
    if (!is.null(lim$numerical) && (!is.logical(lim$numerical) || length(lim$numerical) != 1L || is.na(lim$numerical)))
      stop("load_distributions: invalid numerical detection flag for ", id)
  }
  # Minimal code-generation clients may omit the entire presentation block.
  taxonomy_keys <- c("group", "construction", "relations", "fit", "areas")
  if (any(taxonomy_keys %in% names(f))) {
    if (is.null(f$inference_numeric))
      stop("load_distributions: missing numeric inference scopes for ", id)
    required_taxonomy <- if (identical(f$stage, "dpqr")) setdiff(taxonomy_keys, "fit") else taxonomy_keys
    absent <- setdiff(c(required_taxonomy, "classification", "classification_reason"), names(f))
    if (length(absent)) stop("load_distributions: incomplete taxonomy for ", id, ": ", paste(absent, collapse=", "))
    # Taxonomy is presentation metadata; it does not enter generated R help.
    vocab <- list(
      group = c("Pareto and Lomax", "Poisson, binomial and negative binomial", "beta and generalised beta", "bounded continuous", "discrete heavy-tailed", "extreme value", "finite sampling", "gamma and Weibull", "modified counts", "normal and Student", "normal convolutions", "other positive", "skewed and robust location-scale"),
      construction = c("base", "compounding", "discretisation", "folding", "generalisation", "location-scale", "modification", "skewing", "transformation", "truncation"),
      areas = c("actuarial", "counts and ecology", "economics and income", "extremes and environment", "finance", "measurement and errors", "proportions", "rank-frequency", "reliability and survival", "robust modelling", "sampling and quality control", "waiting times and queues"))
    for (key in names(vocab)) {
      values <- unlist(f[[key]])
      if (!is.character(values) || !length(values) || anyNA(values) ||
          anyDuplicated(values) || length(setdiff(values, vocab[[key]])) ||
          (key == "group" && length(values) != 1L) || (key == "areas" && length(values) > 3L))
        stop("load_distributions: invalid taxonomy ", key, " for ", id)
    }
    if ((!identical(f$stage, "dpqr") || !is.null(f$fit)) &&
        (!scalar_text(f$fit$method) || !f$fit$method %in%
        c("closed-form", "scalar-solve", "profile-search", "multistart", "integer-search", "delegation", "convex-solve") ||
        !scalar_text(f$fit$certified) || !f$fit$certified %in% c("yes", "partial", "no", "unknown") ||
        !scalar_text(f$fit$note))) stop("load_distributions: invalid fit taxonomy for ", id)
    if (is.null(f$relations) || length(setdiff(names(f$relations), c("special_case_of", "transform_of", "truncation_of", "modification_of", "convolution_of", "mixture_of", "discretisation_of", "closure_point"))))
      stop("load_distributions: invalid relations for ", id)
    for (kind in names(f$relations)) for (edge in f$relations[[kind]]) {
      if (kind == "closure_point") {
        if (!all(vapply(edge[c("name", "path", "reason")], scalar_text, FALSE)) ||
            !identical(edge$reachability, "never"))
          stop("load_distributions: invalid closure point for ", id)
        next
      }
      target <- file.path(dirname(dirname(path)), edge$family, "README.md")
      if (!scalar_text(edge$family) || !edge$family %in% basename(list.dirs(dirname(dirname(path)), recursive=FALSE)) ||
          !file.exists(target) || edge$family == id)
        stop("load_distributions: invalid relation target for ", id)
      parent <- .umvl_read_front_matter(target)$front
      parent_names <- vapply(parent$params, `[[`, "", "name")
      child_names <- vapply(f$params, `[[`, "", "name")
      validate_map <- function(spec, variables) {
        target_path <- file.path(dirname(dirname(path)), spec$family, "README.md")
        if (!scalar_text(spec$family) || !file.exists(target_path) || spec$family == id)
          stop("load_distributions: invalid construction target for ", id)
        meta <- .umvl_read_front_matter(target_path)$front
        expected <- vapply(meta$params, `[[`, "", "name")
        if (!setequal(names(spec$at), expected) || anyDuplicated(names(spec$at)))
          stop("load_distributions: invalid relation coefficient map for ", id)
        for (value in spec$at) .umvl_support_expr(value, variables)
        meta
      }
      if (!is.null(edge$when)) {
        if (!scalar_text(edge$when)) stop("load_distributions: invalid relation condition for ", id)
        .umvl_support_expr(edge$when, child_names)
        probe <- setNames(lapply(f$params, `[[`, "simulate"), child_names)
        probe$expit <- function(x) stats::plogis(x)
        result <- eval(parse(text=edge$when), envir=probe)
        if (!is.logical(result) || length(result)!=1L || is.na(result))
          stop("load_distributions: condition must be scalar logical for ", id)
      }
      if (kind == "convolution_of") {
        if (length(edge$components)!=2L || !scalar_text(edge$operation) || !edge$operation %in% c("sum", "difference") ||
            !identical(edge$family, edge$components[[1]]$family))
          stop("load_distributions: invalid convolution for ", id)
        for (component in edge$components) {
          meta <- validate_map(component, child_names)
          if (meta$support$type != f$support$type) stop("load_distributions: convolution measure mismatch for ", id)
        }
      } else {
        validate_map(edge, c(child_names, if (kind=="mixture_of") "mix"))
      }
      if (kind == "mixture_of") {
        meta <- validate_map(edge$mixing, child_names)
        if (meta$support$type != "real" || !scalar_text(edge$mixed) || !edge$mixed %in% parent_names ||
            !"mix" %in% all.vars(parse(text=as.character(edge$at[[edge$mixed]]))) ||
            any(vapply(edge$at[setdiff(parent_names,edge$mixed)], function(v) "mix" %in% all.vars(parse(text=as.character(v))), FALSE)))
          stop("load_distributions: invalid mixed coefficient for ", id)
      }
      if (!is.null(edge$shift)) .umvl_support_expr(edge$shift, child_names)
      if (kind == "discretisation_of" && (f$support$type != "integer" || parent$support$type != "real"))
        stop("load_distributions: discretisation measure mismatch for ", id)
      if (kind == "modification_of") {
        if (!scalar_text(edge$type) || !edge$type %in% c("zero-truncated", "zero-modified", "zero-inflated"))
          stop("load_distributions: invalid modification type for ", id)
        if (edge$type != "zero-truncated") {
          if (is.null(edge$mass)) stop("load_distributions: missing modification mass for ", id)
          .umvl_support_expr(edge$mass, child_names)
        }
        if (f$support$type != "integer" || parent$support$type != "integer")
          stop("load_distributions: zero modifications require integer laws for ", id)
      }
      if (kind == "truncation_of" && f$support$type != parent$support$type)
        stop("load_distributions: truncation measure mismatch for ", id)
      if (kind == "transform_of") {
        if (!scalar_text(edge$transform) || !edge$transform %in%
            c("log", "logit", "inverse", "power", "negate", "abs", "shift", "exp", "expit", "affine"))
          stop("load_distributions: invalid relation transform for ", id)
        for (arg in unique(c(if (edge$transform == "affine") c("shift", "scale"), intersect(edge$transform, c("power", "shift")), if (edge$transform == "abs" && !is.null(edge$shift)) "shift"))) {
          if (is.null(edge[[arg]])) stop("load_distributions: missing transform argument for ", id)
          .umvl_support_expr(edge[[arg]], child_names)
        }
      }
    }
  }
  s <- f$support
  support <- list(
    type = if (identical(tolower(s$type), "integer")) "Z" else "R",
    lower = .umvl_support_expr(s$lower, vapply(f$params, `[[`, "", "name")),
    upper = .umvl_support_expr(s$upper, vapply(f$params, `[[`, "", "name")),
    closed = as.logical(unlist(s$closed %||% c(FALSE, FALSE)))
  )
  support$endpoint_log_density <- lapply(s$endpoint_log_density %||% list(),
    .umvl_support_expr, pn = vapply(f$params, `[[`, "", "name"), endpoint = TRUE)
  params <- lapply(f$params, function(p) {
    obsolete <- intersect(names(p), c("default", "fixable", "fixed", "required", "required_reason"))
    if (length(obsolete)) stop("load_distributions: ", id, ": obsolete parameter keys: ", paste(obsolete, collapse=", "))
    if (!is.null(p$symbol) && (!scalar_text(p$symbol) || p$symbol == "x"))
      stop("load_distributions: invalid symbol for ", id, ": ", p$name)
    p$kind <- p$kind %||% "regular"
    if (!p$kind %in% c("design", "threshold", "endpoint", "regular"))
      stop("load_distributions: ", id, ": invalid kind for ", p$name)
    if (is.null(p$simulate) || length(p$simulate) != 1L)
      stop("load_distributions: ", id, ": simulate must be scalar for ", p$name)
    p$simulate <- .umvl_as_bound(p$simulate)
    if (is.na(p$simulate)) stop("load_distributions: ", id, ": simulate must be numeric for ", p$name)
    if (!is.null(p$advice) && (!is.character(p$advice) || length(p$advice)!=1L ||
                             is.na(p$advice) || !nzchar(p$advice)))
      stop("load_distributions: ", id, ": advice must be nonempty text for ", p$name)
    p$required <- p$kind %in% c("design", "threshold")
    domains <- p$fit_domain
    if (!is.null(domains)) {
      if (!is.null(domains$restriction)) domains <- list(domains)
      for (d in domains) {
        r <- d$restriction
        if (is.null(r) || !r$mechanism %in% c("collapse", "spike", "branch", "endpoint") ||
            is.null(r$exempt_when_held) || is.null(r$reason) || !nzchar(r$reason) ||
            (is.null(d$lower) && is.null(d$upper)))
          stop("load_distributions: ", id, ": invalid fit_domain restriction for ", p$name)
        if (p$kind != "regular") stop("load_distributions: ", id,
          ": R5 fit_domain must restrict a regular coefficient, not ", p$kind)
        lower <- if (is.null(d$lower)) -Inf else .umvl_as_bound(d$lower)
        upper <- if (is.null(d$upper)) Inf else .umvl_as_bound(d$upper)
        if (is.na(lower) || is.na(upper) || lower >= upper)
          stop("load_distributions: ", id, ": invalid fit_domain bounds for ", p$name)
        exemptions <- unlist(r$exempt_when_held)
        # ht's translated mu belongs to the declared P4 extension.
        if (length(setdiff(exemptions, c(vapply(f$params, `[[`, "", "name"), if(id=="ht") "mu"))))
          stop("load_distributions: ", id, ": unknown restriction exemption")
      }
    }
    p
  })
  symbols <- vapply(params, function(p) p$symbol %||% p$name, "")
  if (anyDuplicated(symbols)) stop("load_distributions: duplicate symbols for ", id)
  pn <- vapply(params, `[[`, "", "name")
  for (h in f$holds %||% list()) {
    if (!h$status %in% c("no-mle", "not-implemented") ||
        is.null(h$reason) || !nzchar(h$reason) || length(setdiff(unlist(h$held), pn)))
      stop("load_distributions: ", id, ": invalid holds entry")
  }
  # Arguments of the default fit (goldens, generic tests); every required
  # parameter must have one.
  required <- unlist(lapply(params, function(p) if (p$required) p$name))
  missing <- setdiff(required, names(f$fit_args))
  if (length(missing)) {
    stop("load_distributions: ", path, ": fit_args lacks required ",
         paste(missing, collapse = ", "))
  }
  modifier <- f$modifiers
  for (key in c("zero", "truncate", "censor")) {
    records <- if(key=="truncate" && !is.null(modifier[[key]])) list(modifier[[key]]) else modifier[[key]]
    if(key=="zero") records <- records[intersect(names(records),c("truncated","modified","inflated"))]
    for(record in records) {
      allowed <- if(key=="censor") c("certified","local","not-implemented") else c("implemented","not-implemented","no-mle")
      if(is.null(record$status) || !record$status %in% allowed)
        stop("load_distributions: invalid modifier status for ",id)
      if(key!="censor" && (!is.character(record$certified) ||
          !record$certified %in% c("yes","partial","no","unknown")))
        stop("load_distributions: invalid modifier certification for ",id)
      if(length(setdiff(c(unlist(record$requires_held),unlist(record$requires_free)),pn)))
        stop("load_distributions: unknown modifier hold for ",id)
    }
  }
  if(identical(modifier$truncate$status,"implemented") && is.null(modifier$truncate$domain_bounded))
    stop("load_distributions: implemented truncation needs domain_bounded for ",id)
  list(
    id = id,
    name = f$name,
    summary = f$summary,
    group = f$group,
    construction = f$construction,
    modifiers = f$modifiers %||% list(),
    modifier_eligibility = list(zero = (identical(f$support$type, "integer") &&
      identical(as.character(f$support$lower), "0") &&
      !any(c("truncation", "modification") %in% unlist(f$construction))) ||
      isTRUE(f$modifiers$zero_exception), truncate = TRUE),
    relations = f$relations,
    fit = f$fit,
    areas = f$areas,
    classification = f$classification,
    classification_reason = f$classification_reason,
    aliases = f$aliases,
    key_references = keys,
    support = support,
    params = params,
    density = f$r$density,
    sampler = f$r$sampler,
    args = if (is.null(f$r$args)) NULL else unlist(f$r$args),
    shift = f$r$shift %||% NULL,
    fit_args = f$fit_args,
    control = f$r$control %||% list(),
    holds = f$holds %||% list(),
    limits = f$limits %||% list(),
    existence = f$existence,
    inference_numeric = f$inference_numeric,
    references = unlist(f$references %||% list()),
    examples = f$examples %||% NULL,
    sections = .umvl_body_sections(fm$body)
  )
}

load_distributions <- function(dist_dir) {
  folders <- sort(list.dirs(dist_dir, full.names = TRUE, recursive = FALSE))
  models <- list()
  for (folder in folders) {
    id <- basename(folder)
    readme <- file.path(folder, "README.md")
    if (!file.exists(readme)) stop("load_distributions: ", folder, " has no README.md")
    if (!file.exists(file.path(folder, "mle.hpp")) &&
        identical(.umvl_read_front_matter(readme)$front$stage, "dpqr")) next
    models[[id]] <- .umvl_from_readme(readme, id)
  }
  models[order(names(models))]
}
