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
               "lgamma", "lbeta", "dnorm", "dt")
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
  list(
    id = id,
    name = f$name,
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
    models[[id]] <- .umvl_from_readme(readme, id)
  }
  models[order(names(models))]
}
