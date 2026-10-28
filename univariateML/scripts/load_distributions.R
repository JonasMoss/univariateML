# Load per-distribution metadata for the R-side codegen and goldens from
# distributions/<id>/README.md (YAML front matter + prose; schema:
# distributions/README_SCHEMA.md). Returns a list of models sorted by id,
# each with: id, name, support, params, density, sampler, control, limits,
# references, examples, sections.

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
    lower = .umvl_as_bound(s$lower),
    upper = .umvl_as_bound(s$upper),
    closed = as.logical(unlist(s$closed %||% c(FALSE, FALSE)))
  )
  params <- lapply(f$params, function(p) {
    list(name = p$name, role = p$role %||% "", domain = p$domain %||% "",
         default = as.numeric(p$default))
  })
  list(
    id = id,
    name = f$name,
    support = support,
    params = params,
    density = f$r$density,
    sampler = f$r$sampler,
    control = f$r$control %||% list(),
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
