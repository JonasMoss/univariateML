# Regex parser for per-distribution metadata.hpp files.
#
# The cppunivariate canonical form is a constexpr-only header at
# cppunivariate/include/cppunivariate/distributions/<id>/metadata.hpp.
# This parser extracts the data the R-side codegen scripts need
# (id, name, support, params) without depending on a C++ compiler.
#
# The parser keys on a constrained format documented in
# univariateML/metadata/SCHEMA.md. Files that deviate from that
# format produce parse errors with the offending section noted;
# either fix the file or extend the parser.
#
# kInf is recognised as a per-distribution alias for
# std::numeric_limits<double>::infinity(); -kInf for -Inf.

# Strip C/C++ comments before regex matching.
.strip_comments <- function(text) {
  text <- gsub("//[^\n]*", "", text)
  gsub("/\\*.*?\\*/", "", text, perl = TRUE)
}

.parse_number <- function(tok) {
  tok <- trimws(tok)
  if (identical(tok, "kInf")) return(Inf)
  if (identical(tok, "-kInf")) return(-Inf)
  v <- suppressWarnings(as.numeric(tok))
  if (is.na(v)) {
    stop("parse_cpp_metadata: cannot parse number from token '", tok, "'")
  }
  v
}

.parse_support <- function(content) {
  parts <- trimws(strsplit(content, ",", fixed = TRUE)[[1]])
  if (length(parts) != 5L) {
    stop(
      "parse_cpp_metadata: support descriptor expected 5 fields ",
      "(type, lower, upper, lower_closed, upper_closed); got ",
      length(parts), " in '", content, "'"
    )
  }
  type <- if (grepl("Integer", parts[1], fixed = TRUE)) "Z" else "R"
  list(
    type = type,
    lower = .parse_number(parts[2]),
    upper = .parse_number(parts[3]),
    closed = c(parts[4] == "true", parts[5] == "true")
  )
}

.extract_match <- function(text, pattern, what) {
  m <- regmatches(text, regexpr(pattern, text, perl = TRUE))
  if (length(m) == 0L || !nzchar(m)) {
    stop("parse_cpp_metadata: could not find ", what, " in metadata.hpp")
  }
  m
}

#' Parse a cppunivariate distributions/<id>/metadata.hpp file.
#'
#' Returns a list with `id`, `name`, `support`, `params`.
parse_cpp_metadata <- function(path) {
  text <- paste(readLines(path, warn = FALSE), collapse = "\n")
  text <- .strip_comments(text)

  id_block   <- .extract_match(text,
    'constexpr\\s+const\\s+char\\s*\\*\\s*id\\s*=\\s*"([^"]+)"', "id")
  name_block <- .extract_match(text,
    'constexpr\\s+const\\s+char\\s*\\*\\s*name\\s*=\\s*"([^"]+)"', "name")
  id   <- sub('.*"([^"]+)".*', "\\1", id_block)
  name <- sub('.*"([^"]+)".*', "\\1", name_block)

  support_block <- .extract_match(text,
    'constexpr\\s+SupportDescriptor\\s+support\\s*\\{([^}]+)\\}',
    "support descriptor")
  support_content <- sub('.*\\{([^}]+)\\}.*', "\\1", support_block)
  support <- .parse_support(support_content)

  # The params block: constexpr ParamDescriptor params[] = { ENTRIES };
  # ENTRIES is one or more {NAME, ROLE, DEFAULT, ESTIMABLE, {SUPPORT}}
  # joined by commas (trailing comma allowed).
  param_pattern <- paste0(
    '\\{"([^"]+)",\\s*',         # name
    '"([^"]+)",\\s*',            # role
    '(-?[0-9.eE+\\-]+|-?kInf),\\s*',  # default (number or kInf alias)
    '(true|false),\\s*',         # estimable
    '\\{([^}]+)\\}\\s*',          # inner support
    '\\}'
  )
  matches <- regmatches(text, gregexpr(param_pattern, text, perl = TRUE))[[1]]
  if (length(matches) == 0L) {
    stop("parse_cpp_metadata: no ParamDescriptor entries found in ", path)
  }
  params <- lapply(matches, function(entry) {
    cap <- regmatches(entry, regexec(param_pattern, entry, perl = TRUE))[[1]]
    list(
      name = cap[2],
      role = cap[3],
      default = .parse_number(cap[4]),
      estimable = cap[5] == "true",
      support = .parse_support(cap[6])
    )
  })

  list(id = id, name = name, support = support, params = params)
}
