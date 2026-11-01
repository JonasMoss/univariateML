# Assembly starts without man/*.Rd, so roxygen cannot discover local topics
# during its first Markdown pass. Qualify links to functions defined in this
# package before roxygen runs; external links and displayed labels stay intact.
pkg_r_dir <- Sys.getenv("UMVL_PKG_R_DIR", "../../univariateML-r/R")
files <- list.files(pkg_r_dir, pattern = "\\.R$", full.names = TRUE)
functions <- unique(unlist(lapply(files, function(file) {
  expressions <- parse(file)
  vapply(Filter(function(e) is.call(e) && identical(e[[1L]], as.name("<-")) &&
    is.symbol(e[[2L]]) && is.call(e[[3L]]) &&
    identical(e[[3L]][[1L]], as.name("function")), as.list(expressions)),
    function(e) as.character(e[[2L]]), "")
})))
for (file in files) {
  lines <- readLines(file, warn = FALSE)
  docs <- startsWith(lines, "#'")
  # Normalize implicit roxygen titles in the assembled copy. checkRd rejects
  # terminal periods in titles, including titles spanning several lines.
  starts <- which(docs & !c(FALSE, head(docs, -1L)))
  for (start in starts) {
    end <- start
    while (end < length(lines) && docs[end + 1L] &&
           nzchar(trimws(sub("^#'", "", lines[end + 1L]))) &&
           !grepl("^#'\\s*@", lines[end + 1L])) end <- end + 1L
    if (!grepl("^#'\\s*@", lines[start]))
      lines[end] <- sub("\\.[[:space:]]*$", "", lines[end])
  }
  for (topic in functions) {
    # Function-call Markdown links, including [label][function()].
    old <- paste0("[", topic, "()]")
    new <- paste0("[univariateML::", topic, "()]")
    lines[docs] <- gsub(old, new, lines[docs], fixed = TRUE)
  }
  writeLines(lines, file)
}
