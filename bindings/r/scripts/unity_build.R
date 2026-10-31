# Select compilation units without hiding the family sources from Rcpp's
# attribute scanner. Run after d/p/q/r generation during package assembly.
configure_unity_build <- function(pkg) {
  setting <- Sys.getenv("UMVL_UNITY_GROUPS", "4")
  if (!grepl("^[0-9]+$", setting)) stop("UMVL_UNITY_GROUPS must be a nonnegative integer")
  groups <- as.integer(setting)
  if (is.na(groups)) stop("UMVL_UNITY_GROUPS is too large")
  src <- file.path(pkg, "src")
  families <- sort(list.files(src, "^dist_.*\\.cpp$"))
  if (!groups || !length(families)) return(invisible(NULL))
  groups <- min(groups, length(families))
  units <- sprintf("unity_%02d.cpp", seq_len(groups))
  for (i in seq_len(groups)) {
    members <- families[(seq_along(families) - 1L) %% groups == i - 1L]
    includes <- unique(unlist(lapply(file.path(src, members), function(f) {
      x <- readLines(f, warn = FALSE)
      x[grepl("^#include", x)]
    })))
    # Preload every C++/shared-fit header at global scope, before Rcpp/Rmath
    # introduces function-name macros. Include guards then protect subsequent
    # family includes. R-shell headers follow in their original order.
    library <- grepl('"(common/|cppunivariate/)', includes)
    writeLines(c("// Generated unity compilation unit; edit the binding sources.",
                 includes[library], includes[!library],
                 paste0('#include "', members, '"')), file.path(src, units[i]))
  }
  common <- setdiff(list.files(src, "\\.cpp$"), c(families, units))
  # RcppExports.cpp is generated later by compileAttributes().
  common <- union(common, "RcppExports.cpp")
  objects <- sub("\\.cpp$", ".o", c(common, units))
  for (makevars in c("Makevars", "Makevars.win")) {
    path <- file.path(src, makevars)
    if (!file.exists(path)) next
    cat("\n# Generated unity layout; UMVL_UNITY_GROUPS=0 restores family objects.\n",
        "OBJECTS = ", paste(objects, collapse = " "), "\n", sep = "", file = path, append = TRUE)
    # Makevars is read before R's standard targets: keep the library as the
    # default goal rather than accidentally making the first group the goal.
    cat("all: $(SHLIB)\n", file = path, append = TRUE)
    # Explicit dependencies also support direct incremental R CMD INSTALL.
    for (i in seq_len(groups)) {
      members <- families[(seq_along(families) - 1L) %% groups == i - 1L]
      cat(sub("\\.cpp$", ".o", units[i]), ": ", paste(members, collapse = " "),
          "\n", sep = "", file = path, append = TRUE)
    }
  }
}
