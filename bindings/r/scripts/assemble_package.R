# Wipe and rebuild univariateML-r/ from workspace sources.
#
# Run from bindings/r/ cwd, e.g. via `just assemble`.
#
# After this script, package/ contains hand-authored sources only.
# The codegen scripts, Rcpp::compileAttributes(), and roxygen2 layer
# generated files (ml00_metadata.R, ml02_wrappers.R, RcppExports.*,
# NAMESPACE, man/*.Rd) on top in subsequent regen steps.

PKG_DIR <- Sys.getenv("UMVL_PKG_DIR", "../../univariateML-r")

unlink(PKG_DIR, recursive = TRUE, force = TRUE)
dir.create(PKG_DIR, recursive = TRUE)

top_files <- c(
  "DESCRIPTION", "NAMESPACE", ".Rbuildignore", "LICENSE", "LICENSE.md",
  "README.md", "NEWS.md"
)
for (f in top_files) {
  if (file.exists(f)) {
    file.copy(f, PKG_DIR, overwrite = TRUE)
  }
}

for (d in c("R", "src", "tests", "data", "inst", "vignettes")) {
  if (dir.exists(d)) {
    file.copy(d, PKG_DIR, recursive = TRUE)
  }
}

# README.md references images at man/figures/. Roxygen2 owns
# man/*.Rd, but man/figures/ is hand-authored asset content; copy it
# into the freshly assembled package so the rendered README in the
# tarball still resolves its image links.
if (dir.exists(file.path("man", "figures"))) {
  dir.create(file.path(PKG_DIR, "man"), recursive = TRUE, showWarnings = FALSE)
  file.copy(
    file.path("man", "figures"), file.path(PKG_DIR, "man"),
    recursive = TRUE
  )
}

# One folder per distribution: copy each distributions/<id>/adapter.cpp to
# src/dist_<id>.cpp and test.R to tests/testthat/test-dist-<id>.R.
dist_dir <- Sys.getenv(
  "UMVL_DOSSIER_DIR",
  "../../univariateML-cpp/include/cppunivariate/distributions"
)
binding_dist_dir <- Sys.getenv("UMVL_BINDINGS_DIR", "distributions")
for (folder in list.dirs(binding_dist_dir, full.names = TRUE, recursive = FALSE)) {
  id <- basename(folder)
  adapter <- file.path(folder, "adapter.cpp")
  if (file.exists(adapter)) {
    file.copy(adapter, file.path(PKG_DIR, "src", paste0("dist_", id, ".cpp")))
  }
  test <- file.path(folder, "test.R")
  if (file.exists(test)) {
    file.copy(test, file.path(PKG_DIR, "tests", "testthat", paste0("test-dist-", id, ".R")))
  }
}

# Test-only evidence: front matter and all frozen JSON, never installed.
fixtures <- file.path(PKG_DIR, "tests", "testthat", "fixtures")
for (folder in list.dirs(dist_dir, full.names = TRUE, recursive = FALSE)) {
  id <- basename(folder)
  readme <- file.path(folder, "README.md")
  if (!file.exists(readme)) next
  target <- file.path(fixtures, id)
  dir.create(target, recursive = TRUE, showWarnings = FALSE)
  lines <- readLines(readme, warn = FALSE, encoding = "UTF-8")
  marks <- which(trimws(lines) == "---")
  stopifnot(length(marks) >= 2L, marks[1L] == 1L)
  writeLines(lines[seq_len(marks[2L])], file.path(target, "README.md"))
  json <- list.files(folder, pattern = "\\.json$", full.names = TRUE)
  if (length(json)) file.copy(json, target)
}

source(file.path("scripts", "codegen_dpqr.R"))

invisible(NULL)
