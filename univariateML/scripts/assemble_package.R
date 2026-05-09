# Wipe and rebuild univariateML/package/ from workspace sources.
#
# Run from univariateML/ cwd, e.g. via `just assemble`.
#
# After this script, package/ contains hand-authored sources only.
# The codegen scripts, Rcpp::compileAttributes(), and roxygen2 layer
# generated files (ml00_metadata.R, ml02_wrappers.R, RcppExports.*,
# NAMESPACE, man/*.Rd) on top in subsequent regen steps.

PKG_DIR <- "package"

unlink(PKG_DIR, recursive = TRUE, force = TRUE)
dir.create(PKG_DIR)

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

invisible(NULL)
