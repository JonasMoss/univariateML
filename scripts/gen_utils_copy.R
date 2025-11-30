# Copy handwritten R sources from r-src into R with prefixes by category.
# Run via: Rscript scripts/gen_utils_copy.R

dst_dir <- "R"
if (!dir.exists(dst_dir)) dir.create(dst_dir)

# code_* files
code_files <- list.files("r-src/code", pattern = "\\.R$", full.names = TRUE)
for (f in code_files) {
  file.copy(f, file.path(dst_dir, paste0("code_", basename(f))), overwrite = TRUE)
}

# data files prefixed with data_
data_files <- list.files("r-src/data", pattern = "\\.R$", full.names = TRUE)
for (f in data_files) {
  file.copy(f, file.path(dst_dir, paste0("data_", basename(f))), overwrite = TRUE)
}

# pkg_* files (e.g., zzz.R, univariateML-package.R)
pkg_files <- list.files("r-src", pattern = "^zzz\\.R$|^univariateML-package\\.R$", full.names = TRUE)
for (f in pkg_files) {
  file.copy(f, file.path(dst_dir, paste0("pkg_", basename(f))), overwrite = TRUE)
}
