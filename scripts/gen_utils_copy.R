# Copy handwritten R sources from r-src/util into R/
# Run via: Rscript scripts/gen_utils_copy.R

src_dir <- "r-src/util"
dst_dir <- "R"

files <- list.files(src_dir, pattern = "\\.R$", full.names = TRUE)
if (!dir.exists(dst_dir)) dir.create(dst_dir)

for (f in files) {
  file.copy(f, file.path(dst_dir, basename(f)), overwrite = TRUE)
}
