#!/usr/bin/env bash
# Two independent source generations; run under the workstation CPU lock.
set -euo pipefail
root=$(cd "$(dirname "$0")/../.." && pwd)
export CCACHE_DIR="${UMVL_CCACHE_DIR:-$HOME/.cache/ccache/univariateML}"
export CCACHE_BASEDIR="$root" CCACHE_NOHASHDIR=1
export TMPDIR="$root/build/tmp" LC_ALL=C.UTF-8 TZ=UTC
mkdir -p "$TMPDIR" "$root/build/scratch"
export UMVL_DOSSIER_DIR="$root/univariateML-cpp/include/cppunivariate/distributions"
for run in a b; do
  export UMVL_PKG_DIR="$root/build/scratch/repro-$run"
  export UMVL_PKG_R_DIR="$UMVL_PKG_DIR/R"
  (cd "$root/bindings/r" && Rscript scripts/assemble_package.R)
  mkdir -p "$UMVL_PKG_DIR/inst/include"
  rsync -a --delete --prune-empty-dirs --include='*/' --include='*.hpp' --exclude='*' \
    "$root/univariateML-cpp/include/cppunivariate" "$UMVL_PKG_DIR/inst/include/"
  (cd "$UMVL_PKG_DIR" && Rscript -e 'Rcpp::compileAttributes()')
  (cd "$root/bindings/r" && Rscript scripts/codegen_metadata.R && Rscript scripts/codegen_wrappers.R)
  # Use the production loader (including documented datasets).
  (cd "$UMVL_PKG_DIR" && Rscript -e 'roxygen2::roxygenise()')
  # These compiler products are excluded by R CMD build too; compare sources.
  find "$UMVL_PKG_DIR/src" -type f \( -name '*.o' -o -name '*.so' \) -delete
done
diff -ru "$root/build/scratch/repro-a" "$root/build/scratch/repro-b" \
  > "$root/build/scratch/reproducibility.diff"
echo 'Two independent generations are byte-identical (diff -ru exit 0).'
