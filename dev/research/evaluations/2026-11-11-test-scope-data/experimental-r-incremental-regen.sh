#!/usr/bin/env bash
# Experimental TASK-193 candidate; no public assembly mode is enabled.
# Run from the repository root under the CPU reservation.
set -euo pipefail
root=$(pwd)
test -f "$root/Justfile"
cd "$root"
export CCACHE_DIR="${UMVL_CCACHE_DIR:-$HOME/.cache/ccache/univariateML}"
export CCACHE_BASEDIR="$root" CCACHE_NOHASHDIR=1 CCACHE_MAXSIZE=8G
stage="$root/build/scratch/r-incremental-stage"
export UMVL_PKG_DIR="$stage" UMVL_PKG_R_DIR="$stage/R"
export UMVL_DOSSIER_DIR="$root/univariateML-cpp/include/cppunivariate/distributions"
(cd bindings/r && Rscript scripts/assemble_package.R)
mkdir -p "$stage/inst/include"
rsync -a --delete --prune-empty-dirs --include='*/' --include='*.hpp' --exclude='*' \
  univariateML-cpp/include/cppunivariate "$stage/inst/include/"
(cd "$stage" && Rscript -e 'Rcpp::compileAttributes()')
(cd bindings/r && Rscript scripts/codegen_metadata.R && Rscript scripts/codegen_wrappers.R && Rscript scripts/codegen_links.R)
mkdir -p univariateML-r
# R does not track transitive header dependencies. Invalidate objects when any
# header changes (ccache still reuses unaffected translation units), and whenever
# the build layout or flags change. Source timestamps handle adapter edits.
changes=$(rsync -rcni --delete --exclude='*.o' --exclude='*.so' "$stage/" univariateML-r/)
if printf '%s\n' "$changes" | grep -Eq '(\.h(pp)?$|src/Makevars|src/unity_.*\.cpp$|deleting.*src/.*\.cpp$)'; then
  find univariateML-r/src -type f \( -name '*.o' -o -name '*.so' \) -delete 2>/dev/null || true
fi
rsync -rc --delete --exclude='*.o' --exclude='*.so' "$stage/" univariateML-r/
rm -rf "$stage"
(cd univariateML-r && PKG_BUILD_EXTRA_FLAGS=false Rscript -e 'options(pkg.build_extra_flags=FALSE); roxygen2::roxygenise()')
