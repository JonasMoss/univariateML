#!/usr/bin/env bash
# Non-overlapping stages of just test (compilation separated from roxygen load).
# Run inside run4; generated products and logs remain in this worktree.
set -euo pipefail
label=${1:?usage: measure-r-rebuild.sh label}
root=$(pwd)
export CCACHE_DIR="${UMVL_CCACHE_DIR:-$HOME/.cache/ccache/univariateML}"
export CCACHE_BASEDIR="$root" CCACHE_NOHASHDIR=1 CCACHE_MAXSIZE=8G
log="$root/build/scratch/r-$label"
mkdir -p "$log" "$root/build/tmp"
export TMPDIR="$root/build/tmp" JUST_TEMPDIR="$root/build/tmp" XDG_RUNTIME_DIR="$root/build/tmp"
export UMVL_DOSSIER_DIR="$root/univariateML-cpp/include/cppunivariate/distributions"
export UMVL_PKG_R_DIR="$root/univariateML-r/R"
time_stage() { /usr/bin/time -p -o "$log/$1.time" "${@:2}" > "$log/$1.log" 2>&1; }
time_stage assemble just sync-headers
time_stage gen bash -c 'cd univariateML-r && Rscript -e "Rcpp::compileAttributes()" && cd ../bindings/r && Rscript scripts/codegen_metadata.R && Rscript scripts/codegen_wrappers.R && Rscript scripts/codegen_links.R'
export UMVL_DISTRIBUTIONS_DIR="$UMVL_DOSSIER_DIR"
export UMVL_TEST_TIER=${UMVL_TEST_TIER:-dev} PKG_BUILD_EXTRA_FLAGS=false TESTTHAT_CPUS=4
cd univariateML-r
time_stage compile Rscript -e 'options(pkg.build_extra_flags=FALSE); pkgload::load_all(".", helpers=FALSE)'
time_stage regen Rscript -e 'options(pkg.build_extra_flags=FALSE); roxygen2::roxygenise()'
if [ "${UMVL_MEASURE_SKIP_TESTS:-0}" != 1 ]; then
  time_stage tests Rscript ../bindings/r/scripts/test_local.R
fi
cat "$log/"*.time
