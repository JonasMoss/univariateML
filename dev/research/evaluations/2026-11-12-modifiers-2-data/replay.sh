#!/bin/sh
# Run from the repository root through /tmp/umvl-cpu/run (one slot).
# Isolate generation from any concurrently queued full R suite.
set -eu
umvl_replay_root=$PWD
export TMPDIR=$PWD/build/tmp JUST_TEMPDIR=$PWD/build/tmp XDG_RUNTIME_DIR=$PWD/build/tmp
export UMVL_PKG_DIR=$PWD/build/scratch/modifiers-2-audit-package
export UMVL_PKG_R_DIR=$UMVL_PKG_DIR/R
export UMVL_AUDIT_PKG=$UMVL_PKG_DIR
export UMVL_DOSSIER_DIR=$PWD/univariateML-cpp/include/cppunivariate/distributions
export CCACHE_DIR=$HOME/.cache/ccache/univariateML CCACHE_BASEDIR=$PWD CCACHE_NOHASHDIR=1
export MAKEFLAGS=-j1 PKG_BUILD_EXTRA_FLAGS=false
mkdir -p "$TMPDIR"
(cd bindings/r && Rscript scripts/assemble_package.R)
mkdir -p "$UMVL_PKG_DIR/inst/include"
rsync -a --delete --prune-empty-dirs --include='*/' --include='*.hpp' --exclude='*' univariateML-cpp/include/cppunivariate "$UMVL_PKG_DIR/inst/include/"
Rscript -e 'Rcpp::compileAttributes(Sys.getenv("UMVL_PKG_DIR"))'
(cd bindings/r && Rscript scripts/codegen_metadata.R)
(cd bindings/r && Rscript scripts/codegen_wrappers.R)
(cd bindings/r && Rscript scripts/codegen_links.R)
Rscript -e 'options(pkg.build_extra_flags=FALSE);roxygen2::roxygenise(Sys.getenv("UMVL_PKG_DIR"))'
UMVL_REPLAY_ONLY=1 Rscript dev/research/evaluations/2026-11-12-modifiers-2-data/audit.R
python3 dev/research/evaluations/2026-11-12-modifiers-2-data/classify.py
python3 dev/research/evaluations/2026-11-12-modifiers-2-data/check_tbinom_defaults.py
python3 dev/research/evaluations/2026-11-12-modifiers-2-data/zeta_replay_reference.py
Rscript dev/research/evaluations/2026-11-12-modifiers-2-data/changed_likelihoods.R
python3 dev/research/evaluations/2026-11-12-modifiers-2-data/narrow_normal_reference.py
python3 dev/research/evaluations/2026-11-12-modifiers-2-data/pack_evidence.py
