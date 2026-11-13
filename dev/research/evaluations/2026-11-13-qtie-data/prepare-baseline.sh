#!/usr/bin/env bash
# Read-only Git projection into ignored scratch. No archive is retained.
set -euo pipefail
mkdir -p build/tmp build/scratch/baseline
export TMPDIR=$PWD/build/tmp JUST_TEMPDIR=$PWD/build/tmp XDG_RUNTIME_DIR=$PWD/build/tmp
git archive fbc13bf8 univariateML-cpp/include | tar -x -C build/scratch/baseline
# The current report-only harness mentions the new helper in unexecuted
# doctest branches. Baseline kernels never call this declaration/shim.
cat > build/scratch/baseline-shim.hpp <<'CPP'
namespace cppunivariate { namespace special {
inline int discrete_reached(double f,double p,bool lower,bool) {return lower?f>=p:f<=p;}
}}
CPP
