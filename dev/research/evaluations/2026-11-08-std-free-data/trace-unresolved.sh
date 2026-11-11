#!/usr/bin/env bash
# Instrument temporary copies only; production headers remain unchanged.
set -euo pipefail
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
export CCACHE_DIR=/home/jonas/.cache/ccache CCACHE_BASEDIR="$PWD"
python3 - <<'PY'
from pathlib import Path
import os
before=os.environ.get("UMVL_DIAGNOSTIC_BEFORE")=="1"
root=Path.cwd();folder=root/'build/scratch/std-free-trace';folder.mkdir(exist_ok=True)
library=root/'univariateML-cpp/include/cppunivariate/distributions/lst'
for name,label in [('shape_certificate.hpp','shape'),('signed_face_certificate.hpp','joint')]:
    source=(root/'dev/research/evaluations/2026-11-08-std-free-data'/('shape-before-refinement.hpp' if label=='shape' else 'signed-before-refinement.hpp')) if before else library/name
    s=source.read_text();s='#include <iostream>\n'+s
    if label=='shape':
        condition='if(!root.valid()||root.hi-root.lo>accepted_width*root.mid())return e.out;'
        trace='if(!root.valid()||root.hi-root.lo>accepted_width*root.mid()){std::cerr<<" root_width="<<(root.hi-root.lo)<<" allowed="<<(accepted_width*root.mid())<<" slope_floor="<<(-curvature.hi)<<" ";return e.out;}'
        assert condition in s;s=s.replace(condition,trace)
    else:
        condition='if(!(mid>b.shape.lo&&mid<b.shape.hi))return e.out;'
        trace='if(!(mid>b.shape.lo&&mid<b.shape.hi)){std::cerr<<" shape_width="<<(v.hi-v.lo)<<" scale_bracket="<<b.nuisance.lo<<","<<b.nuisance.hi<<" included="<<b.included<<" ";return e.out;}'
        assert condition in s;s=s.replace(condition,trace)
    s=s.replace('return e.out;', '{std::cerr<<"'+label+':"<<__LINE__<<"\\n";return e.out;}')
    (folder/name).write_text(s)
s=(root/'dev/research/evaluations/2026-11-08-std-free-data/uncertified-diagnostics.cpp').read_text()
s=s.replace('#include "cppunivariate/distributions/std/mle.hpp"','#include "signed_face_certificate.hpp"')
s=s.replace('    if(mask==3){','    std::cerr<<f[0]<<","<<f[1]<<","<<f[2]<<","<<mask<<" ";\n    if(mask==3){')
if before:s='#define UMVL_DIAGNOSTIC_BEFORE 1\n'+s
(folder/'trace.cpp').write_text(s)
PY
/tmp/umvl-cpu/run ccache c++ -O2 -std=c++17 -Ibuild/scratch/std-free-trace -IunivariateML-cpp/include/cppunivariate/distributions/lst -IunivariateML-cpp/include -I/home/jonas/R/x86_64-pc-linux-gnu-library/4.6/BH/include build/scratch/std-free-trace/trace.cpp -o build/scratch/std-free-trace/trace
suffix=current
if test "${UMVL_DIAGNOSTIC_BEFORE:-0}" = 1; then suffix=before-refinement; fi
/tmp/umvl-cpu/run build/scratch/std-free-trace/trace > "dev/research/evaluations/2026-11-08-std-free-data/unresolved-trace-$suffix.csv" 2> "dev/research/evaluations/2026-11-08-std-free-data/unresolved-trace-$suffix.txt"
