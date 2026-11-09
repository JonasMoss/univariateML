#!/usr/bin/env bash
# Restore the audit replay without modifying its frozen evidence directory.
set -euo pipefail
cd "$(dirname "$0")/../../../.."
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
mkdir -p "$TMPDIR" build/scratch/fit-audit
python3 - <<'PY'
import pathlib,gzip,re
root=pathlib.Path.cwd(); old=root/'dev/research/evaluations/2026-11-09-fit-audit-rerun-data'; w=root/'build/scratch/fit-audit'
s=gzip.decompress((old/'harness.cpp.gz').read_bytes()).decode()
s=re.sub(r'"/[^"\n]+/bindings/common/fit.hpp"',f'"{root}/bindings/common/fit.hpp"',s)
(w/'stress.cpp').write_text(s)
(w/'cases.json').write_bytes(gzip.decompress((old.parent/'2026-11-07-fit-audit-data/cases.json.gz').read_bytes()))
(w/'reference.jsonl').write_bytes(gzip.decompress((old/'reference.jsonl.gz').read_bytes()))
(w/'reproduce.py').write_text((old/'reproduce.py').read_text().replace('ROOT=HERE.parents[3]',f'ROOT=pathlib.Path({str(root)!r})'))
PY
/tmp/umvl-cpu/run bash -euo pipefail -c '
boost=$(Rscript -e '\''cat(system.file("include",package="BH"))'\'')
export CCACHE_DIR="$HOME/.cache/ccache/univariateML" CCACHE_BASEDIR="$PWD" CCACHE_NOHASHDIR=1
ccache c++ -std=c++17 -O1 -IunivariateML-cpp/include -I"$boost" build/scratch/fit-audit/stress.cpp -o build/scratch/fit-audit/stress
' 
