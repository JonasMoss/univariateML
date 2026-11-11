#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")/../../../.."
data=dev/research/evaluations/2026-11-10-fallback-count-data
mkdir -p build/tmp build/scratch/count-frozen
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
tar xzf "$data/baseline-headers.tar.gz" -C build/scratch/count-frozen
export CCACHE_DIR="${UMVL_CCACHE_DIR:-$HOME/.cache/ccache/univariateML}" CCACHE_BASEDIR="$PWD" CCACHE_NOHASHDIR=1
compiler=(c++)
if command -v ccache >/dev/null; then compiler=(ccache c++); fi
boost=$(Rscript -e 'cat(system.file("include",package="BH"))')
"${compiler[@]}" -std=c++17 -O2 -Ibuild/scratch/count-frozen -Ibuild/scratch/count-frozen/include -I"$boost" "$data/study.cpp" -o build/scratch/count-before-final
"${compiler[@]}" -std=c++17 -O2 -I. -IunivariateML-cpp/include -I"$boost" "$data/study.cpp" -o build/scratch/count-after-final
python3 - "$data" <<'PY'
import hashlib,json,pathlib,sys
data=pathlib.Path(sys.argv[1])
def digest(p):return hashlib.sha256(p.read_bytes()).hexdigest()
for side in ('before','after'):
    binary=pathlib.Path(f'build/scratch/count-{side}-final')
    output=binary.with_suffix('.tsv');stamp=binary.with_suffix('.identity.json')
    identity={str(p):digest(p) for p in (binary,data/'cases.json',data/'study.py')}
    if not stamp.exists() or json.loads(stamp.read_text())!=identity:
        output.unlink(missing_ok=True)
    stamp.write_text(json.dumps(identity,indent=2)+'\n')
PY
python3 "$data/study.py" "$PWD/build/scratch/count-before-final" "$PWD/build/scratch/count-before-final.tsv"
python3 "$data/study.py" "$PWD/build/scratch/count-after-final" "$PWD/build/scratch/count-after-final.tsv"
python3 "$data/recheck.py"
cp build/scratch/count-before-final.tsv "$data/before-final.tsv"
cp build/scratch/count-after-final.tsv "$data/after-final.tsv"
python3 "$data/summarize.py"
python3 "$data/check-points.py"
c++ --version | head -n 1
