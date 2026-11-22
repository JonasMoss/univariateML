#!/usr/bin/env bash
set -euo pipefail
# Run from the repository root. The archived baseline and probes are disposable.
mkdir -p "$PWD/build/tmp"
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
work="$PWD/build/scratch/flat-cont"
base="$work/source"
data="$PWD/dev/research/evaluations/2026-11-22-flat-cont-data"
families=gev,gompertz,gpd,sged,tlambda
mkdir -p "$base"
git archive 7d0dd9ec univariateML-cpp/include bindings/common     dev/validation/fit-snapshot.py dev/validation/flat_maxima.py     dev/research/evaluations/2026-11-07-fit-audit-data/cases.json.gz     dev/research/evaluations/2026-11-13-retire-16-data/route-inputs.json | tar -x -C "$base"
for mode in native double; do
    flags=()
    if [[ $mode == double ]]; then flags+=(--no-long-double); fi
    /tmp/umvl-cpu/run python3 dev/validation/fit-snapshot.py --family "$families" --output "$work/$mode" "${flags[@]}"
    /tmp/umvl-cpu/run python3 "$base/dev/validation/fit-snapshot.py" --family "$families" --output "$work/base/$mode" "${flags[@]}"
done
/tmp/umvl-cpu/run python3 "$data/compare.py" > "$data/snapshots.txt"
/tmp/umvl-cpu/run python3 dev/validation/fit-consistency.py "$work/native" "$work/double" | tee "$data/strict.txt"
/tmp/umvl-cpu/run python3 dev/research/evaluations/2026-11-22-zero-unique-data/sged_profile.py > "$data/sged-signs.txt"
