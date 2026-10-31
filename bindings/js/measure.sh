#!/usr/bin/env bash
# Reserve all four CPU slots before invoking; builds remain sequential.
set -euo pipefail
cd "$(dirname "$0")/../.."
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
mkdir -p "$TMPDIR" build/scratch
mapfile -t families < <(python3 - <<'PY'
from pathlib import Path
for path in sorted(Path('bindings/common/distributions').glob('*/fit.hpp')):
 print(path.parent.name)
PY
)
if [[ ${#families[@]} != 98 ]]; then
 echo 'Layout measurement requires all 98 shared fitters.' >&2
 exit 1
fi
modules=()
measured=0
for family in "${families[@]}"; do
 JS_FAMILIES="$family" bash bindings/js/build.sh O2 > "build/scratch/layout-$family-build.log" 2>&1
 mkdir -p "build/js/per-family/$family"
 cp build/js/univariateML.{mjs,wasm} "build/js/per-family/$family/"
 modules+=("per-family/$family/univariateML")
 measured=$((measured+1))
 echo "Layout build $measured/98: $family"
done
unset JS_FAMILIES
just js-build > build/scratch/js-final-build.log 2>&1
node --v8-pool-size=1 bindings/js/measure-all.mjs univariateML "${modules[@]}" > build/scratch/module-layout.log 2>&1
just js-test > build/scratch/js-final-test.log 2>&1
