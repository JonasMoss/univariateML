#!/usr/bin/env bash
# Foreground replay of all saved cases, with an immutable master baseline.
set -euo pipefail
cd "$(dirname "$0")/../../../.."
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
mkdir -p build/tmp build/scratch/audit-support-baseline
/tmp/umvl-cpu/run bash -euo pipefail -c '
  evidence=dev/research/evaluations/2026-11-09-audit-support-data
  baseline=build/scratch/audit-support-baseline
  cp -a univariateML-cpp/include/cppunivariate "$baseline/"
  for path in emg/location_certificate.hpp triang/mle.hpp gev/mle.hpp; do
    git show "594c491dd495892b9cffd8977fcc9809fcbec151:univariateML-cpp/include/cppunivariate/distributions/$path" > "$baseline/cppunivariate/distributions/$path"
  done
  boost=$(Rscript -e '\''cat(system.file("include",package="BH"))'\'')
  c++ -O1 -std=c++17 -I"$baseline" -I"$boost" "$evidence/replay.cpp" -o build/scratch/audit-support-before
  AUDIT_SUPPORT_DEFAULTS=1 AUDIT_SUPPORT_BINARY=build/scratch/audit-support-before python3 "$evidence/replay.py" > "$evidence/before.csv"
  AUDIT_SUPPORT_BINARY=build/scratch/audit-support-before python3 "$evidence/replay.py" > "$evidence/before-contract.csv"
  c++ -O1 -std=c++17 -IunivariateML-cpp/include -I"$boost" "$evidence/replay.cpp" -o build/scratch/audit-support-replay
  AUDIT_SUPPORT_DEFAULTS=1 python3 "$evidence/replay.py" > "$evidence/after.csv"
  python3 "$evidence/replay.py" > "$evidence/after-contract.csv"
  AUDIT_SUPPORT_DEFAULTS=1 AUDIT_SUPPORT_EXHAUSTIVE=1 AUDIT_SUPPORT_FAMILY=triang AUDIT_SUPPORT_BINARY=build/scratch/audit-support-before python3 "$evidence/replay.py" > "$evidence/before-exhaustive.csv"
  AUDIT_SUPPORT_DEFAULTS=1 AUDIT_SUPPORT_EXHAUSTIVE=1 AUDIT_SUPPORT_FAMILY=triang python3 "$evidence/replay.py" > "$evidence/after-exhaustive.csv"
  c++ -O1 -std=c++17 -I"$baseline" -I"$boost" "$evidence/subnormal-support.cpp" -o build/scratch/audit-support-subnormal-before
  build/scratch/audit-support-subnormal-before > "$evidence/subnormal-before.csv"
  c++ -O1 -std=c++17 -IunivariateML-cpp/include -I"$boost" "$evidence/subnormal-support.cpp" -o build/scratch/audit-support-subnormal-after
  build/scratch/audit-support-subnormal-after > "$evidence/subnormal-after.csv"
  python3 "$evidence/support_validation.py"
  c++ -O1 -std=c++17 -IunivariateML-cpp/include -I"$boost" "$evidence/settled-seam.cpp" -o build/scratch/audit-support-settled-seam
  build/scratch/audit-support-settled-seam > "$evidence/settled-seam.json"
  python3 "$evidence/precision.py"
  python3 "$evidence/remaining.py"
  python3 "$evidence/validate.py" before after before-contract after-contract
'
