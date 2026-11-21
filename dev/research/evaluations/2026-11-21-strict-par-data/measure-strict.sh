#!/usr/bin/env bash
set -euo pipefail
root=$PWD
scratch=$root/build/scratch
run=$1
dest=$scratch/strict-$run
jobs=4
profile=-
source=$root
uncached=()
case $run in
  old)
    source=$scratch/master
    profile=$scratch/profile-replay.py
    jobs=1
    export STRICT_PROFILE=$dest/mechanisms.csv
    export STRICT_SEAM_PROFILE=$dest
    ;;
  serial)
    uncached=(--no-cache)
    jobs=1
    ;;
  warm-serial) jobs=1 ;;
  cold|warm|touched) ;;
  *) exit 2 ;;
esac
mkdir -p "$dest"
export TMPDIR=$root/build/tmp JUST_TEMPDIR=$root/build/tmp XDG_RUNTIME_DIR=$root/build/tmp
export PYTHONPATH=$scratch/audit-hook:$source/dev/validation
export STRICT_RATCHET_DIR=$dest
export PYTHONUNBUFFERED=1
export PYTHONDONTWRITEBYTECODE=1
python3 "$scratch/timed-strict.py" "$source/Justfile" "$dest/Justfile" "$dest" "$profile" "$dest/replay.jsonl" "${uncached[@]}"
native=$root/build/snapshots/native,$root/build/snapshots/routes-native
double=$root/build/snapshots/double,$root/build/snapshots/routes-double
if [[ $run == touched ]]; then
  native+=,$scratch/touch-native
  double+=,$scratch/touch-double
fi
args=("$native" "$double")
[[ $run == old ]] || args+=("$jobs")
cd "$source"
status=0
/usr/bin/time -o "$dest/total.time" -f '%e %U %S %x' just --justfile "$dest/Justfile" strict "${args[@]}" >"$dest/strict.log" 2>"$dest/strict.stderr" || status=$?
if [[ -d build/strict/seam ]]; then
  cp build/strict/seam/{native,double}.jsonl "$dest/" || true
  for f in native-retry double-retry; do
    [[ ! -f build/strict/seam/$f.jsonl ]] || cp "build/strict/seam/$f.jsonl" "$dest/"
  done
fi
exit "$status"
