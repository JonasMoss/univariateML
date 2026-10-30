#!/usr/bin/env bash
# Run after `just regen`, under the workstation CPU slot lock.
# Package installation by R CMD check is private to this fresh check directory.
set -euo pipefail
root=$(cd "$(dirname "$0")/../.." && pwd)
export CCACHE_DIR="${UMVL_CCACHE_DIR:-$HOME/.cache/ccache/univariateML}"
export CCACHE_BASEDIR="$root" CCACHE_NOHASHDIR=1
export LC_ALL=C.UTF-8 TZ=UTC
export TMPDIR="$root/build/tmp"
mkdir -p "$TMPDIR" "$root/build/scratch"
cd "$root/univariateML-r"
R CMD build .
version=$(sed -n 's/^Version: //p' DESCRIPTION)
tarball="univariateML_${version}.tar.gz"
test "$(stat -c %s "$tarball")" -lt 5000000
# /var/tmp avoids the workstation's per-user /tmp quota. Override if necessary.
external=$(mktemp -d "${UMVL_CHECK_PARENT:-/var/tmp}/umvl-r-check.XXXXXX")
cp "$tarball" "$external/"
printf '%s\n' "$external" > "$root/build/scratch/standalone-directory.txt"
mkdir -p "$external/tmp" "$external/source"
tar -xzf "$external/$tarball" -C "$external/source"
# No absolute paths may be baked into the package. Repository-relative paths
# are allowed in comments and provenance; that the package does not need the
# checkout is established by the Landlock check below, not by this scan.
pattern='(^|[[:space:]"(=])/[[:alnum:]_.-]+/|(^|[[:space:]"(=])[A-Za-z]:\\'
set +e
grep -RInE --exclude='*.o' --exclude='*.so' --exclude='*.tar.gz' "$pattern" \
  "$root/univariateML-r" "$external/source" > "$root/build/scratch/standalone-paths.log"
scan_status=$?
set -e
if [ "$scan_status" -ne 1 ]; then
  cat "$root/build/scratch/standalone-paths.log"
  exit 1
fi
# Linux Landlock denies reading the checkout (and all other worktrees). Allow
# system tools, user R libraries/configuration/cache, and this check directory.
# Fail rather than silently weaken isolation when Landlock is unavailable.
access=read-file,read-dir
isolate=(setpriv --no-new-privs --landlock-access "fs:$access")
for path in /usr /bin /sbin /lib /lib64 /etc /opt /var /run /dev /proc /sys \
            "$HOME/R" "$HOME/.R" "$HOME/.cache/ccache" "$external"; do
  if [ -e "$path" ]; then
    isolate+=(--landlock-rule "path-beneath:$access:$path")
  fi
done
cd "$external"
unset UMVL_DISTRIBUTIONS_DIR UMVL_DOSSIER_DIR UMVL_BINDINGS_DIR UMVL_PKG_DIR UMVL_PKG_R_DIR
unset R_LIBS R_TESTS NOT_CRAN JUST_TEMPDIR
export TMPDIR="$external/tmp" CCACHE_BASEDIR="$external" UMVL_TEST_TIMINGS=true
# Confirm isolation before checking; retain the check tree for review.
"${isolate[@]}" sh -c 'if cat "$1/CLAUDE.md" >/dev/null 2>&1; then exit 1; fi' sh "$root"
set +e
/usr/bin/time -p -o "$external/check-time.txt" \
  "${isolate[@]}" R CMD check --no-manual --timings "$tarball" \
  > "$external/check-output.log" 2>&1
status=$?
set -e
cp "$external/check-time.txt" "$external/check-output.log" "$root/build/scratch/"
cp "$external/univariateML.Rcheck/00check.log" "$root/build/scratch/standalone-00check.log"
cat "$external/check-output.log"
cat "$external/check-time.txt"
printf 'Retained independent check: %s\n' "$external"
exit "$status"
