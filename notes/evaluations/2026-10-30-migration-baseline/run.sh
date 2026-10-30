#!/usr/bin/env bash
# From the worktree root. Default: simbox. --local uses the workstation lock.
# --only {cpp-test,test,check,quality}; --collect-only trims existing scratch logs.
# No dependency installs, fixture regeneration, source edits, or Git writes.
set -uo pipefail
out=notes/evaluations/2026-10-30-migration-baseline
expected=5f11fb9e878219ac47a9a372e985eb69da6198e3
local_run=0; worker=0; collect=0; only=
while (($#)); do
  case "$1" in
    --local) local_run=1;;
    --worker) worker=1;;
    --collect-only) collect=1;;
    --only) shift; only=${1:?--only needs a recipe};;
    *) echo "Unknown argument: $1" >&2; exit 2;;
  esac
  shift
done
case "$only" in ""|cpp-test|test|check|quality) ;; *) echo "Unknown recipe: $only" >&2; exit 2;; esac
export TMPDIR="$PWD/build/tmp"
mkdir -p "$TMPDIR" build/scratch "$out" || exit 2
if ((!worker)); then
  revision=$(git rev-parse HEAD) || exit 2
  [[ "$revision" == "$expected" ]] || { echo "Expected $expected; found $revision" >&2; exit 2; }
  git diff --quiet "$expected" -- cppunivariate quality univariateML Justfile CLAUDE.md roadmap ||
    { echo "Baseline source differs from the recorded revision." >&2; exit 2; }
  printf '%s\n' "$revision" > "$out/source-revision.log"
  args=(--worker)
  [[ -n "$only" ]] && args+=(--only "$only")
  ((collect)) && args+=(--collect-only)
  if ((local_run)); then
    /tmp/umvl-cpu/run bash "$out/run.sh" "${args[@]}"
  else
    /tmp/umvl-codex/simbox-run.sh bash "$out/run.sh" "${args[@]}"
  fi
  exit $?
fi
export NOT_CRAN=true OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1 MKL_NUM_THREADS=1
export CMAKE_BUILD_PARALLEL_LEVEL=1 MAKEFLAGS=-j1
tag=${only:+-$only}
envlog="$out/environment$tag.log"
exitlog="$out/exits$tag.log"
if ((!collect)); then
  : > "$exitlog"
  {
    date -u --iso-8601=seconds
    uname -a
    cat /etc/os-release
    lscpu
    R --version
    R CMD config CXX
    R CMD config CXX17
    Rscript -e 'for(p in c("Rcpp","BH","testthat")) cat(p,as.character(packageVersion(p)),"\n"); cat("testthat reporter:",testthat::default_reporter(),"\n"); sessionInfo()'
    python3 -c 'import sys,sympy,mpmath,scipy; print(sys.version); print("sympy",sympy.__version__,"mpmath",mpmath.__version__,"scipy",scipy.__version__)'
    if command -v ccache; then ccache --show-config; fi
    if [[ -f ~/.R/Makevars ]]; then cat ~/.R/Makevars; fi
  } > "$envlog" 2>&1
fi
checks=(cpp-test test check quality)
[[ -n "$only" ]] && checks=("$only")
result=0
for check in "${checks[@]}"; do
  if ((!collect)); then
    echo "Starting just $check $(date -u --iso-8601=seconds)"
    # Both transport locks already apply nice -n 10.
    just "$check" > "build/scratch/baseline-$check.full.log" 2>&1
    rc=$?
    echo "just $check exit=$rc" | tee -a "$exitlog"
    ((rc)) && result=1
  fi
  python3 - "$check" "$out" <<'PY'
import pathlib,re,sys
check,out=sys.argv[1:]
out=pathlib.Path(out)
s=pathlib.Path(f"build/scratch/baseline-{check}.full.log").read_text()
lines=re.sub(r'\x1b\[[0-9;]*[A-Za-z]', '', s).splitlines()
compile_cmd=r'^(g\+\+|gcc|ccache|/usr/.*g\+\+|clang\+\+).* (?:-c|-shared)(?: |$)'
def trimmed(xs):
    return [x for x in xs if not re.search(r'\[\s*\d+%\]|Building CXX|Linking CXX',x)
            and not re.match(compile_cmd,x) and not x.startswith("Writing '")]
if check in ("test","check"):
    token="testthat::test_local" if check=="test" else "&& R CMD build"
    start=next((i for i,x in enumerate(lines) if token in x),0)
    generation=lines[:start]
    # Keep every diagnostic verbatim, with context/continuations, not export/build listings.
    keep=set()
    for i,line in enumerate(generation):
        if "warning:" in line:
            keep.update(range(max(0,i-3),min(len(generation),i+7)))
        elif line.startswith(("✖","Warning","WARNING")):
            j=i+1
            while j<len(generation) and generation[j].startswith(("  ","ℹ")):
                j+=1
            keep.update(range(i,j))
    if keep:
        diagnostics=trimmed([generation[i] for i in sorted(keep)])
        (out/f"{check}-generation.log").write_text("\n".join(diagnostics)+"\n")
    lines=lines[start:]
(out/f"{check}.log").write_text("\n".join(trimmed(lines))+"\n")
PY
  rc=$?
  ((rc)) && result=1
  tail -12 "$out/$check.log"
  if [[ "$check" == cpp-test && -f cppunivariate/build/Testing/Temporary/LastTest.log ]]; then
    cp cppunivariate/build/Testing/Temporary/LastTest.log "$out/cpp-targets.log" || result=1
  fi
  if [[ "$check" == check && -f univariateML/package/univariateML.Rcheck/00check.log ]]; then
    cp univariateML/package/univariateML.Rcheck/00check.log "$out/R-CMD-check.log" || result=1
    if [[ -f univariateML/package/univariateML.Rcheck/tests/testthat.Rout ]]; then
      sed -n '/> test_check/,$p' univariateML/package/univariateML.Rcheck/tests/testthat.Rout > "$out/check-tests.log"
    fi
  fi
  if [[ "$check" == quality && -f build/quality/report.md ]]; then
    # Limits have their own complete source inventory; retain the per-family table.
    sed '/^Limits are listed/,$d' build/quality/report.md > "$out/quality-families.log"
  fi
done
if ((!collect)); then
  {
    if [[ -f cppunivariate/build/CMakeCache.txt ]]; then
      grep -E 'CMAKE_CXX_COMPILER:|CMAKE_CXX_COMPILER_LAUNCHER:' cppunivariate/build/CMakeCache.txt
      grep -E 'CMAKE_CXX_COMPILER_(ID|VERSION)' cppunivariate/build/CMakeFiles/*/CMakeCXXCompiler.cmake
    fi
    R CMD config CXX17 | xargs -r sh -c '"$@" --version' sh
    date -u --iso-8601=seconds
  } >> "$envlog" 2>&1
fi
python3 "$out/inventory.py" || result=1
exit "$result"
