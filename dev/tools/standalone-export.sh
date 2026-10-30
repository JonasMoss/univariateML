#!/usr/bin/env bash
# Run under /tmp/umvl-cpu/run (or run4 with CMAKE_BUILD_PARALLEL_LEVEL=4).
set -euo pipefail
root=$(cd "$(dirname "$0")/../.." && pwd)
export TMPDIR="$root/build/tmp"
mkdir -p "$TMPDIR" "$root/build/scratch"
boost=${1:-${Boost_INCLUDE_DIR:-${BOOST_ROOT:-}}}
if [[ -z "$boost" && -f "$root/univariateML-cpp/build/CMakeCache.txt" ]]; then
    boost=$(sed -n 's/^Boost_INCLUDE_DIR:[^=]*=//p' "$root/univariateML-cpp/build/CMakeCache.txt")
fi
[[ -n "$boost" ]] || { echo 'Pass a Boost header directory or set Boost_INCLUDE_DIR (or run just cpp-test first).' >&2; exit 1; }
boost=$(cd "$boost" && pwd)
[[ -f "$boost/boost/version.hpp" ]] || { echo 'Set Boost_INCLUDE_DIR to plain Boost headers' >&2; exit 1; }
# The source export must be outside the repository. Keep all build scratch in it.
export_dir=$(mktemp -d "$HOME/.cache/ccache/umvl-standalone.XXXXXX")
trap '/bin/rm -rf "$export_dir"' EXIT
mkdir "$export_dir/source" "$export_dir/bin"
tar -C "$root/univariateML-cpp" --exclude=./build -cf - . | tar -C "$export_dir/source" -xf -
# Resolve every quoted include and reject filesystem links and build inputs
# that escape the artifact. Prose links to monorepo documentation are harmless.
python3 - "$export_dir/source" "$root" <<'PY'
import pathlib,re,sys
root=pathlib.Path(sys.argv[1]).resolve()
for p in root.rglob('*'):
    if p.is_symlink(): raise SystemExit(f'Export contains symlink: {p}')
    if p.suffix not in ('.hpp','.h','.cpp','.txt') and p.name!='CMakeLists.txt': continue
    text=p.read_text()
    if sys.argv[2] in text: raise SystemExit(f'Repository absolute path: {p}')
    for name in re.findall(r'^\s*#include\s*"([^"]+)"',text,re.M):
        # The bundled amalgamated doctest defines SINGLE_HEADER before its
        # dormant split-header include. Compilation checks active includes.
        if p.name=='doctest.h' and name=='doctest_fwd.h' and '#define DOCTEST_SINGLE_HEADER' in text:
            continue
        candidates=[p.parent/name,root/'include'/name,root/'tests'/name]
        if not any(c.is_file() and c.resolve().is_relative_to(root) for c in candidates):
            raise SystemExit(f'Include outside export or missing: {p}: {name}')
    if p.name=='CMakeLists.txt' and re.search(r'(bindings/|dev/|\.\./)',text):
        raise SystemExit(f'External build reference: {p}')
PY
# A whitelist PATH excludes both R entrypoints, including /bin aliases.
for tool in cmake ctest make gmake c++ g++ cc gcc as ld ar ranlib uname ninja; do
    path=$(command -v "$tool" || true)
    if [[ -n "$path" ]]; then ln -s "$path" "$export_dir/bin/$tool"; fi
done
export PATH="$export_dir/bin"
if command -v R || command -v Rscript; then exit 1; fi
cmake -S "$export_dir/source" -B "$export_dir/build" -DBoost_INCLUDE_DIR="$boost"
cmake --build "$export_dir/build" -j "${CMAKE_BUILD_PARALLEL_LEVEL:-1}"
ctest --test-dir "$export_dir/build" --output-on-failure
