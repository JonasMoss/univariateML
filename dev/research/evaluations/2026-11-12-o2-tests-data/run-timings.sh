#!/usr/bin/env bash
# Run from the repository root through /tmp/umvl-cpu/run4.
# Baseline source is reconstructed into ignored scratch, never stored as data.
set -uo pipefail
origin=$PWD
mkdir -p build/tmp build/scratch/timing-bin
cat > build/scratch/timing-bin/cmake <<'CMAKE'
#!/usr/bin/env bash
label=configure
if [ "${1:-}" = --build ]; then label=build; fi
/usr/bin/time -f "${label}_wall_seconds=%e" /usr/bin/cmake "$@"
CMAKE
chmod +x build/scratch/timing-bin/cmake
export TMPDIR="$origin/build/tmp" JUST_TEMPDIR="$origin/build/tmp" XDG_RUNTIME_DIR="$origin/build/tmp"
export PATH="$origin/build/scratch/timing-bin:$PATH"
phase=${1:?Use before or after}
case "$phase" in before|after) ;; *) exit 2 ;; esac
status=0
if [ "$phase" = before ]; then
  mkdir -p build/scratch/timing-baseline
  git archive 217ab8b533cac25ccc8eba8d66eccde1b376f0b4 \
    Justfile univariateML-cpp dev/validation/diagnostics-audit.cpp | \
    tar -x -C build/scratch/timing-baseline || exit 1
  cd "$origin/build/scratch/timing-baseline"
fi
for entry in dev no-long-double full; do
  args=(just cpp-test)
  if [ "$entry" = no-long-double ]; then args=(just cpp-test-no-long-double); fi
  tier=dev
  if [ "$entry" = full ]; then tier=full; fi
  export CCACHE_LOGFILE="$origin/build/scratch/$phase-$entry-ccache.log"
  UMVL_TEST_TIER=$tier /usr/bin/time -f 'recipe_wall_seconds=%e' "${args[@]}" > "$origin/build/scratch/$phase-$entry.log" 2>&1
  rc=$?
  if [ "$rc" != 0 ]; then status=1; fi
  echo "$phase $entry exit=$rc"
  tail -n 13 "$origin/build/scratch/$phase-$entry.log"
done
if [ "$phase" = before ]; then
  cd "$origin/build/scratch/timing-baseline"
  # Freeze the exploratory O2 replay's assertion-enabled configuration.
  # The final after measurements use the current R-compatible NDEBUG mode.
  python3 - <<'CONFIG'
from pathlib import Path
path = Path('univariateML-cpp/CMakeLists.txt')
path.write_text(path.read_text().replace('set(CMAKE_CXX_EXTENSIONS OFF)',
                                         'set(CMAKE_CXX_EXTENSIONS ON)'))
path = Path('univariateML-cpp/tests/CMakeLists.txt')
text = path.read_text().replace('set(_CPPUNIV_TEST_FLAGS -Wall -Wextra -Wpedantic)',
                               'set(_CPPUNIV_TEST_FLAGS -Wall -Wextra -Wpedantic -g -O2)')
text = text.replace('  target_compile_options(diagnostics_tests PRIVATE -O2)\n', '')
path.write_text(text)
CONFIG
  for recipe in cpp-test cpp-test-no-long-double; do
    export CCACHE_LOGFILE="$origin/build/scratch/optimized-unfixed-$recipe-ccache.log"
    UMVL_TEST_TIER=full /usr/bin/time -f 'recipe_wall_seconds=%e' just "$recipe" > "$origin/build/scratch/optimized-unfixed-$recipe.log" 2>&1
    echo "optimized-unfixed $recipe exit=$?"
    tail -n 16 "$origin/build/scratch/optimized-unfixed-$recipe.log"
  done
fi

exit "$status"
