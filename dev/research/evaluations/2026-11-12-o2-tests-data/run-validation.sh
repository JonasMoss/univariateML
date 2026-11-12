#!/usr/bin/env bash
# Run from the repository root after run-timings.sh after, without an outer
# slot wrapper. This driver reserves slots for each local computation.
set -u
unset UMVL_TEST_FAMILIES FIT_FAMILY JS_FAMILIES
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
status=0
for tier in dev full; do
  recipe=test
  if [ "$tier" = full ]; then recipe=test-full; fi
  machine=simbox
  /tmp/umvl-cpu/simbox-run env UMVL_TEST_TIER="$tier" CMAKE_BUILD_PARALLEL_LEVEL=4 CTEST_PARALLEL_LEVEL=4 /usr/bin/time -f 'recipe_wall_seconds=%e' just "$recipe" > "build/scratch/final-r-$tier.log" 2>&1
  rc=$?
  if [ "$rc" = 75 ]; then
    machine=workstation
    /tmp/umvl-cpu/run4 env UMVL_TEST_TIER="$tier" /usr/bin/time -f 'recipe_wall_seconds=%e' just "$recipe" >> "build/scratch/final-r-$tier.log" 2>&1
    rc=$?
  fi
  echo "R $tier machine=$machine exit=$rc"
  tail -n 15 "build/scratch/final-r-$tier.log"
  if [ "$rc" != 0 ]; then status=1; fi
done
/tmp/umvl-cpu/simbox-run env CMAKE_BUILD_PARALLEL_LEVEL=4 CTEST_PARALLEL_LEVEL=4 just cpp-test-no-long-double > build/scratch/final-simbox-no-long-double.log 2>&1
rc=$?
echo "simbox no-long-double exit=$rc (75 means workstation fallback already covered by timing runs)"
if [ "$rc" != 0 ] && [ "$rc" != 75 ]; then status=1; fi
for recipe in js-build js-test; do
  /tmp/umvl-cpu/run4 env UMVL_TEST_TIER=full /usr/bin/time -f 'recipe_wall_seconds=%e' just "$recipe" > "build/scratch/final-$recipe.log" 2>&1
  rc=$?
  echo "$recipe machine=workstation exit=$rc"
  tail -n 15 "build/scratch/final-$recipe.log"
  if [ "$rc" != 0 ]; then status=1; fi
done
exit "$status"
