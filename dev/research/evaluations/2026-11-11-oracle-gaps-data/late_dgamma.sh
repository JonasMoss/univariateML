#!/usr/bin/env bash
# One run4 reservation; targeted numerical checks after the local DGamma fix.
set -u
cd "$(dirname "$0")/../../../.."
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
export UMVL_TEST_FAMILIES=dgamma
logdir="$PWD/build/scratch/oracle-final"
failed=0
cmake --build univariateML-cpp/build --target distribution_tests -j4 >"$logdir/dgamma-cpp-native.log" 2>&1 && ctest --test-dir univariateML-cpp/build -R '^distribution_dgamma$' --output-on-failure >>"$logdir/dgamma-cpp-native.log" 2>&1
rc=$?; echo "dgamma-cpp-native workstation $rc" | tee -a "$logdir/receipts.txt"; if [ "$rc" != 0 ]; then failed=1; fi
cmake --build build/cpp-no-long-double --target distribution_tests -j4 >"$logdir/dgamma-cpp-double.log" 2>&1 && ctest --test-dir build/cpp-no-long-double -R '^distribution_dgamma$' --output-on-failure >>"$logdir/dgamma-cpp-double.log" 2>&1
rc=$?; echo "dgamma-cpp-double workstation $rc" | tee -a "$logdir/receipts.txt"; if [ "$rc" != 0 ]; then failed=1; fi
UMVL_TEST_TIER=full just test >"$logdir/dgamma-r-full.log" 2>&1
rc=$?; echo "dgamma-r-full workstation $rc" | tee -a "$logdir/receipts.txt"; if [ "$rc" != 0 ]; then failed=1; fi
just site-check >"$logdir/dgamma-site.log" 2>&1
rc=$?; echo "dgamma-site workstation $rc" | tee -a "$logdir/receipts.txt"; if [ "$rc" != 0 ]; then failed=1; fi
node --v8-pool-size=1 --test --test-concurrency=1 --test-name-pattern='^dgamma ' bindings/js/test.mjs >"$logdir/dgamma-js.log" 2>&1
rc=$?; echo "dgamma-js workstation $rc" | tee -a "$logdir/receipts.txt"; if [ "$rc" != 0 ]; then failed=1; fi
exit "$failed"
