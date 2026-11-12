#!/usr/bin/env bash
# Reproduce the inherited assertions without modifying or installing anything.
set -eu
cd "$(dirname "$0")/../../../.."
export TMPDIR="$PWD/build/tmp"
mkdir -p "$TMPDIR" build/scratch/oracle-baseline
# Baseline sources are temporary build inputs, never evaluation archives.
git archive 7825e912 dev/validation univariateML-cpp/include/cppunivariate/distributions | tar -x -C build/scratch/oracle-baseline
PYTHONPATH="$PWD/build/scratch/oracle-baseline/dev/validation" /tmp/umvl-cpu/run python3 -m unittest \
 quality.test_truth.SamplerPoints.test_default_is_retained_when_stress_grid_reaches_cap \
 quality.test_truth.SamplerPoints.test_all_tables_select_public_default_and_frozen_golden_fit \
 quality.test_truth.AttainedBoundaries.test_cmb_workload_keeps_both_support_endpoints \
 quality.test_truth_metadata.TruthMetadataTests.test_stored_fingerprints_and_no_timings
