#!/bin/sh
# Workstation preflight; run through the one-slot reservation.
set -eu
mkdir -p build/tmp build/scratch/bnbinom-alpha
sed 's/-std=c++17/-DBOOST_MATH_NO_LONG_DOUBLE_MATH_FUNCTIONS -std=c++17/g' \
  dev/research/evaluations/2026-11-07-bnbinom-alpha-data/targeted.sh \
  > build/scratch/bnbinom-alpha/targeted-double-run.sh
sh build/scratch/bnbinom-alpha/targeted-double-run.sh
