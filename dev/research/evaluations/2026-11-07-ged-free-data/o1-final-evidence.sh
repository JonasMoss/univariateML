#!/bin/bash
set -euo pipefail
printf 'BEGIN_ORACLES\n'
bash dev/research/evaluations/2026-11-07-ged-free-data/o1-oracles.sh
printf 'BEGIN_TIMINGS\n'
bash dev/research/evaluations/2026-11-07-ged-free-data/o1-timings.sh
printf 'BEGIN_CONTROLS\n'
bash dev/research/evaluations/2026-11-07-ged-free-data/o1-cases.sh
printf 'BEGIN_NORMALIZATION\n'
bash dev/research/evaluations/2026-11-07-ged-free-data/check_normalization.sh
printf 'BEGIN_POSTCONDITIONS\n'
bash dev/research/evaluations/2026-11-07-ged-free-data/check_postconditions.sh
printf 'BEGIN_FIXED_PRECISION\n'
bash dev/research/evaluations/2026-11-07-ged-free-data/check_fixed_scale.sh
printf 'BEGIN_PROTECTED_DENSITY\n'
bash dev/research/evaluations/2026-11-07-ged-free-data/check_density_underflow.sh
