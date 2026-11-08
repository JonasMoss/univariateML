#!/bin/bash
set -eu
bash dev/research/evaluations/2026-11-07-ged-free-data/o1-checks.sh
bash dev/research/evaluations/2026-11-07-ged-free-data/o1-probe.sh
python3 dev/research/evaluations/2026-11-07-ged-free-data/check_roots.py build/scratch/o1-probe
