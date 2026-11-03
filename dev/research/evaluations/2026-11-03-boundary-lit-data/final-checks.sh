#!/usr/bin/env bash
# Run from repo root, under one shared CPU slot. No background commands.
set -euo pipefail
export OPENBLAS_NUM_THREADS=1 OMP_NUM_THREADS=1 MKL_NUM_THREADS=1
python3 dev/research/evaluations/2026-11-03-boundary-lit-data/check.py > build/scratch/boundary-check.log
python3 dev/research/evaluations/2026-11-03-boundary-lit-data/validate.py
bash dev/research/evaluations/2026-11-03-boundary-lit-data/build-note.sh
pdfinfo dev/research/boundary-points.pdf
pdftoppm -f 1 -singlefile -scale-to 1200 -png dev/research/boundary-points.pdf build/scratch/boundary-page1
pdftoppm -f 8 -singlefile -scale-to 1200 -png dev/research/boundary-points.pdf build/scratch/boundary-page8
printf '\nEnvironment:\n'
uname -srmo
python3 --version
R --version | head -1
c++ --version | head -1
pdflatex --version | head -1
Rscript -e 'cat("BH ", as.character(packageVersion("BH")), "\n", sep="")'
