#!/usr/bin/env bash
# Native note-build's two pdflatex passes, without its background viewer.
# Invoke from the repository root, under /tmp/umvl-cpu/run.
set -euo pipefail
repo="$PWD"
cd dev/research
pdflatex -interaction=nonstopmode -halt-on-error boundary-points.tex > "$repo/build/scratch/boundary-latex-pass1.log"
pdflatex -interaction=nonstopmode -halt-on-error boundary-points.tex > "$repo/build/scratch/boundary-latex-pass2.log"
cp boundary-points.log "$repo/build/scratch/boundary-latex.log"
if rg -i 'undefined|LaTeX Error|Rerun to get|multiply defined|Overfull' "$repo/build/scratch/boundary-latex-pass2.log"; then
  echo 'FAIL: unresolved reference, LaTeX error, or overfull box' >&2
  exit 1
fi
pdftotext boundary-points.pdf "$repo/build/scratch/boundary-points.txt"
if rg -F '??' "$repo/build/scratch/boundary-points.txt"; then
  echo 'FAIL: unresolved marker in extracted PDF text' >&2
  exit 1
fi
rm -f boundary-points.aux boundary-points.log boundary-points.out
printf 'PASS: two pdflatex passes; no undefined references, rerun requests, or overfull boxes.\n'
