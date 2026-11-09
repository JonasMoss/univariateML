#!/bin/sh
set -eu
evidence=dev/research/evaluations/2026-11-09-bnbinom-face-data
cp build/scratch/bnbinom-face/coverage/coverage.csv "$evidence/coverage.csv"
cp build/scratch/bnbinom-face/coverage/samples.jsonl "$evidence/coverage-samples.jsonl"
python3 "$evidence/coverage-summary.py"
python3 "$evidence/comparison.py"
