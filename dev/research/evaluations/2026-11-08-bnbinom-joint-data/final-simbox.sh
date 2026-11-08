#!/bin/sh
set -eu
python3 dev/research/evaluations/2026-11-08-bnbinom-joint-data/fingerprints.py
sh dev/research/evaluations/2026-11-08-bnbinom-joint-data/run-suites.sh
python3 dev/research/evaluations/2026-11-08-bnbinom-joint-data/fingerprints.py
