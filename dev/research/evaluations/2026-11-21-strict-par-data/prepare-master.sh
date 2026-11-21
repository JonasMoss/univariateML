#!/usr/bin/env bash
set -euo pipefail
baseline=67ff1d731a35f37dad2e01bf8cc6d79d70b0681a
projection=$PWD/build/scratch/master
mkdir -p "$projection"
git archive "$baseline" Justfile univariateML-cpp bindings/common dev/validation \
  dev/research/evaluations/2026-11-07-fit-audit-data/cases.json.gz \
  dev/research/evaluations/2026-11-13-retire-16-data/route-inputs.json \
  dev/research/evaluations/2026-11-13-fit-snapshot-data \
  | tar -x -C "$projection"
for script in cert-replay.py cert_replay_{roots,faces,covers,integer}.py strict_common.py \
              seam-check.py strict-seam.py fit-judge.py fit-density.py fit-consistency.py fit-refine.py; do
  git show "$baseline:dev/validation/$script" > "$projection/dev/validation/$script"
done
