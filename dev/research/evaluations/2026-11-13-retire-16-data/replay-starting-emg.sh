#!/bin/bash
set -euo pipefail
mkdir -p build/scratch/starting-tree
git archive 1e0f5185 univariateML-cpp/include bindings/common | tar -x -C build/scratch/starting-tree
python3 dev/research/evaluations/2026-11-13-retire-16-data/replay-starting-emg.py
