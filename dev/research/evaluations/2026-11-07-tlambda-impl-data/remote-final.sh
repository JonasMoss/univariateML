#!/usr/bin/env bash
# One mirror, then serial validation against one immutable source snapshot.
set -euo pipefail
bash dev/research/evaluations/2026-11-07-tlambda-impl-data/cpp-release.sh no-long-double
bash dev/research/evaluations/2026-11-07-tlambda-impl-data/r-release.sh
