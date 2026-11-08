#!/bin/bash
set -euo pipefail
D=dev/research/evaluations/2026-11-08-sstd-free-data
rsync -a -e 'ssh -F /home/jonas/.ssh/config -o BatchMode=yes' \
 --include='frozen-cpp-dev.txt' --include='frozen-cpp-full.txt' --include='frozen-cpp-no-long-double.txt' --include='frozen-r-*.txt' \
 --include='simbox-status.tsv' --exclude='*' \
 "simbox:umvl-wt/$(basename "$PWD")/$D/" "$D/"
