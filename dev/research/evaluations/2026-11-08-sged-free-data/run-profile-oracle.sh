#!/bin/bash
set -euo pipefail
export TMPDIR=$PWD/build/tmp JUST_TEMPDIR=$PWD/build/tmp XDG_RUNTIME_DIR=$PWD/build/tmp
boost_include=$(Rscript -e 'cat(system.file("include",package="BH"))')
p=dev/research/evaluations/2026-11-08-sged-free-data
g++ -std=c++17 -O2 -IunivariateML-cpp/include -I"$boost_include" "$p/profile-oracle.cpp" -o build/scratch/profile-oracle
build/scratch/profile-oracle < "$p/golden.txt" > "$p/profile-boxes.txt"
python3 "$p/profile-oracle.py"
