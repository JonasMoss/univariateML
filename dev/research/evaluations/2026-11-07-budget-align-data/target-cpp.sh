#!/bin/bash
set -eu
cmake --build univariateML-cpp/build
ctest --test-dir univariateML-cpp/build --output-on-failure -j1 -R '^distribution_gb2$'
