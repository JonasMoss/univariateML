#!/bin/bash
set -eu
boost=$(Rscript -e 'cat(system.file("include",package="BH"))')
c++ -std=c++17 -fsyntax-only -IunivariateML-cpp/include -I"$boost" bindings/js/validation/vcov-native.cpp
