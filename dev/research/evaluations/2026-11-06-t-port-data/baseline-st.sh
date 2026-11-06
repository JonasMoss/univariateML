#!/bin/sh
# Run under a CPU allocation or the supplied simbox launcher.
# Uses a scratch package and frozen master headers; never modifies the checkout.
set -eu
pkg=build/scratch/t-port-baseline-r
mkdir -p "$pkg"
cp -a univariateML-r/. "$pkg/"
ref=dev/research/evaluations/2026-11-06-t-port-data/baseline-master
cp "$ref/bindings/common/fit.hpp" "$pkg/src/common/fit.hpp"
cp "$ref/bindings/common/distributions/lst/fit.hpp" "$pkg/src/common/distributions/lst/fit.hpp"
cp "$ref/bindings/common/distributions/std/fit.hpp" "$pkg/src/common/distributions/std/fit.hpp"
for path in core/stop_detail.hpp distributions/lst/mle.hpp distributions/std/mle.hpp; do
  cp "$ref/univariateML-cpp/include/cppunivariate/$path" "$pkg/inst/include/cppunivariate/$path"
done
find "$pkg/src" -name '*.o' -delete
find "$pkg/src" -name '*.so' -delete
export PKG_BUILD_EXTRA_FLAGS=false
Rscript dev/research/evaluations/2026-11-06-t-port-data/baseline-st.R
