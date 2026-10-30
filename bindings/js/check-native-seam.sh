#!/usr/bin/env bash
# Run under a CPU slot. Compare every extended primitive's native code
# against HEAD, without modifying a tracked header or Git state.
set -euo pipefail
cd "$(dirname "$0")/../.."
mkdir -p build/scratch/baseline-include
cp -a univariateML-cpp/include/cppunivariate build/scratch/baseline-include/
git show HEAD:univariateML-cpp/include/cppunivariate/special/special.hpp > build/scratch/baseline-include/cppunivariate/special/special.hpp
cat > build/scratch/seam-probe.cpp <<'CPP'
#include "cppunivariate/special/special.hpp"
long double n(double x,bool b){return cppunivariate::special::normal_cdf_extended(x,b);}
long double g(double x,double a,bool b){return cppunivariate::special::gamma_cdf_extended(x,a,b);}
double np(double x,bool b,bool l){return cppunivariate::special::fs_normal_p(x,b,l);}
double nq(double x,bool b){return cppunivariate::special::fs_normal_q(x,b);}
double tp(double x,double a,bool b,bool l){return cppunivariate::special::fs_t_p(x,a,b,l);}
double tq(double x,double a,bool b){return cppunivariate::special::fs_t_q(x,a,b);}
double gp(double x,double a,bool b,bool l){return cppunivariate::special::fs_gamma_p(x,a,b,l);}
double gq(double x,double a,bool b){return cppunivariate::special::fs_gamma_q(x,a,b);}
CPP
boost=$(Rscript -e 'cat(system.file("include", package="BH"))')
for opt in O0 O2; do
 c++ -std=c++17 -"$opt" -Ibuild/scratch/baseline-include -I"$boost" -c build/scratch/seam-probe.cpp -o build/scratch/seam-before.o
 c++ -std=c++17 -"$opt" -IunivariateML-cpp/include -I"$boost" -c build/scratch/seam-probe.cpp -o build/scratch/seam-after.o
 cmp build/scratch/seam-before.o build/scratch/seam-after.o
 echo "$opt: identical native object bytes for all eight extended primitives"
done
