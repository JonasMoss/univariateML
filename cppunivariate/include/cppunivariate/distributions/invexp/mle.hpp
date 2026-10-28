// Actuar rate is the reciprocal of the exponential rate on 1/x.
#pragma once
#include "../exp/mle.hpp"
#include <cmath>
#include <limits>
#include <vector>
namespace cppunivariate {
struct InvExpResult { double rate; double logLik; int iterations;  };
inline InvExpResult invexp_mle(span x) {
  for (double v : x) {
    if (!(v > 0) || !std::isfinite(v) || !std::isfinite(1/v)) {
      const double nan=std::numeric_limits<double>::quiet_NaN();
      return {nan,nan,0};
    }
  }
  std::vector<double> y(x.size());
  double jacobian = 0;
  for (std::size_t i=0; i<x.size(); ++i) { y[i]=1.0/x[i]; jacobian -= 2*std::log(x[i]); }
  auto r = exp_mle(span{y.data(), y.size()});
  return {1.0/r.rate, r.logLik+jacobian, 0};
}
} // namespace cppunivariate
