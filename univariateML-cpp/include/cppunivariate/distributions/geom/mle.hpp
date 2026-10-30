// Geometric MLE: closed-form prob = 1 / (mean(x) + 1).
//
// Depends on the Rmath-backed special::dgeom for the log-likelihood
// summation (matches existing Rmath behaviour bit-for-bit).

#pragma once

#include "../../core/span.hpp"
#include "../../special/special.hpp"

#include <cmath>
#include <cstddef>
#include <limits>

namespace cppunivariate {

struct GeomResult {
  double prob;
  double logLik;
};

inline GeomResult geom_mle(ispan x, double fixed=std::numeric_limits<double>::quiet_NaN()) {
  constexpr double kNaN = std::numeric_limits<double>::quiet_NaN();
  const std::size_t n = x.size();
  if (n == 0) return {kNaN, kNaN};

  if(!std::isnan(fixed)) {
    if(!(fixed>0 && fixed<=1 && std::isfinite(fixed)))return {kNaN,kNaN};
    double ll=0;for(int v:x) {if(v<0)return {kNaN,kNaN};ll+=special::dgeom(v,fixed,true);}
    return {fixed,ll};
  }
  double sum = 0.0;
  for (auto v : x) sum += static_cast<double>(v);
  const double mean = sum / static_cast<double>(n);
  const double prob = 1.0 / (mean + 1.0);

  double logLik = 0.0;
  for (auto v : x) {
    logLik += ::cppunivariate::special::dgeom(static_cast<double>(v), prob, 1);
  }
  return {prob, logLik};
}

}  // namespace cppunivariate
